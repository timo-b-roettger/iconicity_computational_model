# Temp script for grid search on server
library(tidyverse)  # data wrangling and plotting
library(magrittr)   # for all pipes
library(ggplot2)    # for plotting
library(patchwork)  # combining plots
library(broom)      # for regression analysis
library(ggdist)     # for plotting
library(ggside) # for plotting densities on y-axis
library(furrr)  # for parallelizing loop in grid search

# HELPER FUNCTIONS CALLED IN SIMULATION LOOP
clamp01 <- function(x) pmax(0, pmin(1, x))
clamp02 <- function(x) pmax(0, pmin(0.95, x))

normalize_01 <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

# At signal production, amount of noise is dependent on guess rate AND distance to attractor centres
# k controls how strongly sd react to speaker_guess; mindful that if p = 1 and k >2, it will return negative sd's
produce_signal <- function(stored_signal, speaker_guess, drift_sd, k_production, 
                           attractor_centers, circle_radius, center_sd, k_attractor_production) {
  # Calculate baseline trial-specific SD based on speaker's guess
  baseline_sd <- drift_sd * (1 + k_production * (0.5 - speaker_guess))
  # Dynamically calculate Euclidean distance to every defined attractor center
  distances <- sapply(attractor_centers, function(center) sqrt(sum((stored_signal - center)^2)))
  dist_to_nearest <- min(distances)
  nearest_id <- which.min(distances)
  # Find if the signal is inside any attractor, and locate the closest one
  inside_attractor <- distances < circle_radius
  is_inside <- any(inside_attractor)
  # noise f(dist) is calibrated against the worst-case (most inflated) baseline_sd (for when speaker_guess = 0), so the attractor pull is guaranteed strong enough;
  # keeping this noise a pure function of distance -- independent of guess on any given trial.
  max_possible_baseline_sd <- drift_sd * (1 + k_production * 0.5)
  f_dist_max <- max_possible_baseline_sd - center_sd
  attractor_reduction <- 0
  
  if (is_inside) {
    # Target the closest attractor that the signal is currently inside
    closest_attractor_dist <- min(distances[inside_attractor])
    rel_dist <- closest_attractor_dist / circle_radius
    # scale distance dynamically; as with signal evidence
    magnitude <- exp(-k_attractor_production * rel_dist)
    max_mag <- exp(-k_attractor_production * 0)
    min_mag <- exp(-k_attractor_production * 1)
    exp_scale <- (magnitude - min_mag) / (max_mag - min_mag)
    # subtract for fixed base sd, keeping this independent from the noise dep on associative strength
    attractor_reduction <- f_dist_max * exp_scale
  } 
  # floor at center_sd so that noise never drops below the attractor's minimum, regardless of speaker_guess. 
  # Guess-dependence remains fully intact everywhere except at the exact center where center_sd is by definition
  # the tightest the noise can ever get.
  #final_sd <- max(baseline_sd - attractor_reduction, center_sd, 0.001)
  final_sd <- max(baseline_sd - attractor_reduction, 0.001)
  
  # Generate the signal using the final calculated SD
  signal <- rnorm(
    length(stored_signal),
    mean = stored_signal,
    sd = final_sd)
  
  # Return both the produced signal and the boolean status flag
  return(list(signal = clamp01(signal), 
              inside_attractor = is_inside,
              dist_to_nearest = dist_to_nearest,
              attractor_id = if (is_inside) nearest_id else NA_integer_))
}

# Signal evidence for iconicity bias
# Measures proximity of Y to its size prototype
signal_evidence <- function(produced_signal, center_target, k_perception, circle_radius) {
  # Calculate eucledian distance to target_center and to the opposite attractor
  dist_to_target <- sqrt(sum((produced_signal - center_target)^2))
  opposite_center <- c(1 - center_target[1], 1 - center_target[2])
  dist_to_opposite <- sqrt(sum((produced_signal - opposite_center)^2))
  # If signal is inside the circle, calculate evidence
  if (dist_to_target < circle_radius) {
    # Calculate distance to targer center from edge of attractor, independent of circle size; 0 at center, 1 at edge
    rel_dist <- dist_to_target / circle_radius 
    # as relative dist grows, magnitude shrinks as a function of k_perception
    magnitude <- exp(-k_perception * rel_dist) 
    # Simple normalization: center = 1, edge = 0
    max_mag <- exp(-k_perception * 0)
    min_mag <- exp(-k_perception * 1) 
    evidence <- (magnitude - min_mag) / (max_mag - min_mag)
    return(evidence)
    # Punish anti-iconic behavior: if signal instead is inside the opposite attractor, return negative evidence of the same magnitude
  } else if (dist_to_opposite < circle_radius) {
    rel_dist <- dist_to_opposite / circle_radius
    magnitude <- exp(-k_perception * rel_dist)
    max_mag <- exp(-k_perception * 0)
    min_mag <- exp(-k_perception * 1)
    
    evidence <- (magnitude - min_mag) / (max_mag - min_mag)
    return(-evidence)
    
  } else {
    return(0)
  }
}

# LISTENER RECOGNITION PROBABILITY UPDATED
listener_guess_probability <- function(listener_guess, produced_signal, size_prototypes,
                                       recognition_bias, iconicity_weight, k_perception, circle_radius) {
  icon_ev <- signal_evidence(produced_signal, size_prototypes, k_perception = k_perception, circle_radius = circle_radius)
  effective_weight <- if (recognition_bias) iconicity_weight else 0
  
  # qlogis() is undefined at exactly 0 or 1 (-Inf/Inf), we clamp listener_guess minimally from those exact bounds before converting to 
  # logits (listener_guess should already be within (0, 0.95) after update_logit() which applies 0.95 lapse-rate clamp, so this is 
  # purely a numerical safety net, not where the lapse rate is enforced).
  logit_guess <- qlogis(pmax(1e-9, pmin(1 - 1e-9, listener_guess)))
  logits <- logit_guess + (effective_weight * icon_ev)
  probs <- clamp02(plogis(logits))
  
  return(list(probs = probs, evidence = icon_ev))
}

# Update learning as dependent on success
update_logit <- function(x, learning_strength, success, success_scale, failure_scale) {
  delta <- learning_strength * ifelse(success == 1, success_scale, failure_scale)
  plogis(qlogis(clamp02(x)) + delta)
}


# MAIN SIM FUNCTION----
run_interaction_sim <- function(
    data,
    n_sim = 1,
    n_referents = 4,
    n_generations = 1,
    n_rounds = 50,
    # motor/production noise; equivalent to approx. 48% chance of wandering into any attractor in a single production step
    # when the signal is at .5, .5 and speaker_guess = 0.5
    drift_sd = 0.19,
    k_attractor_production = 2.5,
    neutral_attractor_centers = list(c(0.15, 0.15), c(0.85, 0.85)),
    # set as a plausible basin size relative to the unit signal space (not independently calibrated ag a spec target)
    circle_radius = 0.3,
    # sd at attractor centers; a signal at the exact centre has ~5% single-step escape probability from the attractor
    # (ratio of 0.4 of circle_radius)
    trap_center_sd = 0.12,
    k_perception = 2.5,
    recognition_bias = FALSE,
    # multiplicator for iconicity; corresponding to ~10% absolute increase for a listener_guess of 0.5 
    # (for the perfectly iconic signal; icon_ev = 1)
    iconicity_weight = 3,
    # amount of added memory strengthening per exposure, dependent on trial success/failure
    learning_strength = 0.015,
    # substantially faster learning for success than failure; from p=0.3, 10 pure-success updates reach ~0.57,
    # 10 pure-failure reach ~0.33; no real referent is reinforced purely by success/failure since updates depend on
    # current associative strength
    success_scale = 7.5,
    failure_scale = 1,
    expressive_agents = TRUE,
    # expressive productions land inside the target attractor w very high probability
    expressive_noise_sd = circle_radius / 4.8,
    # --- two-level hierarchical expressiveness (kept for generational-overturn work) ---
    # expressive_prob_per_agent = 0.10, # per-generation probability an agent is expressive
    # expressive_trial_prob = 0.20,     # per-trial probability of override, conditional on being that type
    # --- simplified single-parameter version ---
    # flat per-trial probability of an expressive override, no agent-level persistence
    expressive_probability = 0.1
) {
  
  referents_blueprint <- tibble(
    id = seq_len(n_referents),
    type = rep(c("small", "large"), length.out = n_referents),
    size_prototypes = if_else(type == "small",
                              list(c(0.15, 0.85)),
                              list(c(0.85, 0.15))))
  
  semantic_attractor_centers <- unique(referents_blueprint$size_prototypes)
  attractor_centers <- c(semantic_attractor_centers, neutral_attractor_centers)
  is_semantic_attractor <- c(
    rep(TRUE, length(semantic_attractor_centers)),
    rep(FALSE, length(neutral_attractor_centers))
  )
  
  # --- OLD: probability of expressive speakers (agent-level trait) ---
  # expressive_prob <- if (expressive_agents) expressive_prob_per_agent else 0
  # --- NEW: flat per-trial probability, no agent-level draw needed ---
  trial_expressive_prob <- if (expressive_agents) expressive_probability else 0
  
  simulation_log <- list()
  
  for (sim in 1:n_sim) {
    
    referents_info <- referents_blueprint %>%
      mutate(
        agentA_stored_signal = rep(list(c(0.5, 0.5)), n_referents),
        agentB_stored_signal = rep(list(c(0.5, 0.5)), n_referents))
    
    for (gen in 1:n_generations) {
      trial_counter <- 0
      
      # --- agent-level expressive assignment, drawn once per generation ---
      # expressive_A <- runif(1) < expressive_prob
      # expressive_B <- runif(1) < expressive_prob
      
      agentA_guess <- rbeta(n_referents, 3, 9)
      agentB_guess <- rbeta(n_referents, 3, 9)
      
      for (round in 1:n_rounds) {
        referent_order <- sample(1:n_referents)
        roles <- sample(rep(c("A", "B"), length.out = n_referents))
        
        for (trial in 1:n_referents) {
          trial_counter <- trial_counter + 1
          ref_id <- referent_order[trial]
          speaker <- roles[trial]
          listener <- ifelse(speaker == "A", "B", "A")
          
          if (speaker == "A") {
            speaker_guess <- agentA_guess
            listener_guess <- agentB_guess
          } else {
            speaker_guess <- agentB_guess
            listener_guess <- agentA_guess
          }
          
          old_guess_A <- agentA_guess[ref_id]
          old_guess_B <- agentB_guess[ref_id]
          old_stored_signal_A <- referents_info$agentA_stored_signal[[ref_id]]
          old_stored_signal_B <- referents_info$agentB_stored_signal[[ref_id]]
          old_stored_signal <- if (speaker == "A") old_stored_signal_A else old_stored_signal_B
          
          production_output <- produce_signal(
            stored_signal = old_stored_signal,
            speaker_guess = speaker_guess[ref_id],
            drift_sd = drift_sd,
            k_production = 1.5,
            attractor_centers = attractor_centers,
            circle_radius = circle_radius,
            center_sd = trap_center_sd,
            k_attractor_production = k_attractor_production)
          
          is_expressive_trial <- FALSE
          
          # --- override only if speaker is the pre-assigned expressive agent for this generation ---
          # if (
          #   ((speaker == "A" && expressive_A) ||
          #    (speaker == "B" && expressive_B)) &&
          #   runif(1) < expressive_trial_prob
          # ) {
          
          # --- flat per-trial draw, no agent identity involved ---
          if (runif(1) < trial_expressive_prob) {
            is_expressive_trial <- TRUE
            target_center <- referents_info$size_prototypes[[ref_id]]
            signal <- clamp01(rnorm(2, mean = target_center, sd = expressive_noise_sd))
            
            distances <- sapply(attractor_centers, function(center) sqrt(sum((signal - center)^2)))
            dist_to_nearest <- min(distances)
            nearest_id <- which.min(distances)
            in_attractor <- dist_to_nearest < circle_radius
            attractor_id <- if (in_attractor) nearest_id else NA_integer_
          } else {
            signal <- production_output$signal
            in_attractor <- production_output$inside_attractor
            dist_to_nearest <- production_output$dist_to_nearest
            attractor_id    <- production_output$attractor_id
          }
          
          recognition <- listener_guess_probability(
            listener_guess[ref_id],
            signal,
            referents_info$size_prototypes[[ref_id]],
            recognition_bias = recognition_bias,
            iconicity_weight = iconicity_weight,
            k_perception = k_perception,
            circle_radius = circle_radius)
          
          prob <- recognition$probs
          success <- rbinom(1, 1, prob)
          
          if (listener == "A") {
            agentA_guess[ref_id] <- update_logit(prob, learning_strength, success, success_scale, failure_scale)
          } else {
            agentB_guess[ref_id] <- update_logit(prob, learning_strength, success, success_scale, failure_scale)
          }
          
          if (success == 1) {
            referents_info$agentA_stored_signal[[ref_id]] <- 
              (signal + referents_info$agentA_stored_signal[[ref_id]]) / 2
            referents_info$agentB_stored_signal[[ref_id]] <- 
              (signal + referents_info$agentB_stored_signal[[ref_id]]) / 2
          }
          
          new_guess_A <- agentA_guess[ref_id]
          new_guess_B <- agentB_guess[ref_id]
          new_stored_signal_A <- referents_info$agentA_stored_signal[[ref_id]]
          new_stored_signal_B <- referents_info$agentB_stored_signal[[ref_id]]
          
          has_semantic <- !is.na(attractor_id) && is_semantic_attractor[attractor_id]
          log_is_semantic <- if (is.na(attractor_id)) FALSE else is_semantic_attractor[attractor_id]
          is_correct_semantic_attractor <- if (has_semantic) {
            identical(attractor_centers[[attractor_id]], referents_info$size_prototypes[[ref_id]])
          } else {
            FALSE
          }
          
          simulation_log[[length(simulation_log) + 1]] <- tibble(
            simulation = sim, generation = gen, round = round, trial = trial, trial_counter = trial_counter,
            referent = ref_id, speaker = speaker, listener = listener, type = referents_info$type[ref_id],
            produced_signal = list(signal), dist_to_nearest = dist_to_nearest, in_attractor = in_attractor,
            attractor_id = attractor_id, is_semantic_attractor = log_is_semantic,
            is_correct_semantic_attractor = is_correct_semantic_attractor,
            prob = prob, evidence = recognition$evidence, success = success,
            # --- agent-level expressive flags, no longer meaningful under flat design ---
            # expressive_A = expressive_A, expressive_B = expressive_B,
            is_expressive_trial = is_expressive_trial,
            old_guess_A = old_guess_A, new_guess_A = new_guess_A,
            old_guess_B = old_guess_B, new_guess_B = new_guess_B,
            old_stored_signal_A = list(old_stored_signal_A), old_stored_signal_B = list(old_stored_signal_B),
            new_stored_signal_A = list(new_stored_signal_A), new_stored_signal_B = list(new_stored_signal_B)
          )
        }
      }
    }
  }
  full_history <- bind_rows(simulation_log)
  return(full_history)
}




plan(multisession, workers = parallel::detectCores() - 1)

N_ROUNDS <- 50

empty_df <- data.frame(
  sim = integer(), gen = integer(), round = integer(), trial = integer(),
  trial_counter = integer(), referent = integer(),
  speaker = character(), listener = character(), type = character(),
  produced_signal = I(list()), dist_to_nearest = numeric(), in_attractor = logical(),
  attractor_id = integer(), is_semantic_attractor = logical(), is_correct_semantic_attractor = logical(),
  old_stored_signal_A = I(list()), new_stored_signal_A = I(list()),
  old_stored_signal_B = I(list()), new_stored_signal_B = I(list()),
  prob = numeric(), success = integer(), evidence = numeric(),
  is_expressive_trial = logical(),
  old_guess_A = numeric(), new_guess_A = numeric(),
  old_guess_B = numeric(), new_guess_B = numeric(),
  stringsAsFactors = FALSE)

# Shared "nuisance" grid, coarse, reused for both mechanisms
d.nuisance_grid <- expand.grid(
  drift_sd          = c(0.125, 0.200, 0.275), #= seq(0.05, 0.35, length.out = 5),
  learning_strength = c(0.015, 0.030, 0.045, 0.060, 0.075, 0.090), # seq(0, 0.05, length.out = 5),
  circle_radius     = seq(0.15, 0.4, length.out = 3), 
  center_sd_ratio   = c(0.2, 0.4, 0.6)) %>%
  mutate(center_sd = circle_radius * center_sd_ratio)

compute_iconicity <- function(history, n_rounds, cutoff = 0.8) {
  d.iconicity <- history %>%
    mutate(total_round = (generation - 1) * n_rounds + round) %>%
    group_by(simulation, total_round) %>%
    summarise(evidence = mean(evidence, na.rm = TRUE), .groups = "drop")
  
  max_round <- max(d.iconicity$total_round)
  threshold <- max_round * cutoff
  
  d.iconicity %>%
    filter(total_round >= threshold) %>%
    summarise(mean_iconicity = mean(evidence, na.rm = TRUE)) %>%
    pull(mean_iconicity)
}

## GRID 1: recognitionBias
if (RESET_MODELS || !file.exists("models/grid-search-recognitionBias.rds")) {
  
  d.grid.recognitionBias <- d.nuisance_grid %>%
    tidyr::crossing(iconicity_weight = seq(0, 6, length.out = 15)) %>% #seq(0, 0.8, length.out = 10)
    mutate(iconicity = NA_real_, history = vector("list", n()))
  
  for (i in seq_len(nrow(d.grid.recognitionBias))) {
    p <- d.grid.recognitionBias[i, ]
    history <- run_interaction_sim(
      data = empty_df, n_sim = 100, n_referents = 4, n_generations = 1, n_rounds = N_ROUNDS,
      drift_sd = p$drift_sd, learning_strength = p$learning_strength,
      recognition_bias = TRUE, iconicity_weight = p$iconicity_weight,
      circle_radius = p$circle_radius, trap_center_sd = p$center_sd,
      expressive_agents = FALSE,
      success_scale = 7.5, failure_scale = 1
    )
    d.grid.recognitionBias$history[[i]] <- history
    d.grid.recognitionBias$iconicity[i] <- compute_iconicity(history, n_rounds = N_ROUNDS)
    if (i %% 50 == 0) message("recognitionBias: ", i, " / ", nrow(d.grid.recognitionBias))
  }
  saveRDS(d.grid.recognitionBias, "models/grid-search-recognitionBias.rds", compress = TRUE)
} else {
  d.grid.recognitionBias <- readRDS("models/grid-search-recognitionBias.rds")
}

## GRID 2: expressiveAgents
if (RESET_MODELS || !file.exists("models/grid-search-expressiveAgents.rds")) {
  
  d.grid.expressiveAgents <- d.nuisance_grid %>%
    tidyr::crossing(expressive_probability = seq(0.02, 0.22, by = 0.04)) %>%
    mutate(iconicity = NA_real_, history = vector("list", n()))
  
  for (i in seq_len(nrow(d.grid.expressiveAgents))) {
    p <- d.grid.expressiveAgents[i, ]
    history <- run_interaction_sim(
      data = empty_df, n_sim = 20, n_referents = 4, n_generations = 1, n_rounds = N_ROUNDS,
      drift_sd = p$drift_sd, learning_strength = p$learning_strength,
      recognition_bias = FALSE, iconicity_weight = 0,
      circle_radius = p$circle_radius, trap_center_sd = p$center_sd,
      expressive_agents = TRUE, expressive_probability = p$expressive_probability,
      success_scale = 7.5, failure_scale = 1
    )
    d.grid.expressiveAgents$history[[i]] <- history
    d.grid.expressiveAgents$iconicity[i] <- compute_iconicity(history, n_rounds = N_ROUNDS)
    if (i %% 50 == 0) message("expressiveAgents: ", i, " / ", nrow(d.grid.expressiveAgents))
  }
  saveRDS(d.grid.expressiveAgents, "models/grid-search-expressiveAgents.rds", compress = TRUE)
} else {
  d.grid.expressiveAgents <- readRDS("models/grid-search-expressiveAgents.rds")
}
