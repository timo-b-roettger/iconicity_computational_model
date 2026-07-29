# LIBRARIES AND HELPERS----
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
                           attractor_centers, circle_radius, center_sd, k_attractor,
                           k_attractor_production) {
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

  final_sd <- max(baseline_sd - attractor_reduction, 0.001) # keep sd above 0
  
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
    drift_sd = 0.19,
    learning_strength = 0.015,
    recognition_bias = FALSE,
    iconicity_weight = 0.4,
    success_scale = 7.5,
    failure_scale = 1,
    k_attractor_production = 2.5,
    k_perception = 2.5,
    neutral_attractor_centers = list(c(0.15, 0.15), c(0.85, 0.85)),
    circle_radius = 0.3,
    trap_center_sd = 0.12,
    expressive_agents = TRUE,
    expressive_noise_sd = circle_radius / 4.8,
    # --- two-level hierarchical expressiveness (kept for generational-overturn work) ---
    # expressive_prob_per_agent = 0.10, # per-generation probability an agent is expressive
    # expressive_trial_prob = 0.20,     # per-trial probability of override, conditional on being that type
    # --- simplified single-parameter version ---
    expressive_probability = 0.1  # flat per-trial probability of an expressive override, no agent-level persistence
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

# Call it
d.empty <- data.frame(
  sim = integer(), gen = integer(), round = integer(), trial = integer(),
  trial_counter = integer(), referent = integer(), 
  speaker = character(), listener = character(), type = character(),
  produced_signal = I(list()), dist_to_nearest = numeric(), in_attractor = logical(), 
  attractor_id = integer(), is_semantic_attractor = logical(), is_correct_semantic_attractor = logical(),
  old_stored_signal_A = I(list()), new_stored_signal_A = I(list()),
  old_stored_signal_B = I(list()), new_stored_signal_B = I(list()),
  prob = numeric(), success = integer(), evidence = numeric(),
  #expressive_A = logical(), expressive_B = logical(),
  is_expressive_trial = logical(),
  old_guess_A = numeric(), new_guess_A = numeric(),
  old_guess_B = numeric(), new_guess_B = numeric(),
  stringsAsFactors = FALSE)

# Run simulation function
d.simulation <- rbind(
  d.empty %>% 
    run_interaction_sim(n_sim = 1000, n_rounds = 50, n_generations = 1, recognition_bias = FALSE, expressive_agents = FALSE) %>%
    mutate(model_type = "baseline"),
  d.empty %>% 
    run_interaction_sim(n_sim = 1000, n_rounds = 50, n_generations = 1, recognition_bias = FALSE, expressive_agents = TRUE) %>%
    mutate(model_type = "expressiveAgents"),
  d.empty %>% 
    run_interaction_sim(n_sim = 1000, n_rounds = 50, n_generations = 1, recognition_bias = TRUE, expressive_agents = FALSE) %>%
    mutate(model_type = "recognitionBias"))

d.simulation.medium <- rbind(
  d.empty %>% 
    run_interaction_sim(n_sim = 100, n_rounds = 50, n_generations = 1, recognition_bias = FALSE, expressive_agents = FALSE) %>%
    mutate(model_type = "baseline"),
  d.empty %>% 
    run_interaction_sim(n_sim = 100, n_rounds = 50, n_generations = 1, recognition_bias = FALSE, expressive_agents = TRUE) %>%
    mutate(model_type = "expressiveAgents"),
  d.empty %>% 
    run_interaction_sim(n_sim = 100, n_rounds = 50, n_generations = 1, recognition_bias = TRUE, expressive_agents = FALSE) %>%
    mutate(model_type = "recognitionBias"))

d.simulation.small <- rbind(
  d.empty %>% 
    run_interaction_sim(n_sim = 10, n_rounds = 50, n_generations = 1, recognition_bias = FALSE, expressive_agents = FALSE) %>%
    mutate(model_type = "baseline"),
  d.empty %>% 
    run_interaction_sim(n_sim = 10, n_rounds = 50, n_generations = 1, recognition_bias = FALSE, expressive_agents = TRUE) %>%
    mutate(model_type = "expressiveAgents"),
  d.empty %>% 
    run_interaction_sim(n_sim = 10, n_rounds = 50, n_generations = 1, recognition_bias = TRUE, expressive_agents = FALSE) %>%
    mutate(model_type = "recognitionBias"))

# function to compute iconicity (signal evidence; e.g., for grid searches further down)
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

# test increasing expressivity proportion
d.test_high_expr <- d.empty %>%
  run_interaction_sim(
    n_sim = 10, n_rounds = 50, n_generations = 1,
    recognition_bias = FALSE, expressive_agents = TRUE,
    expressive_probability = 0.15)

# calculate mean signal evidence and success rate by expressive probability proportion
d.test_high_expr %>%
  group_by(is_expressive_trial) %>%
  summarise(success_rate = mean(success), mean_evidence = mean(evidence), n = n())

# and the actual comparison metric:
compute_iconicity(d.test_high_expr, n_rounds = 50)

# Signal space use across simulations
d_signal_mean <- d.simulation.small %>%
  mutate(total_round = (generation - 1) * 50 + round,
         x = map_dbl(produced_signal, 1),
         y = map_dbl(produced_signal, 2)) %>%
  group_by(model_type, total_round, type, generation) %>%
  summarise(
    mean_x = mean(x, na.rm = TRUE),
    mean_y = mean(y, na.rm = TRUE),
    .groups = "drop")


# Signal space use across simulations
d_signal <- d.simulation.small %>%
  mutate(total_round = (generation - 1) * 50 + round,
         #mutate(total_round = (generation - 1) * 10 + round,
         x = map_dbl(produced_signal, 1),
         y = map_dbl(produced_signal, 2))


# calculate proportion of trials ending up in an attractor, in a semantic attractor, in the correct semantic attractor
d.simulation %>%
  group_by(model_type) %>%
  summarise(
    total_trials = n(),
    n_in = sum(in_attractor == TRUE | in_attractor == "TRUE", na.rm = TRUE),
    n_out = sum(in_attractor == FALSE | in_attractor == "FALSE", na.rm = TRUE),
    n_sem = sum(is_semantic_attractor == TRUE | is_semantic_attractor == "TRUE", na.rm = TRUE),
    n_corr_sem = sum(is_correct_semantic_attractor == TRUE | is_correct_semantic_attractor == "TRUE", na.rm = TRUE),
    pct_in_attractor = (n_in / total_trials) * 100,
    pct_out_attractor = (n_out / total_trials) * 100,
    pct_in_sem_attractor = (n_sem / n_in) * 100,           # Out of those IN an attractor
    pct_in_correct_sem_attractor = (n_corr_sem / n_sem) * 100,     # Out of those that are semantic
    .groups = "drop") %>%
  select(-n_in, -n_out, -n_sem, -n_corr_sem)

# PLOT ICONICITY----
d.iconicity <- d.simulation.small %>%
  mutate(model_type = factor(
    model_type, 
    levels = c("baseline", "expressiveAgents", "recognitionBias"),
    labels = c("Baseline", "Expressive agents", "Iconicity recognition bias"),
    ordered = TRUE),
    total_round = (generation - 1) * 50 + round,
    strength = abs(evidence)) %>%
  group_by(model_type, simulation, generation, total_round, type, referent) %>%
  summarise(
    evidence = mean(evidence),
    strength = mean(strength),
    .groups = "drop") %>%
  group_by(model_type, simulation, generation, total_round) %>%
  summarise(
    evidence = mean(evidence),
    strength = mean(strength),
    .groups = "drop")

d.iconicity.mean <- d.iconicity |> 
  group_by(model_type, total_round) %>%
  summarise(evidence = mean(evidence),
            strength = mean(strength),
            .groups = "drop")

# Attempt at adding densities along y
d.iconicity |> 
  ggplot(aes(x = total_round, y = evidence, group = interaction(model_type, simulation),
             color = evidence)) +
  geom_path(linewidth = 0.5, alpha = 0.06,
            color = "black") +
  geom_path(data = d.iconicity.mean, 
            aes(group = 1), linewidth = 2,
            color = "black") +
  geom_path(data = d.iconicity.mean, 
            aes(group = 1), linewidth = 1) +  
  # # Add lines at generational overturn
  # geom_vline(xintercept = seq(0, max(d.iconicity$total_round), by = 50), 
  #            color = "grey", 
  #            lty = "dotted") +
  scale_color_viridis_c(begin = 0,
                        end = 1,
                        values = seq(0,1,0.1))+
  scale_y_continuous(limits = c(-1,1), breaks = seq(-1,1,0.25)) +
  scale_x_continuous(breaks = seq(0, max(d.iconicity$total_round), by = 10),
                     labels = seq(0, max(d.iconicity$total_round), by = 10)) +
  labs(title = "Iconicity over interaction rounds",
       y = "Iconicity\n(above 0 = iconic,\nbelow zero = anti-iconic)", 
       x = "Interaction rounds") +
  theme_minimal() +
  facet_wrap(~model_type) +
  # marginal distribution per facet (y-axis)
  geom_ysidedensity(
    data = d.iconicity,
    aes(y = evidence, group = model_type),
    inherit.aes = FALSE,
    fill = "grey",
    alpha = 0.5) +
  scale_ysidex_continuous()

# GRID SEARCHES ----
RESET_MODELS <- FALSE  # set TRUE to force a rerun

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
  drift_sd          = seq(0.05, 0.35, length.out = 5),
  learning_strength = seq(0, 0.05, length.out = 5),
  circle_radius     = seq(0.15, 0.4, length.out = 3),
  center_sd_ratio   = c(0.2, 0.4, 0.6)   # proportion of circle_radius for grid search; since center_sd is dependent on radius (0.4 = 40% of the basin's radius is steady-state noise)
) %>%
  mutate(center_sd = circle_radius * center_sd_ratio)

## GRID 1: recognitionBias
if (RESET_MODELS || !file.exists("models/grid-search-recognitionBias-full.rds")) {
  
  d.grid.recognitionBias <- d.nuisance_grid %>%
    tidyr::crossing(iconicity_weight = seq(0, 0.8, length.out = 10)) %>%
    mutate(iconicity = NA_real_, history = vector("list", n()))
  
  for (i in seq_len(nrow(d.grid.recognitionBias))) {
    p <- d.grid.recognitionBias[i, ]
    history <- run_interaction_sim(
      data = empty_df, n_sim = 20, n_referents = 4, n_generations = 1, n_rounds = N_ROUNDS,
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
  saveRDS(d.grid.recognitionBias, "models/grid-search-recognitionBias-full.rds", compress = TRUE)
} else {
  d.grid.recognitionBias <- readRDS("models/grid-search-recognitionBias-update.rds")
}

## GRID 2: expressiveAgents
if (RESET_MODELS || !file.exists("models/grid-search-expressiveAgents-full.rds")) {
  
  d.grid.expressiveAgents <- d.nuisance_grid %>%
    tidyr::crossing(expressive_probability = seq(0.02, 0.20, by = 0.02)) %>%
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
  saveRDS(d.grid.expressiveAgents, "models/grid-search-expressiveAgents-full.rds", compress = TRUE)
} else {
  d.grid.expressiveAgents <- readRDS("models/grid-search-expressiveAgents-full.rds")
}

# ## GRID 1: recognition-bias mechanism (not all params)
# if (RESET_MODELS || !file.exists("models/grid-search-recognitionBias.rds")) {
#   
#   d.grid.recognitionBias <- expand.grid(
#     iconicity_weight  = seq(0, 0.5, length.out = 8),
#     learning_strength = seq(0, 0.05, length.out = 8),
#     drift_sd          = seq(0.09, 0.5, length.out = 8)) %>%
#     mutate(iconicity = NA_real_, history = vector("list", n()))
#   
#   for (i in seq_len(nrow(d.grid.recognitionBias))) {
#     params <- d.grid.recognitionBias[i, ]
#     
#     history <- run_interaction_sim(
#       data = empty_df,
#       n_sim = 100,
#       n_referents = 4,
#       n_generations = 1,
#       n_rounds = N_ROUNDS,
#       drift_sd = params$drift_sd,
#       learning_strength = params$learning_strength,
#       recognition_bias = TRUE,
#       iconicity_weight = params$iconicity_weight,
#       expressive_agents = FALSE,
#       success_scale = 7.5,
#       failure_scale = 1)
#     
#     #d.grid.recognitionBias$history[[i]] <- history
#     d.grid.recognitionBias$iconicity[i] <- compute_iconicity(history, n_rounds = N_ROUNDS)
#     rm(history); gc()                                     # free memory each iteration
#     message("recognitionBias: completed parameter set ", i, " of ", nrow(d.grid.recognitionBias))
#   }
#   saveRDS(d.grid.recognitionBias, file = "models/grid-search-recognitionBias.rds", compress = TRUE)
#   
# } else {
#   d.grid.recognitionBias <- readRDS("models/grid-search-recognitionBias.rds")
# }
# 
# ## GRID 2: expressive-agent mechanism (not all params)
# if (RESET_MODELS || !file.exists("models/grid-search-expressiveAgents.rds")) {
#   
#   d.grid.expressiveAgents <- expand.grid(
#     expressive_probability = seq(0, 0.3, length.out = 8),
#     drift_sd                  = seq(0.09, 0.5, length.out = 8)) %>%
#     mutate(iconicity = NA_real_, history = vector("list", n()))
#   
#   for (i in seq_len(nrow(d.grid.expressiveAgents))) {
#     params <- d.grid.expressiveAgents[i, ]
#     
#     history <- run_interaction_sim(
#       data = empty_df,
#       n_sim = 100,
#       n_referents = 4,
#       n_generations = 1,
#       n_rounds = N_ROUNDS,
#       drift_sd = params$drift_sd,
#       learning_strength = 0.015,      # hold fixed; not this mechanism's axis
#       recognition_bias = FALSE,
#       iconicity_weight = 0,
#       expressive_agents = TRUE,
#       expressive_probability = params$expressive_probability,
#       success_scale = 7.5,
#       failure_scale = 1
#     )
#     
#     #d.grid.expressiveAgents$history[[i]] <- history
#     d.grid.expressiveAgents$iconicity[i] <- compute_iconicity(history, n_rounds = N_ROUNDS)
#     rm(history); gc() 
#     message("expressiveAgents: completed parameter set ", i, " of ", nrow(d.grid.expressiveAgents))
#   }
#   saveRDS(d.grid.expressiveAgents, file = "models/grid-search-expressiveAgents.rds", compress = TRUE)
#   
# } else {
#   d.grid.expressiveAgents <- readRDS("models/grid-search-expressiveAgents.rds")
# }


combined_range <- range(c(d.grid.recognitionBias$iconicity, d.grid.expressiveAgents$iconicity), na.rm = TRUE)
# combined_range = c(-0.08, 0.57)

# adjust this to try different learning strength (0, .0125, .025...)
ls_fixed <- unique(d.grid.recognitionBias$learning_strength)[2]

plot_grid_faceted <- function(d.grid, x_var, x_lab, fixed_learning_strength, limits) {
  d.grid %>%
    filter(learning_strength == fixed_learning_strength) %>%
    ggplot(aes(x = factor(round(.data[[x_var]], 3)),
               y = factor(round(drift_sd, 3)),
               fill = iconicity)) +
    geom_tile() +
    geom_text(aes(label = round(iconicity, 2)), size = 2.5, color = "black") +
    scale_fill_distiller(palette = "RdBu", direction = -1,
                         limits = limits,
                         rescaler = ~ scales::rescale_mid(.x, mid = 0, to = c(0, 1),
                                                          from = limits)) +
    facet_grid(center_sd_ratio ~ circle_radius, labeller = label_both) +
    theme_minimal() +
    labs(x = x_lab, y = "Drift SD", fill = "Iconicity",
         title = paste0("Learning strength = ", round(fixed_learning_strength, 3)))
}

p.recognitionBias <- plot_grid_faceted(d.grid.recognitionBias, "iconicity_weight",
                                       "Iconicity weight", ls_fixed, combined_range)
p.expressiveAgents <- plot_grid_faceted(d.grid.expressiveAgents, "expressive_probability",
                                        "Expressive probability", ls_fixed, combined_range)

#to derive a radius value for a given center_sd_ratio in the plot, do this:
d.nuisance_grid %>%
  filter(center_sd_ratio == 0.2) %>%
  select(circle_radius, center_sd_ratio, center_sd) %>%
  distinct() %>%
  arrange(circle_radius)

d.grid.compare <- bind_rows(
  d.grid.recognitionBias %>%
    group_by(drift_sd) %>%
    summarise(peak_iconicity = max(iconicity, na.rm = TRUE), .groups = "drop") %>%
    mutate(mechanism = "Iconicity recognition bias"),
  d.grid.expressiveAgents %>%
    group_by(drift_sd) %>%
    summarise(peak_iconicity = max(iconicity, na.rm = TRUE), .groups = "drop") %>%
    mutate(mechanism = "Expressive agents"))

ggplot(d.grid.compare, 
       aes(x = drift_sd, 
           y = peak_iconicity, 
           color = mechanism)) +
  geom_line(linewidth = 1) + 
  geom_point() +
  scale_y_continuous(limits = c(-1, 1)) + 
  theme_minimal() +
  labs(x = "Drift SD", 
       y = "Peak iconicity (best combo of all other params)", 
       color = "Mechanism")


# TIMO EXPLORES ----
## plot 10 random simulations
### evidence
d.iconicity |> 
  filter(simulation %in% c(1:10)) |> 
  filter(model_type == "Baseline") |> 
  ggplot(aes(x = total_round, y = evidence, group = interaction(model_type, simulation),
             color = evidence)) +
  geom_path(linewidth = 0.5, 
            color = "black") +
  # # Add lines at generational overturn
  # geom_vline(xintercept = seq(0, max(d.iconicity$total_round), by = 50), 
  #            color = "grey", 
  #            lty = "dotted") +
  scale_color_viridis_c(begin = 0,
                        end = 1,
                        values = seq(0,1,0.1))+
  scale_y_continuous(limits = c(-1,1), breaks = seq(-1,1,0.25)) +
  scale_x_continuous(breaks = seq(0, max(d.iconicity$total_round), by = 10),
                     labels = seq(0, max(d.iconicity$total_round), by = 10)) +
  labs(title = "Iconicity over interaction rounds",
       y = "Iconicity\n(above 0 = iconic,\nbelow zero = anti-iconic)", x = "Interaction rounds") +
  theme_minimal() +
  facet_wrap(simulation~model_type)

### guess rate
d.simulation |> 
  mutate(model_type = factor(
    model_type, 
    levels = c("semanticAttractors", "semanticAttractors_expressiveAgents", "semanticPhonAttractors", "allAttractors_expressiveAgents"),
    labels = c("Semantic attractors", "Semantic attractors, expressive agents", "Semantic and phonological attractors", "Semantic and phonological attractors, expressive agents"),
    ordered = TRUE),
    total_round = (generation - 1) * 50 + round,
    strength = abs(evidence)) %>%
  group_by(model_type, simulation, generation, total_round, type, referent) %>%
  summarise(
    evidence = mean(evidence),
    strength = mean(strength),
    guess = mean(new_guess_A, new_guess_B),
    .groups = "drop") %>%
  group_by(model_type, generation, total_round) %>%
  summarise(
    evidence = mean(evidence),
    strength = mean(strength),
    guess = mean(guess),
    .groups = "drop") |> 
  ggplot(aes(x = total_round, y = guess, group = interaction(model_type),
             color = evidence)) +
  geom_path(linewidth = 0.5, 
            color = "black") +
  # Add lines at generational overturn
  geom_vline(xintercept = seq(0, 100, by = 10), 
             color = "grey", 
             lty = "dotted") +
  scale_color_viridis_c(begin = 0,
                        end = 1,
                        values = seq(0,1,0.1))+
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.25)) +
  scale_x_continuous(breaks = seq(0, 500, 100),
                     labels = seq(0, 50, 10)) +
  
  labs(title = "Representational strength over generations",
       y = "Memory strength from 0-1\n", x = "Generation (each with 10 rounds)") +
  theme_minimal() +
  facet_wrap(~model_type, ncol = 1)

## signal space
# Signal space use across simulations
center_small <- c(0.15, 0.85)
center_large <- c(0.85, 0.15)
neutral_attractors <- list(c(0.15, 0.15), c(0.85, 0.85))

# The unified set used by produce_signal() in the simulation above.
plot_attractor_centers <- list(center_small, center_large,
                               neutral_attractors[[1]], neutral_attractors[[2]])

circle_radius <- 0.3
drift_sd <- 0.19
k_production <- 1.5
speaker_guess_fixed <- 0.5
center_sd <- 0.12
k_attractor_fixed <- 2.5

grid <- expand.grid(
  x = seq(0, 1, length.out = 200),
  y = seq(0, 1, length.out = 200),
  type = c("small", "large"))

grid_mapped <- grid %>%
  rowwise() %>%
  mutate(
    ease = signal_evidence(c(x, y),
                           if (type == "small") center_small else center_large,
                           circle_radius = circle_radius, k_perception = k_attractor_fixed),
    # localized production SD -- driven by ALL FOUR attractors
    local_sd = {
      base_sd <- drift_sd * (1 + k_production * (0.5 - speaker_guess_fixed))
      distances <- sapply(
        plot_attractor_centers,
        function(center) sqrt(sum((c(x, y) - center)^2)))
      inside_attractors <- distances < circle_radius
      if (any(inside_attractors)) {
        closest_dist <- min(distances[inside_attractors])
        rel_dist <- closest_dist / circle_radius
        
        magnitude <- exp(-k_attractor_fixed * rel_dist)
        max_mag <- exp(-k_attractor_fixed * 0)
        min_mag <- exp(-k_attractor_fixed * 1)
        exp_scale <- (magnitude - min_mag) / (max_mag - min_mag)
        
        edge_sd <- drift_sd
        base_sd <- edge_sd - (edge_sd - center_sd) * exp_scale
      }
      base_sd
    }
  ) %>%
  ungroup()


# Mark all four attractor centers, distinguishing -- per facet -- this
# referent's correct meaning, the OTHER referent's (wrong) meaning, and
# the two purely arbitrary/neutral attractors. Built with base merge()
# to avoid requiring tidyr::crossing(), but that works equally well.
attractor_points <- merge(
  data.frame(
    x = c(0.15, 0.85, 0.15, 0.85),
    y = c(0.85, 0.15, 0.15, 0.85),
    is_semantic = c(TRUE, TRUE, FALSE, FALSE)),
  data.frame(type = c("small", "large"))) %>%
  mutate(
    role = case_when(
      !is_semantic ~ "neutral",
      type == "small" & x == 0.15 & y == 0.85 ~ "correct",
      type == "large" & x == 0.85 & y == 0.15 ~ "correct",
      is_semantic ~ "wrong",
      TRUE ~ NA_character_))

grid_mapped %>%
  ggplot(
    aes(
      x = x, 
      y = y)) +
  geom_tile(aes(fill = ease)) +
  # Contour lines now map out all four basins, not two -- identical
  # across facets, since production doesn't know about meaning.
  geom_contour(aes(
    z = local_sd), 
    bins = 10, 
    color = "white", alpha = 0.7) +
  geom_point(
    data = attractor_points,
    aes(x = x, y = y, 
        shape = role, color = role),
        size = 4, stroke = 1.2) +
  scale_shape_manual(values = c(correct = 4, wrong = 4, neutral = 1)) +
  scale_color_manual(values = c(correct = "white", wrong = "red", neutral = "white")) +
  facet_wrap(~ type) +
  scale_fill_viridis_c() +
  theme_minimal() +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1))


# plot average gen 1-10
## not much movement
d_signal_mean  |> 
  group_by(model_type, type, generation) |> 
  summarise(mean_x = mean(mean_x),
            mean_y = mean(mean_y)) |> 
  #filter(generation == 10) |> 
  ggplot(
    aes(x = mean_x, y = mean_y,
        fill = as.ordered(generation))) +
  facet_grid(model_type ~ type) +
  geom_hline(yintercept = 0.5, 
             lty = "dashed") + 
  geom_vline(xintercept = 0.5, 
             lty = "dashed") +
  geom_point(alpha = 0.8, 
             size = 5, 
             pch = 21,
             color = "white") +
  labs(x = "x dimnesion",
       y = "y dimnesion",
       fill = "generations") 

# plot individual
temp_ind <- d_signal  |> 
  filter(model_type == "baseline") |> 
  filter(simulation == 1) |> 
  filter(generation  == 1)

ggplot(data = temp_ind,
       aes(x = x, y = y,
           fill = as.ordered(round),
           color = as.ordered(round),
           group = interaction(type, referent))) +
  facet_grid(referent ~ type) +
  geom_hline(yintercept = 0.5, 
             lty = "dashed") + 
  geom_vline(xintercept = 0.5, 
             lty = "dashed") +
  geom_path(
    arrow = arrow(length = unit(0.2, "cm"), type = "closed")
  ) +
  # success and fail
  geom_point(data = temp_ind |> filter(success == 1),
             alpha = 1, 
             size = 5, 
             pch = 21,
             color = "white") +
  geom_point(data = temp_ind |> filter(success == 0),
             alpha = 1, 
             size = 5, 
             pch = 24,
             fill = "darkred",
             color = "white") +
  labs(x = "x dimnesion",
       y = "y dimnesion",
       fill = "generations") +
  theme_bw() +
  theme()

# calculation ED to semantic attractor, to phonological distractors and semantic repeller
euc_dist <- function(x1, x2){
  return(sqrt(sum((x1 - x2)^2)))
}

# test
euc_dist(c(0.15,0.85), c(0.44,0.56))

# add as vector
d_signal_ed <- d_signal |> 
  mutate(model_type = factor(
    model_type, 
    levels = c("baseline", "expressiveAgents", "recognitionBias"),
    labels = c("Baseline", "Expressive agents", "Iconicity recognition bias"),
    ordered = TRUE)) |> 
  mutate(iconic_ed = ifelse(type == "small",
                            sqrt((x - 0.15)^2 + (y - 0.85)^2),
                            sqrt((x - 0.85)^2 + (y - 0.15)^2)),
         attractor_1_ed = sqrt((x - 0.15)^2 + (y - 0.15)^2),
         attractor_2_ed = sqrt((x - 0.85)^2 + (y - 0.85)^2)
  )

d_signal_ed_agg <- d_signal_ed |> 
  group_by(model_type, simulation, type, round) |> 
  summarise(iconic_ed = mean(iconic_ed),
            attractor_1_ed = mean(attractor_1_ed),
            attractor_2_ed = mean(attractor_2_ed))

d_signal_ed_agg_all <- d_signal_ed_agg |> 
  group_by(model_type, type, round) |> 
  summarise(iconic_ed = mean(iconic_ed),
            attractor_1_ed = mean(attractor_1_ed),
            attractor_2_ed = mean(attractor_2_ed))

# get reference values
## if small where is border for attractor?
iconic_border = euc_dist(c(0.15,0.85), c(0.45,0.55)) ## 0.42
neutral = euc_dist(c(0.15,0.85), c(0.5,0.5)) ## ~0.5

# plot average ED
d_signal_ed_agg_all |> 
  ggplot(aes(x = round, y = iconic_ed,
             group = interaction(model_type))) +
  geom_path(data = d_signal_ed_agg,
            alpha = 0.01) +
  geom_line(data = d_signal_ed_agg_all,
            alpha = 1) +
  # iconic border
  geom_hline(yintercept = iconic_border,
             color = "purple",
             lty = "dashed") +
  annotate("text", 
           x = 25,
           y = 0.21,
           color = "purple",
           label = "within iconic pocket") +
  # neutral position
  geom_hline(yintercept = neutral,
             color = "black",
             lty = "dashed") +
  facet_grid(~model_type) +
  labs(title = "euclidean distance to iconic center",
       y = "euclidean distance to iconic center") +
  theme_minimal()


# plot also for distractor
d_signal_ed_agg_all |> 
  ggplot(aes(x = round, y = attractor_1_ed,
             group = interaction(model_type))) +
  geom_path(data = d_signal_ed_agg,
            alpha = 0.01) +
  geom_line(data = d_signal_ed_agg_all,
            alpha = 1) +
  # attractor border
  geom_hline(yintercept = attractor_border,
             color = "black",
             lty = "dashed") +
  annotate("text", 
           x = 50,
           y = 0.21,
           color = "black",
           label = "within distractor") +
  facet_grid(~model_type) +
  labs(title = "euclidean distance to phonological distractors",
       y = "euclidean distance to phonological distractors") +
  theme_minimal()

# how often does the signal enters pockets
d_signal_ed <- d_signal_ed |> 
  mutate(in_pocket = ifelse(iconic_ed < iconic_border, 1, 0)) 

d_signal_ed_agg <- d_signal_ed |> 
  group_by(model_type, simulation, type, round) |> 
  summarise(iconic_ed = mean(iconic_ed),
            attractor_1_ed = mean(attractor_1_ed),
            attractor_2_ed = mean(attractor_2_ed),
            in_pocket = mean(in_pocket))

d_signal_ed_agg_all <- d_signal_ed_agg |> 
  group_by(model_type, type, round) |> 
  summarise(iconic_ed = mean(iconic_ed),
            attractor_1_ed = mean(attractor_1_ed),
            attractor_2_ed = mean(attractor_2_ed),
            in_pocket = mean(in_pocket))

# calculate when attractor was entered and when 
# plot average ED
d_signal_ed_agg_all |> 
  ggplot(aes(x = round, y = in_pocket,
             group = interaction(model_type))) +
  geom_line() +
  facet_grid(~model_type) +
  labs(title = "proportion of entering the iconic pocket",
       y = "proportion of entering the iconic pocket") +
  theme_minimal()



####
d_signal_mean  |>
  group_by(model_type, type, generation) |>
  summarise(mean_x = mean(mean_x),
            mean_y = mean(mean_y)) |> 
  mutate(quadrant = case_when(mean_x < 0.3 & mean_y < 0.3 ~ "top left",
                              between(mean_x, 0.3, 0.6) & mean_y < 0.3 ~ "top middle",
                              mean_x > 0.6 & mean_y < 0.3 ~ "top right",
                              
                              mean_x < 0.3 & between(mean_y, 0.3, 0.6) ~ "middle left",
                              between(mean_x, 0.3, 0.6) & between(mean_y, 0.3, 0.6) ~ "center",
                              mean_x > 0.6 & between(mean_y, 0.3, 0.6) ~ "middle right",
                              mean_x < 0.3 & mean_y > 0.6 ~ "bottom left",
                              between(mean_x, 0.3, 0.6) & mean_y > 0.6 ~ "bottom middle",
                              mean_x > 0.6 & mean_y > 0.6 ~ "bottom right"
  )) |> 
  group_by(model_type, type) |>
  count(quadrant) |> 
  ggplot(
    aes(x = 0:1, y = 0:1,
        fill = quadrant)) +
  geom_tile()
facet_grid(model_type ~ type) +
  geom_hline(yintercept = 0.5, 
             lty = "dashed") + 
  geom_vline(xintercept = 0.5, 
             lty = "dashed") +
  geom_point(alpha = 0.8, 
             size = 5, 
             pch = 21,
             color = "white") +
  labs(x = "x dimnesion",
       y = "y dimnesion",
       fill = "generations") 

my_breaks = c(2, 10, 50, 250, 1250, 6000)

d_signal  |>
  mutate(model_type = factor(
    model_type, 
    levels = c("baseline", "expressiveAgents", "recognitionBias", "recognitionBias_expressiveAgents"),
    labels = c("Baseline", "+ expressive agents", "+ iconicity recognition bias", "+ iconicity recognition bias and expressive agents"),
    ordered = TRUE)) |> 
  ggplot(aes(x = x, y = y)) +
  # binwidth = 1/3 creates 3 bins for both X and Y across the 0-1 range
  # boundary = 0 forces the bins to start exactly at 0.0
  #geom_bin2d(binwidth = c(1/27, 1/27), boundary = 0, color = "white", linewidth = 0.5) +
  stat_binhex() +
  
  # Use a clean color scale for the heatmap counts
  # scale_fill_gradient2(name = "count", trans = "log",
  #                     breaks = my_breaks, labels = my_breaks,
  #                     low = "white", 
  #                     mid = "blue",
  #                     high = "red"
  #                     ) +
  scale_fill_gradientn(
    # Step A: Define the exact sequence of colors
    colors = c("#f7f7f7", "#fe9e2a", "#d7191c"),
    
    # Step B: Map those colors to specific numeric points along the data range
    # Rescale your target numbers between 0.0 (min) and 1.0 (max)
    values = scales::rescale(c(0.0, 0.1, 1.0)),
    
    # Step C: Customize the legend appearance
    #guide = guide_colorbar(barwidth = 1, barheight = 15)
  ) +
  
  # Styling
  theme_minimal() +
  # attractor outline
  # annotate("rect", 
  #          xmin = c(0, 2/3, 0, 2/3), 
  #          xmax = c(1/3, 1, 1/3, 1), 
  #          ymin = c(0, 0, 2/3, 2/3), 
  #          ymax = c(1/3, 1/3, 1, 1), 
  #          colour = "black", 
  #          fill = "transparent", 
  #          size = 0.5) +
  labs(
    title = "Signal Space",
    subtitle = "Data binned into equal intervals",
    x = "X Coordinate",
    y = "Y Coordinate",
    fill = "Count"
  ) +
  facet_grid(type~model_type) +
  scale_x_continuous(limits = c(0,1),
                     breaks = seq(0,1,1/3),
                     labels = c(0,"1/3", "2/3", 1)) +
  scale_y_continuous(limits = c(0,1),
                     breaks = seq(0,1,1/3),
                     labels = c(0,"1/3", "2/3", 1)) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  ) 



