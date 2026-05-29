# 1. Change production to a two-dimensional space with circle-shaped semantic attractors
n_referents = 4

referents_info <- tibble(
  id = seq_len(n_referents),
  type = rep(c("small","large"), length.out = n_referents),
  size_prototypes = if_else(type == "small", 
                            list(c(0.15, 0.85)), 
                            list(c(0.85, 0.15))),
  agentA_stored_signal = rep(list(c(0.5, 0.5)), n_referents),
  agentB_stored_signal = rep(list(c(0.5, 0.5)), n_referents))

clamp01 <- function(x) pmax(0, pmin(1, x))

produce_signal <- function(stored_signal, speaker_guess, drift_sd, k_production) {
  signal <- rnorm(
    length(stored_signal),
    mean = stored_signal,
    sd = drift_sd * (1 + k_production * (0.5 - speaker_guess)))
  clamp01(signal)
}

signal <- produce_signal(
  stored_signal = c(0.5, 0.5),
  speaker_guess[ref_id],
  drift_sd = 0.23,
  k_production = 1.5)
# Override signal production if agent is expressive; on ~20% of all trials for that expressive agent
is_expressive_trial <- FALSE
if (
  ((speaker == "A" && expressive_A) ||
   (speaker == "B" && expressive_B)) &&
  runif(1) < 0.20
) {
  # If the trial is expressive, produce a perfectly iconic signal for that referent
  is_expressive_trial <- TRUE
  signal <- referents_info$size_prototypes[[ref_id]]
  signal <- clamp01(signal)
}

# Update signal evidence calculation
signal_evidence <- function(produced_signal, center_target, k_perception = 2.5, circle_radius = 0.3) {
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


listener_guess_probability <- function(listener_guess, produced_signal, size_prototypes) {
  icon_ev <- signal_evidence(produced_signal, size_prototypes)
  logits <- qlogis(clamp02(listener_guess + (iconicity_weight * icon_ev)))
  probs <- plogis(logits)
  
  return(list(probs = probs, evidence = icon_ev))
}

recognition <- listener_guess_probability(
  listener_guess[ref_id],
  produced_signal,
  referents_info$size_prototypes[[ref_id]])


# ENTIRE LOOP REVISITED --------------
run_interaction_sim <- function(
    data,
    n_sim = 1, # number of simulations
    n_referents = 4, # number of unique referents in guessing game
    n_generations = 10, # no of generations
    n_rounds = 10, # number of interaction rounds; 10
    drift_sd = 0.19,  # amount of variation introduced during production; motor noise; equivalent to approx. 10% chance of wandering into a pocket when the signal is at .5, .5 and previous guess = 0.5
    learning_strength = 0.015, # amount of added memory strengthening for referents as dependent on success; corresponding to .01 increase for a probability of 0.5 on failure, and .09 increase on success
    iconicity_weight = 0.4,  # multiplicator for iconicity; corresponding to ~10% absolute increase for a probability of 0.5 (for the perfectly iconic signal)
    success_scale = 7.5, # more learning with success; 95% accuracy at the end of 10th round
    failure_scale = 1, # also increase in learning with failure, but less so; 60% accuracy at the end of 10th round
    expressive_agents = TRUE, # switch on/off expressive agents
    phonological_attractors = TRUE, # switch on/off use of phonological attractors
    k_attractor = 2.5, # controls the shape of decay from attractor centers
    phonological_traps = list(c(0.15, 0.15), c(0.85, 0.85)), # define phonological attractors/traps; currently two, in the opposite corners of the two-dimensional space
    circle_radius = 0.3, # atm, identical size of both types of attractors
    trap_center_sd = 0.12  # 5% single-step escape probability from center of phonological attractor
) {
  
  # Turn phonological attractors on/off
  if (!phonological_attractors) {
    phonological_traps <- list()
  }
  
  # Probability of expressive speakers--10% chance that an agent is expressive; ~20% chance of an expressive agent in this generation
  expressive_prob <- if (expressive_agents) 0.10 else 0
  
  # HELPER FUNCTIONS CALLED IN SIMULATION LOOP
  # SIGNAL PRODUCTION + MECHANISMS AFFECTING Y-DIMENSION
  clamp01 <- function(x) pmax(0, pmin(1, x))

  # At signal production, amount of noise is dependent on guess rate AND phonological traps
  # k controls how strongly sd react to speaker_guess; mindful that if p = 1 and k >2, it will return negative sd's
  produce_signal <- function(stored_signal, speaker_guess, drift_sd, k_production, phonological_traps, circle_radius, center_sd, k_attractor) {
    # Calculate baseline trial-specific SD based on speaker's guess
    sd <- drift_sd * (1 + k_production * (0.5 - speaker_guess))
    is_inside <- FALSE # Default to FALSE
    
    if (length(phonological_traps) > 0) {
      # Dynamically calculate Euclidean distance to every defined trap center
      distances <- sapply(phonological_traps, function(center) sqrt(sum((stored_signal - center)^2)))
      # Find if the signal is inside any trap, and locate the closest one
      inside_traps <- distances < circle_radius
      
      if (any(inside_traps)) {
        is_inside <- TRUE # Flip to TRUE because the starting signal is in a trap
        # Target the closest trap that the signal is currently inside
        closest_trap_dist <- min(distances[inside_traps])
        rel_dist <- closest_trap_dist / circle_radius
        # scale distance dynamically; similar to semantic attractors
        magnitude <- exp(-k_attractor * rel_dist)
        max_mag <- exp(-k_attractor * 0)
        min_mag <- exp(-k_attractor * 1)
        exp_scale <- (magnitude - min_mag) / (max_mag - min_mag)
        edge_sd <- drift_sd
        sd <- edge_sd - (edge_sd - center_sd) * exp_scale
      }
    }
    # Generate the signal using the final calculated SD
    signal <- rnorm(
      length(stored_signal),
      mean = stored_signal,
      sd = sd)
    
    # Return both the produced signal and the boolean status flag
    return(list(signal = clamp01(signal), in_trap = is_inside))
  }

  # RECOGNIZE A SIGNAL
  # The probability of correct guess refers to the probability of form-meaning mapping = the signal representing the semantic prototype
  clamp02 <- function(x) pmax(0, pmin(0.95, x))
  
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
  listener_guess_probability <- function(listener_guess, produced_signal, size_prototypes) {
    icon_ev <- signal_evidence(produced_signal, size_prototypes, k_perception = k_attractor, circle_radius = circle_radius)
    logits <- qlogis(clamp02(listener_guess + (iconicity_weight * icon_ev)))
    probs <- plogis(logits)
    
    return(list(probs = probs, evidence = icon_ev))
  }

  # UPDATE LEARNING AS DEPENDENT ON SUCCESS
  update_logit <- function(x, learning_strength, success, success_scale = 7.5, failure_scale = 1) {
    delta <- learning_strength * ifelse(success == 1, success_scale, failure_scale)
    plogis(qlogis(clamp02(x)) + delta)
  }
  
  # SETTING UP SIGNAL-MEANING INFORMATION
  # No of referents + their prototypical values; semantic prototypes; initial signal y values
  referents_info <- tibble(
    id = seq_len(n_referents),
    type = rep(c("small","large"), length.out = n_referents),
    size_prototypes = if_else(type == "small", 
                              list(c(0.15, 0.85)), 
                              list(c(0.85, 0.15))),
    agentA_stored_signal = rep(list(c(0.5, 0.5)), n_referents),
    agentB_stored_signal = rep(list(c(0.5, 0.5)), n_referents))

  simulation_log <- list()
  
  # MAIN SIMULATION LOOP
  for (sim in 1:n_sim) {
    
    for (gen in 1:n_generations) {
      trial_counter <- 0
      
      # expressive agent assignment
      expressive_A <- runif(1) < expressive_prob
      expressive_B <- runif(1) < expressive_prob
      
      # Initialize agents; reset representation strength at each generation
      agentA_guess <- rbeta(n_referents, 3, 9)
      agentB_guess <- rbeta(n_referents, 3, 9)
      
      for (round in 1:n_rounds) {
        # Shuffle referents so the order is different each round
        referent_order <- sample(1:n_referents)
        roles <- sample(rep(c("A", "B"), length.out = n_referents))
        
        for (trial in 1:n_referents) {
          # Increment global trial counter within generation (for expressive agent trials)
          trial_counter <- trial_counter + 1
          ref_id <- referent_order[trial]
          # speakers/listeners are taking turns
          speaker <- roles[trial]
          listener <- ifelse(speaker == "A", "B", "A")
          
          if (speaker == "A") {
            speaker_guess <- agentA_guess
            listener_guess <- agentB_guess
          } else {
            speaker_guess <- agentB_guess
            listener_guess <- agentA_guess
          }
          
          # Store guesses before trial updates
          old_guess_A <- agentA_guess[ref_id]
          old_guess_B <- agentB_guess[ref_id]
          old_stored_signal_A <- referents_info$agentA_stored_signal[[ref_id]]
          old_stored_signal_B <- referents_info$agentB_stored_signal[[ref_id]]
          
          old_stored_signal <- if (speaker == "A") old_stored_signal_A else old_stored_signal_B
          
          # Signal production (speaker knows lexeme & size)
          production_output <- produce_signal(
            stored_signal = old_stored_signal,
            speaker_guess[ref_id],
            drift_sd = drift_sd,
            k_production = 1.5,
            phonological_traps = phonological_traps,
            circle_radius = circle_radius,
            center_sd = trap_center_sd,
            k_attractor = k_attractor)
          
          signal  <- production_output$signal
          in_trap <- production_output$in_trap  # Extract the trap flag for logging

          # Override signal production if agent is expressive; on ~20% of all trials for that expressive agent
          is_expressive_trial <- FALSE
          if (
            ((speaker == "A" && expressive_A) ||
             (speaker == "B" && expressive_B)) &&
            runif(1) < 0.20
          ) {
            # If the trial is expressive, produce a perfectly iconic signal for that referent
            is_expressive_trial <- TRUE
            signal <- referents_info$size_prototypes[[ref_id]]
            signal <- clamp01(signal)
          }
          
          # Calculate recognition probability for the listener
          recognition <- listener_guess_probability(
            listener_guess[ref_id],
            signal,
            referents_info$size_prototypes[[ref_id]])
          
          prob <- recognition$probs
          
          # Success or failure
          success <- rbinom(1, 1, prob)
          
          # Update listener's guess for this referent as dependent on success or failure
          if (listener == "A") {
            agentA_guess[ref_id] <- update_logit(prob, learning_strength, success)
          } else {
            agentB_guess[ref_id] <- update_logit(prob, learning_strength, success)
          }
          
          # Update stored signal of both listener and speaker if success; integrate over stored and produced signals
          if (success == 1) {
            referents_info$agentA_stored_signal[[ref_id]] <- 
              (signal + referents_info$agentA_stored_signal[[ref_id]]) / 2
            referents_info$agentB_stored_signal[[ref_id]] <- 
              (signal + referents_info$agentB_stored_signal[[ref_id]]) / 2
          }
          
          
          # Store POST-UPDATE values
          new_guess_A <- agentA_guess[ref_id]
          new_guess_B <- agentB_guess[ref_id]
          new_stored_signal_A <- referents_info$agentA_stored_signal[[ref_id]]
          new_stored_signal_B <- referents_info$agentB_stored_signal[[ref_id]]
          
          
          # Log everything
          simulation_log[[length(simulation_log) + 1]] <- tibble(
            simulation = sim, generation = gen, round = round, trial = trial, trial_counter = trial_counter, referent = ref_id, speaker = speaker, listener = listener, 
            type = referents_info$type[ref_id], produced_signal = list(signal), in_trap = in_trap, prob = prob, evidence = recognition$evidence, success = success, expressive_A = expressive_A, 
            expressive_B = expressive_B, is_expressive_trial = is_expressive_trial, old_guess_A = old_guess_A, new_guess_A = new_guess_A, old_guess_B = old_guess_B, new_guess_B = new_guess_B,
            old_stored_signal_A = list(old_stored_signal_A), old_stored_signal_B = list(old_stored_signal_B), new_stored_signal_A = list(new_stored_signal_A), new_stored_signal_B = list(new_stored_signal_B)
          )
        }
      }
    }
    
    # Combine
    full_history <- bind_rows(simulation_log)
    
  }
  return(full_history)
}

# Call it
d.empty <- data.frame(
  sim = integer(), gen = integer(), round = integer(), trial = integer(),
  trial_counter = integer(), referent = integer(), 
  speaker = character(), listener = character(), type = character(),
  produced_signal = I(list()), in_trap = logical(), 
  old_stored_signal_A = I(list()), new_stored_signal_A = I(list()),
  old_stored_signal_B = I(list()), new_stored_signal_B = I(list()),
  prob = numeric(), success = integer(), evidence = numeric(),
  expressive_A = logical(), expressive_B = logical(),
  is_expressive_trial = logical(),
  old_guess_A = numeric(), new_guess_A = numeric(),
  old_guess_B = numeric(), new_guess_B = numeric(),
  stringsAsFactors = FALSE)

# Run simulation function
d.simulation <- rbind(
  d.empty %>% 
    run_interaction_sim(n_sim = 100, n_rounds = 10, n_generations = 10, phonological_attractors = FALSE, expressive_agents = FALSE) %>%
    mutate(model_type = "semanticAttractors"),
  d.empty %>% 
    run_interaction_sim(n_sim = 100, n_rounds = 10, n_generations = 10, phonological_attractors = FALSE, expressive_agents = TRUE) %>%
    mutate(model_type = "semanticAttractors_expressiveAgents"),
  d.empty %>% 
    run_interaction_sim(n_sim = 100, n_rounds = 10, n_generations = 10, phonological_attractors = TRUE, expressive_agents = FALSE) %>%
    mutate(model_type = "semanticPhonAttractors"),
  d.empty %>% 
    run_interaction_sim(n_sim = 100, n_rounds = 10, n_generations = 10, phonological_attractors = TRUE, expressive_agents = TRUE) %>%
    mutate(model_type = "allAttractors_expressiveAgents"))
  

write_csv(d.simulation, "temp_data/temp_data.csv")

# REVISIT SCALING TESTS --------------
# MOST TESTS generated by AI but checked
# prob of entering a pocket
# 1. The 2D Probability Helper
pocket_prob_2d <- function(drift_sd_base, 
                           n = 1e5, 
                           stored_signal = c(0.5, 0.5), 
                           target_center = c(0.15, 0.85), # Updated to match your prototypes
                           speaker_guess = 0.5, 
                           k_production = 1.5,
                           circle_radius = 0.3,
                           center_sd = 0.1,
                           k_attractor = 2.5,
                           attractor_type = c("semantic", "phonological")) {
  
  attractor_type <- match.arg(attractor_type)
  
  # 1. Evaluate the base variance for the trial context
  if (attractor_type == "semantic") {
    # Semantic respects the current conversational guess state
    sd_eff <- drift_sd_base * (1 + k_production * (0.5 - speaker_guess))
  } else {
    # Phonological ignores speaker guess at its outer boundary
    sd_eff <- drift_sd_base
  }
  
  # 2. Check if the starting position falls within the attractor radius
  dist_to_center <- sqrt(sum((stored_signal - target_center)^2))
  
  if (dist_to_center < circle_radius) {
    # Apply your updated exponential landscape compression
    rel_dist <- dist_to_center / circle_radius
    magnitude <- exp(-k_attractor * rel_dist)
    max_mag <- exp(-k_attractor * 0)
    min_mag <- exp(-k_attractor * 1)
    
    exp_scale <- (magnitude - min_mag) / (max_mag - min_mag)
    
    # Compress variance based on starting proximity
    sd_eff <- sd_eff - (sd_eff - center_sd) * exp_scale
  }
  
  # Generate 2D Gaussian noise using the context-aware variance
  noise_x <- rnorm(n, mean = stored_signal[1], sd = sd_eff)
  noise_y <- rnorm(n, mean = stored_signal[2], sd = sd_eff)
  
  # Calculate destination distances
  dists <- sqrt((noise_x - target_center[1])^2 + (noise_y - target_center[2])^2)
  
  return(mean(dists < circle_radius))
}

find_max_hit_rate_sd <- function(stored_signal = c(0.5, 0.5), 
                                 target_center = c(0.15, 0.85), 
                                 circle_radius = 0.3,
                                 speaker_guess = 0.5,
                                 k_production = 1.5,
                                 center_sd = 0.1,
                                 k_attractor = 2.5,
                                 attractor_type = c("semantic", "phonological")) { # Added toggle
  
  attractor_type <- match.arg(attractor_type)
  
  obj_fun <- function(sd_candidate) {
    pocket_prob_2d(
      drift_sd_base = sd_candidate,
      stored_signal = stored_signal,
      target_center = target_center,
      circle_radius = circle_radius,
      speaker_guess = speaker_guess,
      k_production = k_production,
      center_sd = center_sd,
      k_attractor = k_attractor,
      attractor_type = attractor_type, # Properly forwarded
      n = 50000 
    )
  }
  
  result <- optimize(obj_fun, interval = c(0.01, 1.0), maximum = TRUE)
  
  return(list(
    peak_sd = result$maximum,
    max_prob = result$objective
  ))
}

# verify Entry Probability from Neutral Center (0.5, 0.5)
# Checks if your current setup achieves the desired ~10% hit rate 
prob_entering_semantic <- pocket_prob_2d(
  drift_sd_base = 0.18, 
  stored_signal = c(0.5, 0.5), 
  target_center = c(0.15, 0.85), 
  speaker_guess = 0.5, 
  attractor_type = "semantic"
)
print(paste("Semantic Entry Probability:", round(prob_entering_semantic, 4)))

prob_entering_phonological <- pocket_prob_2d(
  drift_sd_base = 0.18, 
  stored_signal = c(0.5, 0.5), 
  target_center = c(0.15, 0.15), # Pointing to the phonological trap corner
  speaker_guess = 0.8,           # Even with a high guess, phonological entry is unaffected
  attractor_type = "phonological" # <--- Evaluates the phonological track rules
)
print(paste("Phonological Entry Probability:", round(prob_entering_phonological, 4)))

# Find peak variance settings for Semantic; maximum possible probability that a random production step will land inside the attractor from starting position
semantic_peaks <- find_max_hit_rate_sd(target_center = c(0.15, 0.85), attractor_type = "semantic")
# Find peak variance settings for Phonological; maximum possible probability that a random production step will land inside the attractor from starting position
phonological_peaks <- find_max_hit_rate_sd(target_center = c(0.15, 0.15), attractor_type = "phonological")

# check ceiling
res <- find_max_hit_rate_sd(attractor_type = "semantic")
print(res)
# if max_prob > 0.10, scan left side of peak to find 10% mark
sd_scans <- seq(0.01, res$peak_sd, length.out = 100)

probs <- sapply(sd_scans, function(sd_candidate) {
  pocket_prob_2d(drift_sd_base = sd_candidate, attractor_type = "semantic", n = 10000)
})

# Find the SD that gets closest to a 0.10 probability
closest_index <- which.min(abs(probs - 0.10))
exact_drift_sd <- sd_scans[closest_index]
print(paste("Use this value for drift_sd:", round(exact_drift_sd, 4)))

# find sd for probability of moving out of the attractor pocket
p_stay_2d <- function(center_sd = 0.1, 
                      n = 1e5, 
                      circle_radius = 0.3) {
  
  # When starting at the absolute center, rel_dist = 0.
  # Both landscapes compress fully to center_sd.
  y_x <- rnorm(n, mean = 0, sd = center_sd)
  y_y <- rnorm(n, mean = 0, sd = center_sd)
  
  dists <- sqrt(y_x^2 + y_y^2)
  
  # Returns probability of retention (1 - Escape Probability)
  return(mean(dists < circle_radius))
}
# check the Escape Probability from the Center Core
# If center_sd = 0.1 and radius = 0.3, what is the single-step escape rate?
prob_retained <- p_stay_2d(center_sd = 0.18, circle_radius = 0.3)
print(paste("Single-Step Escape Probability:", round(1 - prob_retained, 4)))

# Find the center_sd that gives you a low escape rate (e.g., ~5%)
# We scan candidate values for center_sd
center_candidates <- seq(0.01, 0.18, by = 0.01)
escapes <- sapply(center_candidates, function(csd) {
  1 - p_stay_2d(center_sd = csd, circle_radius = 0.3)
})

best_center_sd <- center_candidates[which.min(abs(escapes - 0.05))]
print(paste("To get a ~5% center escape rate, set trap_center_sd =", best_center_sd))

# Find the drift_sd that gives a 10% entry rate from (0.5, 0.5)
# Using the new best_center_sd we just found
sd_scans <- seq(0.05, 0.30, length.out = 100)
entry_probs <- sapply(sd_scans, function(dsd) {
  pocket_prob_2d(
    drift_sd_base = dsd, 
    stored_signal = c(0.5, 0.5), 
    target_center = c(0.15, 0.15), # Phonological trap corner
    circle_radius = 0.3,
    center_sd = best_center_sd,
    k_attractor = 2.5,
    attractor_type = "phonological",
    n = 20000
  )
})

best_drift_sd <- sd_scans[which.min(abs(entry_probs - 0.10))]
print(paste("To get a 10% entry rate from the center, set drift_sd =", round(best_drift_sd, 4)))


# CHECK iconicity scaling
simulate_diagnostic_weights <- function(trials, 
                                        is_iconic = FALSE, 
                                        icon_weight = 0.2,
                                        icon_ev = 1.8) {
  n_steps <- length(trials)
  p <- numeric(n_steps + 1)
  p[1] <- 0.25 
  
  # Calculate impact dynamically based on the weight argument
  icon_impact <- if (is_iconic) (icon_weight * icon_ev) else 0.0
  
  for (i in seq_len(n_steps)) {
    current_logit <- qlogis(p[i])
    
    step_success <- trials[i]
    trial_impact <- ifelse(step_success == 1, 0.364, 0.00) 
    
    p[i+1] <- plogis(current_logit + trial_impact + icon_impact)
  }
  return(p)
}

my_trials <- c(0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 1, 1, 1, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1) 

# Run them using an explicit true/false flag
neutral_res <- simulate_diagnostic(my_trials, is_iconic = FALSE)
iconic_res  <- simulate_diagnostic(my_trials, is_iconic = TRUE)

# Print the first 10 rows explicitly to check the tracking
df_test <- data.frame(
  Trial   = 0:40,
  Success = c(NA, my_trials),
  Neutral = round(neutral_res, 3),
  Iconic  = round(iconic_res, 3),
  Diff    = round(iconic_res - neutral_res, 3)
)

df_test

# find out actual icon_ev value
current_icon_ev <- signal_evidence( 
  produced_signal = c(0.15, 0.85), 
  center_target = c(0.15, 0.85),   
  k_perception = 2.5, 
  circle_radius = 0.3
)

# translate between weights and absolute Gains at p=0.5
calibrate_icon_weight <- function(target_absolute_gain = NULL, 
                                  current_weight = NULL, 
                                  icon_ev) {
  
  if (!is.null(target_absolute_gain)) {
    # Calculate required weight for a target gain (e.g., 0.10 for a 10% absolute increase)
    target_p <- 0.5 + target_absolute_gain
    required_logit <- qlogis(target_p)
    required_weight <- required_logit / icon_ev
    
    cat(paste0("To get a +", target_absolute_gain*100, "% absolute increase (moving 0.5 -> ", target_p, "):\n"))
    cat(paste("Set icon_weight =", round(required_weight, 4), "\n\n"))
  }
  
  if (!is.null(current_weight)) {
    # Calculate what absolute gain a specific weight gives you
    resulting_impact <- current_weight * icon_ev
    resulting_p <- plogis(resulting_impact)
    absolute_gain <- resulting_p - 0.5
    
    cat(paste0("With an icon_weight of ", current_weight, ":\n"))
    cat(paste0("The probability moves from 0.50 to ", round(resulting_p, 3), " (a +", round(absolute_gain*100, 2), "% absolute gain).\n"))
  }
}
calibrate_icon_weight(target_absolute_gain = 0.01, icon_ev = current_icon_ev)
calibrate_icon_weight(current_weight = 0.4, icon_ev = current_icon_ev)

# PLOT ATTRACTORS------
center_small <- c(0.15, 0.85)
center_large <- c(0.85, 0.15)
phonological_traps <- list(c(0.15, 0.15), c(0.85, 0.85))

trap_radius <- 0.3
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
    # A. Calculate semantic evidence for background fill
    ease = signal_evidence(c(x, y), 
                           if(type == "small") center_small else center_large,
                           circle_radius = 0.3, k_perception = k_attractor_fixed),
    # B. Calculate localized production SD for structural contours
    local_sd = {
      # baseline production variability (outside trap influence)
      base_sd <- drift_sd * (1 + k_production * (0.5 - speaker_guess_fixed))
      distances <- sapply(
        phonological_traps,
        function(center) sqrt(sum((c(x, y) - center)^2))
      )
      inside_traps <- distances < trap_radius
      if (any(inside_traps)) {
        closest_trap_dist <- min(distances[inside_traps])
        rel_dist <- closest_trap_dist / trap_radius

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

grid_mapped %>%
  ggplot(
    aes(x = x, y = y)) +
  geom_tile(aes(fill = ease)) +
  # Contour lines map out the dropping SD zones (the trap depth)
  geom_contour(aes(z = local_sd), bins = 10, color = "white", alpha = 0.7) +
  annotate("point", x = c(0.15, 0.85), y = c(0.15, 0.85), 
           color = "white", shape = 4, size = 4, stroke = 1.2) +
  facet_wrap(~ type) +
  scale_fill_viridis_c() +
  theme_minimal() +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1))

# PLOT SIGNAL SPACE----
# d.simulation <- read_csv("temp_data/temp_data.csv")
# readRDS("temp_data/temp_data.rds")
# load("temp_data/temp_data.rds")

# Signal space use across simulations
d_signal <- d.simulation %>%
  mutate(total_round = (generation - 1) * 10 + round,
         x = map_dbl(produced_signal, 1),
         y = map_dbl(produced_signal, 2)) %>%
  group_by(model_type, total_round, type, generation) %>%
  summarise(
    mean_x = mean(x, na.rm = TRUE),
    mean_y = mean(y, na.rm = TRUE),
    .groups = "drop")

# Signal space use within simulations
d_signal_sim <- d.simulation %>%
  mutate(total_round = (generation - 1) * 10 + round,
         x = map_dbl(produced_signal, 1),
         y = map_dbl(produced_signal, 2)) %>%
  group_by(model_type, total_round, type, generation, simulation) %>%
  summarise(
    mean_x = mean(x, na.rm = TRUE),
    mean_y = mean(y, na.rm = TRUE),
    .groups = "drop")

# PLOT ICONICITY----
d.iconicity <- d.simulation %>%
  mutate(model_type = factor(
    model_type, 
    levels = c("semanticAttractors", "semanticAttractors_expressiveAgents", "semanticPhonAttractors", "allAttractors_expressiveAgents"),
    labels = c("Semantic attractors", "Semantic attractors, expressive agents", "Semantic and phonological attractors", "Semantic and phonological attractors, expressive agents"),
    ordered = TRUE),
    total_round = (generation - 1) * 10 + round,
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
library(ggside)
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
  # Add lines at generational overturn
  geom_vline(xintercept = seq(0, 100, by = 10), 
             color = "grey", 
             lty = "dotted") +
  scale_color_viridis_c(begin = 0,
                        end = 1,
                        values = seq(0,1,0.1))+
  scale_y_continuous(limits = c(-1,1), breaks = seq(-1,1,0.25)) +
  scale_x_continuous(breaks = seq(0, 100, 10),
                     labels = seq(0, 10, 1)) +
  labs(title = "Iconicity over generations",
       y = "Iconicity\n(above 0 = iconic,\nbelow zero = anti-iconic)", x = "Generation (each with 10 rounds)") +
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


# TIMO EXPLORES ----
## plot 10 random simulations
### evidence
d.iconicity |> 
  filter(simulation %in% c(1:10)) |> 
  filter(model_type == "Semantic attractors") |> 
  ggplot(aes(x = total_round, y = evidence, group = interaction(model_type, simulation),
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
  scale_y_continuous(limits = c(-1,1), breaks = seq(-1,1,0.25)) +
  scale_x_continuous(breaks = seq(0, 100, 10),
                     labels = seq(0, 10, 1)) +
  labs(title = "Iconicity over generations",
       y = "Iconicity\n(above 0 = iconic,\nbelow zero = anti-iconic)", x = "Generation (each with 10 rounds)") +
  theme_minimal() +
  facet_wrap(simulation~model_type)

### guess rate
d.simulation |> 
  mutate(model_type = factor(
    model_type, 
    levels = c("semanticAttractors", "semanticAttractors_expressiveAgents", "semanticPhonAttractors", "allAttractors_expressiveAgents"),
    labels = c("Semantic attractors", "Semantic attractors, expressive agents", "Semantic and phonological attractors", "Semantic and phonological attractors, expressive agents"),
    ordered = TRUE),
    total_round = (generation - 1) * 10 + round,
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
  scale_x_continuous(breaks = seq(0, 100, 10),
                     labels = seq(0, 10, 1)) +
  labs(title = "Representational strength over generations",
       y = "Memory strength from 0-1\n", x = "Generation (each with 10 rounds)") +
  theme_minimal() +
  facet_wrap(~model_type, ncol = 1)

## looks like very low memory strength, check first repetion
d.iconicity |> 
  filter(total_round %in% c(1,11,21,31,41,51,61,71,81,91)) |> 
  summarise(strength = mean(strength))
