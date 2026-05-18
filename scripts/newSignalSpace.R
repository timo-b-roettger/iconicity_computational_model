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
signal_evidence <- function(produced_signal, center_target, k_perception = 2.5, circle_radius = 0.35) {
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
    drift_sd = 0.18,       # amount of variation introduced during production; motor noise; equivalent to approx. 10% chance of wandering into a pocket when the signal is at .5
    learning_strength = 0.015, # amount of added memory strengthening for referents as dependent on success; corresponding to .01 increase for a probability of 0.5 on failure, and .09 increase on success
    iconicity_weight = 0.04,  # multiplicator for iconicity; corresponding to 1% absolute increase for a probability of 0.5 (for the perfectly iconic signal)
    success_scale = 7.5, # more learning with success; 95% accuracy at the end of 10th round
    failure_scale = 1, # also increase in learning with failure, but less so; 60% accuracy at the end of 10th round
    phonological_traps = list(c(0.15, 0.15), c(0.85, 0.85)), # define phonological attractors/traps; currently two, in the opposite corners of the two-dimensional space
    circle_radius = 0.35, # atm, identical size of both types of attractors
    trap_center_sd = 0.1  # Locked 5% single-step escape probability from center of phonological attractor
) {
  
  # HELPER FUNCTIONS CALLED IN SIMULATION LOOP
  # SIGNAL PRODUCTION + MECHANISMS AFFECTING Y-DIMENSION
  clamp01 <- function(x) pmax(0, pmin(1, x))

  # At signal production, amount of noise is dependent on guess rate AND phonological traps
  # k controls how strongly sd react to speaker_guess; mindful that if p = 1 and k >2, it will return negative sd's
  produce_signal <- function(stored_signal, speaker_guess, drift_sd, k_production, phonological_traps, circle_radius, center_sd) {
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
        
        # Scale SD linearly based on proximity to that trap's center from speaker's current noise (at border, rel_dist = 1; at attractor center, rel_dist = 0) 
        # under target_alpha (fixed escape probability if signal at trap center)
        # SD at entering trap
        edge_sd <- drift_sd
        sd <- center_sd + (edge_sd - center_sd) * rel_dist
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
  signal_evidence <- function(produced_signal, center_target, k_perception = 2.5, circle_radius) {
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
    icon_ev <- signal_evidence(produced_signal, size_prototypes, circle_radius = circle_radius)
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
      
      # expressive agent assignment--10% chance that an agent is expressive; ~20% chance of an expressive agent in this generation
      expressive_A <- runif(1) < 0.10
      expressive_B <- runif(1) < 0.10
      
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
            center_sd = trap_center_sd)
          
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
d.simulation <- d.empty %>% 
  run_interaction_sim(n_sim = 10, n_rounds = 10, n_generations = 10, drift_sd = 0.18, iconicity_weight = 0.4, learning_strength = 0.015)

# REVISIT SCALING TESTS --------------
# prob of entering a pocket
# 1. The 2D Probability Helper
pocket_prob_2d <- function(drift_sd_base, 
                           n = 1e5, 
                           stored_signal = c(0.5, 0.5), 
                           target_center = c(0.2, 0.8), 
                           speaker_guess = 0.5, 
                           k_production = 1.5,
                           circle_radius = 0.35) {
  
  sd_eff <- drift_sd_base * (1 + k_production * (0.5 - speaker_guess))
  
  # Generate 2D Gaussian noise
  noise_x <- rnorm(n, mean = stored_signal[1], sd = sd_eff)
  noise_y <- rnorm(n, mean = stored_signal[2], sd = sd_eff)
  
  # Euclidean distance to attractor center
  dists <- sqrt((noise_x - target_center[1])^2 + (noise_y - target_center[2])^2)
  
  # Return proportion of signals that landed inside the circle
  return(mean(dists < circle_radius))
}

find_max_hit_rate_sd <- function(stored_signal = c(0.5, 0.5), 
                                 target_center = c(0.2, 0.8), 
                                 circle_radius = 0.35,
                                 speaker_guess = 0.3,
                                 k_production = 1.5) {
  
  # The function to maximize
  obj_fun <- function(sd_candidate) {
    pocket_prob_2d(
      drift_sd_base = sd_candidate,
      stored_signal = stored_signal,
      target_center = target_center,
      circle_radius = circle_radius,
      speaker_guess = speaker_guess,
      k_production = k_production,
      n = 50000 # Higher N for a smoother peak
    )
  }
  
  # optimize() finds the maximum between two bounds
  result <- optimize(obj_fun, interval = c(0.01, 1.0), maximum = TRUE)
  
  return(list(
    peak_sd = result$maximum,
    max_prob = result$objective
  ))
}

# RUN IT
peak_results <- find_max_hit_rate_sd()
print(peak_results)


p_stay_2d <- function(drift_sd_base, 
                      n = 1e5, 
                      target_center = c(0.2, 0.8), 
                      speaker_guess = 0.8, # Higher guess = lower noise
                      k_production = 1.5,
                      circle_radius = 0.35) {
  
  sd_eff <- drift_sd_base * (1 + k_production * (0.5 - speaker_guess))
  
  # Start at the perfect center
  y_x <- rnorm(n, mean = target_center[1], sd = sd_eff)
  y_y <- rnorm(n, mean = target_center[2], sd = sd_eff)
  
  dists <- sqrt((y_x - target_center[1])^2 + (y_y - target_center[2])^2)
  
  # Probability of staying inside the attractor
  mean(dists < circle_radius)
}


# CHECK iconicity scaling
simulate_vector_learning <- function(trials,
                                     icon_weight = 0.04, 
                                     k_perception = 2.5) {
  
  n_steps <- length(trials)
  p <- numeric(n_steps + 1)
  p[1] <- 0.25 # Start probability (1/4 chance)
  
  # Calculate iconicity evidence once
  icon_ev <- signal_evidence( 
    produced_signal = c(0.5, 0.5), # A "decent" but not perfect signal
    center_target = c(0.2, 0.8),   # The actual attractor center
    k_perception = 2.5, 
    circle_radius = 0.35)
  
  # Your specific calibrations (at p=0.5)
  # Success: +0.364 log-odds (~ +9% gain)
  # Failure: +0.040 log-odds (~ +1% gain)
  
  for (i in seq_len(n_steps)) {
    current_logit <- qlogis(p[i])
    
    # Check if this specific trial was a success or failure
    step_success <- trials[i]
    trial_impact <- ifelse(step_success == 1, 0.364, 0.04)
    
    # Apply the iconicity boost for this trial
    icon_impact <- icon_weight * icon_ev
    
    p[i+1] <- plogis(current_logit + trial_impact + icon_impact)
  }
  
  return(p)
}

# Define a history of 40 trials (e.g., struggling at first, then succeeding)
my_trials <- c(0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 1, 1, 1, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1) 

# Call for an Iconic word
iconic_results <- simulate_vector_learning(
  trials = my_trials,
  icon_weight = 0.04, 
  k_perception = 2.5
)

# Call for a Neutral word with the SAME trial history
neutral_results <- simulate_vector_learning(
  trials = my_trials, 
  icon_weight = 0.04, 
  k_perception = 2.5
)

# Compare the learning paths
data.frame(
  Trial = 0:40,
  Success = c(NA, my_trials),
  Neutral_P = round(neutral_results, 3),
  Iconic_P = round(iconic_results, 3),
  Difference = round(iconic_results - neutral_results, 3))


# find trap sd 
calibrate_trap_sd <- function(circle_radius, p_escape = 0.05) {
  q <- qchisq(1 - p_escape, df = 2)
  sd <- circle_radius / sqrt(q)
  return(sd)
}

calibrate_trap_sd(0.25, 0.05)

# Plot iconicity


# PLOT ATTRACTORS------
center_small <- c(0.15, 0.85)
center_large <- c(0.85, 0.15)
phonological_traps <- list(c(0.15, 0.15), c(0.85, 0.85))

trap_radius <- 0.25
drift_sd <- 0.18
k_production <- 1.5
speaker_guess_fixed <- 0.5
center_sd <- 0.1

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
                           circle_radius = 0.4, k_perception = 5),
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
        # TRAP MODEL (absolute interpolation, not multiplicative scaling)
        edge_sd <- base_sd
        base_sd <- center_sd + (edge_sd - center_sd) * rel_dist
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
  #geom_contour(aes(z = local_sd), color = "white", alpha = 0.6, bins = 8) +
  geom_contour(aes(z = log(local_sd)), bins = 10, color = "white") +
  annotate("point", x = c(0.15, 0.85), y = c(0.15, 0.85), 
           color = "white", shape = 4, size = 4, stroke = 1.2) +
  facet_wrap(~ type) +
  scale_fill_viridis_c() +
  theme_minimal() +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1))

# PLOT SIGNAL SPACE----


