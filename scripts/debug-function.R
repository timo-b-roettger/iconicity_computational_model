# START OVER ----------------------------------------
# 1. Initiate a data set that contains the first settings needed for sim--referent info
# 2. Add produce signal + recognition + success/failure + learning + storing of data, all outside loop, for one trial only
# 3. Get loop to work
# 4. Adjust functions applied between generations
# 5. Add mechanisms and evaluate

# 1. INITIATE DATA SET - for one trial ----------------------------------------
n_referents = 4

# Referent info + initial speaker_guess and listener_guess to the dataframe + initial starting y-value for all refs
# guesses and stored y-value to be overwritten at each round
referents_info <- tibble(
  id = seq_len(n_referents),
  lexeme = LETTERS[1:n_referents],
  type = rep(c("small","large"), length.out = n_referents),
  lex_prototypes = seq(0, 1, length.out = n_referents),
  size_prototypes = ifelse(type == "small", 0, 1),
  speaker_guess = rbeta(n_referents, 3, 9),
  listener_guess = rbeta(n_referents, 3, 9),
  stored_y = rep(0.5, n_referents))

# Randomly selects a referent by their ID
r <- sample(1:n_referents, 1)
# Query the information for that referent from the referents_info dataframe
ref <- referents_info[r,]

# 2. ADD PRODUCE SIGNAL + RECOGNITION + SUCCESS/FAILURE + LEARNING + STORING ----------------------------------------
# PRODUCE A SIGNAL
drift_sd_x = 0.01
drift_sd_y = 0.05  # this should be the same at each generational overturn
clamp01 <- function(x) pmax(0, pmin(1, x))

# At signal production, amount of noise is dependent on guess rate
# k controls how strongly sd react to speaker_guess; mindful that if p = 1 and k >2, it will return negative sd's
produce_signal <- function(stored_y, lex_prototypes, size_prototypes, speaker_guess, k) {
  x <- rnorm(1, mean = lex_prototypes, sd = drift_sd_x)
  y <- rnorm(1, mean = stored_y,
             sd = drift_sd_y * (1 + k * (0.5 - speaker_guess)))
  c(x, clamp01(y)) ## I think X also needs to be clamped, as it otherwise also produces negative values with noise?
}

signal <- produce_signal(ref$stored_y, ref$lex_prototypes, ref$size_prototypes, ref$speaker_guess, k = .5)
produced_x <- signal[1]
produced_y <- signal[2]

# RECOGNIZE A SIGNAL
# The probability of correct guess refers to the probability of form-meaning mapping = the signal representing the semantic prototype
# Should it be dependent on previous guess?
clamp02 <- function(x) pmax(0, pmin(0.95, x))
iconicity_weight = 0.0004  # this should probably increase substantially if we want signals to stay there

# Signal evidence for iconicity bias
# Measures proximity of Y to its size prototype
signal_evidence <- function(produced_y, type, k=5) { # change k for steeper or more shallow decay
  target <-  referents_info$size_prototypes[r] # 0 or 1
  dist <- abs(produced_y - target)
  # For signals in the 'neutral' mid-part of the space, make evidence = 0; define boundaries for when evidence begin to matter
  if(dist >= 0.2 & dist <= 0.8) {
    evidence <- 0
  } else {
    # calculate distance from signal to boundaries
    edge_dist <- ifelse(dist < 0.2, dist - 0.8, 0.2 - dist)
    # calculate the magnitude of decay/boost, exponentially (same for boost and decay)
    magnitude <- exp(-k * edge_dist)
    # TR: normalize that magnitude to [0,1] manually, need to be done relatively to k later
    min_magnitude = 20.08554
    max_magnitude = 54.59815
    magnitude_norm <- ((magnitude - min_magnitude) / (max_magnitude - min_magnitude))
    
    # determine sign of evidence (boost or decay)
    if(dist > 0.8) {
      evidence <- -magnitude_norm
    } else {
      evidence <- magnitude_norm
    }
  }
  return(evidence)
}

# Listener recognition
lapse = 0.05

# LISTENER RECOGNITION PROBABILITY UPDATED ---lapse missing?
listener_guess_probability <- function(listener_guess, produced_y, type, size_prototypes) {
  # Iconicity bias
  icon_ev <- signal_evidence(produced_y, type)
  # Combine in logodds space
  logits <- qlogis(clamp02(listener_guess + (iconicity_weight * icon_ev)))
  # Return final probability
  probs <- plogis(logits)
  return(list(probs = probs, evidence = icon_ev))
}

probs <- listener_guess_probability(listener_guess, produced_y, referents_info)

# CALCULATE SUCCESS/FAILURE
success <- rbinom(1, 1, probs$probs)

# STORE OUTPUT INCREMENTALLY IN REFERENTS_INFO
# signals are only updated when recognition is successful
referents_info$stored_y[r] <- ifelse(success == 1, produced_y, referents_info$stored_y[r])
# learning carries over
referents_info$listener_guess[r] <- probs$probs

# UPDATE LEARNING AT GENERATIONAL OVERTURN
update_logit <- function(x, delta) {
  plogis(qlogis(clamp02(x)) + delta)
}

# 3. ADD LOOP OF 10 ROUNDS AND THEN GENERATIONAL OVERTURN ----------------------------------------
# Each interaction consists of 10 rounds. At each round, all referents are being communicated.
# The output of the final round is stored to be the input for the next generation. 
# Learning bias only applies between generations, while noise along the y-dimension is reset to the initial value of first generation.

# INTERACTION LOOP (within and across generations)
# Main interaction loop function
run_interaction_sim <- function(
    data,
    n_sim = 1, # number of simulations
    n_referents = 4, # number of unique referents in guessing game
    n_generations = 10, # no of generations
    n_rounds = 10, # number of interaction rounds; 10
    drift_sd_x = 0.01,   # tiny drift to simulate less than perfect production; motor noise
    drift_sd_y = 0.23,       # amount of variation introduced during production; motor noise; equivalent to approx. 10% chance of wandering into a pocket when the signal is at .5
    learning_strength = 0.015, # amount of added memory strengthening for referents as dependent on success; corresponding to .01 increase for a probability of 0.5 on failure, and .09 increase on success
    iconicity_weight = 0.04,  # multiplicator for iconicity; corresponding to 1% absolute increase for a probability of 0.5 (for the perfectly iconic signal)
    success_scale = 7.5, # more learning with success; 95% accuracy at the end of 10th round
    failure_scale = 1, # also increase in learning with failure, but less so; 60% accuracy at the end of 10th round
    lapse = 0.05 # soft lapse in probability space
) {
  
  # HELPER FUNCTIONS CALLED IN SIMULATION LOOP
  # SIGNAL PRODUCTION + MECHANISMS AFFECTING Y-DIMENSION
  clamp01 <- function(x) pmax(0, pmin(1, x))
  
  # At signal production, amount of noise is dependent on guess rate
  # k controls how strongly sd react to speaker_guess; mindful that if p = 1 and k >2, it will return negative sd's
  produce_signal <- function(stored_y, lex_prototypes, size_prototypes, speaker_guess, k) {
    x <- rnorm(1, mean = lex_prototypes, sd = drift_sd_x)
    y <- rnorm(1, mean = stored_y,
               sd = drift_sd_y * (1 + k * (0.5 - speaker_guess))) ## when prob = .5, sd = drift_sd_y, prob < .5, sd increase and vv
    c(x, clamp01(y)) ## I think X also needs to be clamped, as it otherwise also produces negative values with noise?
  }
  
  # RECOGNIZE A SIGNAL
  # The probability of correct guess refers to the probability of form-meaning mapping = the signal representing the semantic prototype
  # Should it be dependent on previous guess?
  clamp02 <- function(x) pmax(0, pmin(0.95, x))
  
  # Signal evidence for iconicity bias
  # Measures proximity of Y to its size prototype
   signal_evidence <- function(produced_y, target, k = 4) {
    dist <- abs(produced_y - target)
    
    if (dist >= 0.2 & dist <= 0.8) {
      evidence <- 0
    } else {
      edge_dist <- ifelse(dist < 0.2, dist - 0.8, 0.2 - dist)
      
      magnitude <- exp(-k * edge_dist)
      #calculate bounds dynamically based on k
      min_magnitude <- exp(-k * -0.6)
      max_magnitude <- exp(-k * -0.8)
      magnitude_norm <- (magnitude - min_magnitude) / (max_magnitude - min_magnitude)
      
      evidence <- ifelse(dist > 0.8, -magnitude_norm, magnitude_norm)
    }
    
    return(evidence)
   }

  # LISTENER RECOGNITION PROBABILITY UPDATED ---lapse missing?
  listener_guess_probability <- function(listener_guess, produced_y, type, size_prototypes) {
    # Iconicity bias
    icon_ev <- signal_evidence(produced_y, size_prototypes)
    # Combine in logodds space
    logits <- qlogis(clamp02(listener_guess + (iconicity_weight * icon_ev)))
    # Return final probability
    probs <- plogis(logits)
    return(list(probs = probs, evidence = icon_ev))
  }

  # UPDATE LEARNING AS DEPENDENT ON SUCCESS
  update_logit <- function(x, learning_strength, success, success_scale = 7.5, failure_scale = 1) {
    delta <- learning_strength * ifelse(success == 1, success_scale, failure_scale)
    plogis(qlogis(clamp02(x)) + delta)
  }
  
  # SETTING UP SIGNAL-MEANING INFORMATION
  # No of lexemes + their prototypical values; semantic prototypes; initial signal y values
  referents_info <- tibble(
    id = seq_len(n_referents),
    lexeme = LETTERS[1:n_referents],
    type = rep(c("small","large"), length.out = n_referents),
    lex_prototypes = seq(0, 1, length.out = n_referents),
    size_prototypes = ifelse(type == "small", 0, 1),
    agentA_stored_y = rep(0.5, n_referents),
    agentB_stored_y = rep(0.5, n_referents))
  
  simulation_log <- list()
  
  # MAIN SIMULATION LOOP
  for (sim in 1:n_sim) {
    
    for (gen in 1:n_generations) {
      
      # Initialize agents; reset representation strength at each generation
      agentA_guess <- rbeta(n_referents, 3, 9)
      agentB_guess <- rbeta(n_referents, 3, 9)
      
      for (round in 1:n_rounds) {
        # Shuffle referents so the order is different each round
        referent_order <- sample(1:n_referents)
        
        for (trial in 1:n_referents) {
          
          ref_id <- referent_order[trial]
          # speakers/listeners are taking turns
          if ((trial + round) %% 2 == 1) {
            speaker <- "A"
            listener <- "B"
            speaker_guess <- agentA_guess
            listener_guess <- agentB_guess
          } else {
            speaker <- "B"
            listener <- "A"
            speaker_guess <- agentB_guess
            listener_guess <- agentA_guess
          }
          
          # Store guesses before trial updates
          old_guess_A <- agentA_guess[ref_id]
          old_guess_B <- agentB_guess[ref_id]
          old_stored_y_A <- referents_info$agentA_stored_y[ref_id]
          old_stored_y_B <- referents_info$agentB_stored_y[ref_id]
          
          old_stored_y <- if (speaker == "A") old_stored_y_A else old_stored_y_B
          
          # Signal production (speaker knows lexeme & size)
          signal <- produce_signal(
            old_stored_y,
            referents_info$lex_prototypes[ref_id],
            referents_info$size_prototypes[ref_id],
            speaker_guess[ref_id],
            k = 0.5)
          produced_x <- signal[1]
          produced_y <- signal[2]
          
          # Calculate recognition probability for the listener
          recognition <- listener_guess_probability(
            listener_guess[ref_id],
            produced_y,
            referents_info$type[ref_id],
            referents_info$size_prototypes[ref_id])
          
          prob <- recognition$probs
          
          # Success or failure
          success <- rbinom(1, 1, prob)
          
          # Update listener's guess for this referent as dependent on success or failure
          if (listener == "A") {
            agentA_guess[ref_id] <- update_logit(prob, learning_strength, success)
          } else {
            agentB_guess[ref_id] <- update_logit(prob, learning_strength, success)
          }
          
          # Update stored_y of both listener and speaker if success; integrate over stored and produced y
          if (success == 1) {
            referents_info$agentA_stored_y[ref_id] <- 
              (produced_y + referents_info$agentA_stored_y[ref_id]) / 2
            referents_info$agentB_stored_y[ref_id] <- 
              (produced_y + referents_info$agentB_stored_y[ref_id]) / 2
          }
          
          # Store POST-UPDATE values
          new_guess_A <- agentA_guess[ref_id]
          new_guess_B <- agentB_guess[ref_id]
          new_stored_y_A <- referents_info$agentA_stored_y[ref_id]
          new_stored_y_B <- referents_info$agentB_stored_y[ref_id]
          
          
          # Log everything
          simulation_log[[length(simulation_log) + 1]] <- tibble(
            simulation = sim, generation = gen, round = round, trial = trial, referent = ref_id, speaker = speaker, listener = listener, 
            lexeme = referents_info$lexeme[ref_id], type = referents_info$type[ref_id], produced_x = produced_x, produced_y = produced_y,
            prob = prob, evidence = recognition$evidence, success = success,
            old_guess_A = old_guess_A, new_guess_A = new_guess_A, old_guess_B = old_guess_B, new_guess_B = new_guess_B,
            old_stored_y_A = old_stored_y_A, old_stored_y_B = old_stored_y_B, new_stored_y_A = new_stored_y_A, new_stored_y_B = new_stored_y_B
          )
        }
      }
    }
    
    # Combine
    full_history <- bind_rows(simulation_log)
    
  }
  return(full_history)
}

d.empty <- data.frame(
  sim = integer(), gen = integer(), round = integer(), trial = integer(), 
  referent = integer(), speaker = character(), listener = character(), type = character(), lexeme = character(),
  produced_x = numeric(), produced_y = numeric(), prob = integer(), success = integer(), evidence = integer(),
  old_guess_A = integer(), new_guess_A = integer(), old_guess_B = integer(), new_guess_B = integer(),
  old_stored_y = integer(), new_stored_y = integer(), stringsAsFactors = FALSE)

d.simulation <- d.empty %>% run_interaction_sim(n_sim = 10, n_rounds = 10, n_generations = 10, drift_sd_y = 0.23, iconicity_weight = 0.04, learning_strength = 0.015)

# Check whether the signal updates only on successes
test_signal_update <- function(df) {
  df %>%
    mutate(
      A_changed = new_stored_y_A != old_stored_y_A,
      B_changed = new_stored_y_B != old_stored_y_B
    ) %>%
    summarise(
      # violation should be 0 (no update on failure)
      A_violation = mean(A_changed & success == 0),
      B_violation = mean(B_changed & success == 0),
      # missed should be ~ 0 (updates happen on success)
      A_missed    = mean(!A_changed & success == 1),
      B_missed    = mean(!B_changed & success == 1)
    )
}

#signals should match as agents update signals identically; 0 = signals are synchronised, >0 = bug in shared update
test_signal_sync <- function(df) {
  df %>%
    summarise(
      mismatch = mean(abs(new_stored_y_A - new_stored_y_B) > 1e-10)
    )
}

# Check whether probabilities are updated after each trial
test_listener_update <- function(df) {
  df %>%
    mutate(
      A_changed = new_guess_A != old_guess_A,
      B_changed = new_guess_B != old_guess_B
    ) %>%
    summarise(
      # wrong = 0, speaker should not update
      A_wrong = mean(A_changed & listener != "A"),
      B_wrong = mean(B_changed & listener != "B"),
      # missed ~ 0 listener should update
      A_missed = mean(!A_changed & listener == "A"),
      B_missed = mean(!B_changed & listener == "B")
    )
}

# belief is carried over, ~1 = perfect carry over, <1 = memory bug
test_carryover <- function(df) {
  
  df_listener <- df %>%
    mutate(
      listener_old = if_else(listener == "A", old_guess_A, old_guess_B),
      listener_new = if_else(listener == "A", new_guess_A, new_guess_B)
    ) %>%
    arrange(simulation, generation, referent, round, trial)
  
  df_listener %>%
    group_by(simulation, generation, listener, referent) %>%
    mutate(
      prev_new = lag(listener_new),
      correct = abs(listener_old - prev_new) < 1e-10
    ) %>%
    summarise(
      carry_accuracy = mean(correct, na.rm = TRUE),
      .groups = "drop"
    )
}

run_all_tests <- function(df) {
  list(
    signal_update = test_signal_update(df),
    listener_update = test_listener_update(df),
    carryover = test_carryover(df),
    signal_sync = test_signal_sync(df)
  )
}

run_all_tests(d.simulation)

# 4. CHECK SCALING OF SD FUNCTION ----------------------------------------
# The sd of y at producing a signal is dependent on previous associative strength and previous signal
# Check the scaling of this sd; what is the probability of the signal walking out of the 'pocket' if the previous guess is low?
# That is, if your previous guess was a fail.
simulate_production <- function(n,
                     sd_base,
                     k = 1.5,
                     n_means = 20,
                     mean_fun = NULL,
                     associative_strength = seq(0, 1, length.out = 20)) {
  
  # generate means
  means <- if (is.null(mean_fun)) {
    seq(0.05, 1, length.out = n_means)
  } else {
    mean_fun(n_means)
  }
  
  # full parameter grid
  params <- expand_grid(
    sd_base = sd_base,
    mean = means,
    assoc = associative_strength
  ) %>%
    mutate(
      sd_eff = sd_base * (1 + k * (0.5 - assoc))
    )
  
  # simulate
  sims <- pmap(params, function(sd_base, mean, assoc, sd_eff) {
    tibble(
      value = clamp01(rnorm(n, mean = mean, sd = sd_eff)),
      sd_base = sd_base,
      sd_eff = sd_eff,
      mean = mean,
      assoc = assoc,
      run = seq_len(n)
    )
  })
  
  bind_rows(sims)
}


test.sd <- simulate_production(
  n = 100,
  sd_base = c(.1, .23, .3),
  k = 1.5,
  n_means = 40)


test.sd_long <- test.sd %>%
  rename(stored_y = mean) %>%
  mutate(
    case = case_when(
      stored_y >= 0.8 ~ "high",
      stored_y <= 0.2 ~ "low",
      TRUE ~ NA_character_)) %>%
  filter(!is.na(case)) %>%
  group_by(sd_base, sd_eff, stored_y, assoc, case) %>%
  summarise(
    probability = mean(
      if (first(case) == "high") value <= 0.8 else value >= 0.2),
    .groups = "drop")

p.sd.high <- test.sd_long %>% 
  filter(case == "high") %>%
  ggplot(aes(x = stored_y, y = assoc, fill = probability)) +
  geom_tile() +
  scale_x_continuous(limits = c(.79,1)) +
  facet_grid(case ~ sd_base, scales = "free_x") +
  scale_fill_viridis_c(limits = c(0,1))

p.sd.low <- p.sd.high %+%
  (test.sd_long %>% filter(case == "low")) +
  scale_x_continuous(limits = c(0,.21))

p.sd.high / p.sd.low


# CHECK SCALING OF LEARNING STRENGTH
# change function of learning scale so that it scales directly to update in logodds
# additive updates in log-odds space
# At probability 0.5 (log-odds = 0):
# success_scale = 0.3 increases probability to ~0.57
# failure_scale = 0.04 decreases probability to ~0.49
# update_logit <- function(x, success,
#                          success_scale = 0.3,
#                          failure_scale = 0.04) {
#   
#   delta <- ifelse(success == 1, success_scale, -failure_scale)
#   
#   plogis(qlogis(clamp02(x)) + delta)
# }

# helper function to check learning across 10 trials
simulate_learning <- function(trials,
                              n_paths = 8,
                              start = rbeta(n_paths, 3, 9),
                              success_scale = 7.5,
                              failure_scale = 1,
                              learning_strength = 0.015) {
  
  n_steps <- length(trials)
  
  p <- matrix(NA, nrow = n_steps + 1, ncol = n_paths)
  p[1, ] <- start
  
  for (i in seq_len(n_steps)) {
    
    success <- trials[i]
    
    p[i + 1, ] <- p[i, ] |> 
      vapply(function(x) {
        update_logit(
          x,
          success = success,
          success_scale = success_scale,
          failure_scale = failure_scale,
          learning_strength = learning_strength
        )
      }, numeric(1))
  }
  
  p
}
# For when there is 40 successful trials in a row
simulate_learning(rep(1, 40))

# CHECK iconicity scaling
simulate_vector_learning <- function(trials, 
                                     y_produced = 0.9, 
                                     icon_weight = 0.04, 
                                     k = 4) {
  
  n_steps <- length(trials)
  p <- numeric(n_steps + 1)
  p[1] <- 0.25 # Start probability (1/4 chance)
  
  # Calculate iconicity evidence once
  icon_ev <- signal_evidence(y_produced, target = 1, k = k)
  
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
  y_produced = 0.95, 
  icon_weight = 0.04, 
  k = 4
)

# Call for a Neutral word with the SAME trial history
neutral_results <- simulate_vector_learning(
  trials = my_trials, 
  y_produced = 0.5, 
  icon_weight = 0.04, 
  k = 4
)

# Compare the learning paths
data.frame(
  Trial = 0:40,
  Success = c(NA, my_trials),
  Neutral_P = round(neutral_results, 3),
  Iconic_P = round(iconic_results, 3),
  Difference = round(iconic_results - neutral_results, 3)
)

# 5. APPLY ADJUSTED FUNCTION ----------------------------------------
normalize_01 <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

# Measures proximity of Y to its size prototype
signal_evidence <- function(produced_y, target, k = 4) {
  dist <- abs(produced_y - target)
  
  if (dist >= 0.2 & dist <= 0.8) {
    evidence <- 0
  } else {
    edge_dist <- ifelse(dist < 0.2, dist - 0.8, 0.2 - dist)
    
    magnitude <- exp(-k * edge_dist)
    #calculate bounds dynamically based on k
    min_magnitude <- exp(-k * -0.6)
    max_magnitude <- exp(-k * -0.8)
    magnitude_norm <- (magnitude - min_magnitude) / (max_magnitude - min_magnitude)
    
    evidence <- ifelse(dist > 0.8, -magnitude_norm, magnitude_norm)
  }
  
  return(evidence)
}

# Calculate iconicity based on the current implementation of the model function
d.iconicity <- d.simulation %>%
  mutate(
    total_round = (generation - 1) * 10 + round,
    strength = abs(evidence)) %>%
  group_by(simulation, generation, total_round, type, lexeme) %>%
  summarise(
    evidence = mean(evidence),
    strength = mean(strength),
    .groups = "drop") %>%
  group_by(simulation, generation, total_round) %>%
  summarise(
    evidence = mean(evidence),
    strength = mean(strength),
    .groups = "drop")

# Aggregated
d.iconicity.mean <- d.iconicity |> 
  group_by(total_round) %>%
  summarise(evidence = mean(evidence),
            strength = mean(strength),
            .groups = "drop")

d.iconicity |> 
  ggplot(aes(x = total_round, y = evidence, group = simulation,
             color = evidence)) +
  geom_path(size = 0.5, alpha = 0.05,
            color = "black") +
  geom_path(data = d.iconicity.mean, 
            aes(group = 1), size = 2,
            color = "black") +
  geom_path(data =  d.iconicity.mean, 
            aes(group = 1), size = 1) +
  geom_vline(xintercept = seq(0, 100, by = 10), 
             color = "grey", 
             lty = "dotted") +
  #scale_color_viridis_c(limits = c(-1, 1)) +
  scale_color_viridis_c() +
  scale_y_continuous(limits = c(-1,1), breaks = seq(-1,1,0.25)) +
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  labs(title = "Iconicity over generations",
       y = "Iconicity\n(above 0 = iconic,\nbelow zero = anti-iconic)", x = "Generation (each with 10 rounds)") +
  theme_minimal()

ggplot(d.iconicity.mean, aes(x = total_round)) +
  geom_line(aes(y = direction), size = 1.2, color = "black") +
  geom_line(aes(y = strength), size = 1.2, linetype = "dashed") +
  labs(
    title = "Iconicity over generations",
    y = "Direction (solid) / Strength (dashed)",
    x = "Generation"
  ) +
  theme_minimal()

d.iconicity %>%
  ggplot(aes(x = total_round, y = evidence, group = simulation)) +
  geom_line(alpha = 0.05) +
  stat_summary(aes(group = 1), fun = mean, geom = "line", size = 1.5) +
  theme_minimal()

# Check learning
d.simulation_success <- d.simulation %>%
  select(simulation, generation, round, trial, speaker, listener, success, new_guess_A, new_guess_B, prob) %>%
  pivot_longer(
    cols = c(new_guess_A, new_guess_B),
    names_to = c("memory", "process", "agent"),
    values_to = "associative_strength",
    names_sep = "_") %>%
  select(-process) %>%
  # filter down to rows for which the guess pertains to the listener
  filter((listener == "A" & agent == "A") | (listener == "B" & agent == "B")) %>%
  #cumulative rounds
  mutate(total_round = (generation - 1) * 10 + round) %>%
  group_by(simulation, generation, total_round) %>%
  summarise(
    prob = mean(prob, na.rm = TRUE),
    success = mean(success, na.rm = TRUE),
    associative_strength = mean(associative_strength, na.rm = TRUE),
    .groups = "drop")

d.simulation_success  %>%
  ggplot(
    aes(x = total_round, y = associative_strength, group = interaction(simulation, generation))) +
  # Individual simulation paths
  geom_path(linewidth = 0.5, alpha = 0.1, color = "black") +
  # mean across all simulations
  geom_line(data = d.simulation_success |>
              group_by(total_round) |>
              summarise(associative_strength = mean(associative_strength), .groups = "drop"),
            aes(x = total_round, y = associative_strength),
            inherit.aes = FALSE,
            color = "black", 
            linewidth = 1) +
  # mean prob across all simulations
  geom_line(data = d.simulation_success |>
              group_by(total_round) |>
              summarise(prob = mean(prob), .groups = "drop"),
            aes(x = total_round, y = prob),
            inherit.aes = FALSE,
            color = "blue", 
            linewidth = 1) +
  labs(title = "Learning progress across generations",
       y = "Associative strength", 
       x = "Total rounds (cumulative)") +
  scale_y_continuous(limits = c(0, 1)) + 
  # breaks every 10 rounds to mark the start of a new generation
  scale_x_continuous(breaks = seq(0, 100, 10)) +  
  theme_minimal()

# Look at the signal space
d_signal <- d.simulation %>%
  mutate(total_round = (generation - 1) * 10 + round) %>%
  group_by(total_round, type, generation) |> 
  summarise(produced_y = mean(produced_y, na.rm = T),
            .groups = "drop") 

d_signal_sim <- d.simulation %>%
  mutate(total_round = (generation - 1) * 10 + round) %>%
  group_by(total_round, type, generation, simulation) |> 
  summarise(produced_y = mean(produced_y, na.rm = T),
            .groups = "drop") 

ggplot(d_signal,
       aes(x = total_round,
           y = produced_y,
           colour = type)) +
  geom_path(data = d_signal_sim,
            aes(group = type),
            size = 0.5,
            alpha = 0.2) +
  geom_path(aes(group = type),
            size = 2) +
  geom_hline(yintercept = c(0.2,0.8), lty = "dashed") +
  geom_vline(xintercept = seq(0, 100, by = 10), 
             color = "black", 
             lty = "dotted") +
  labs(title = "Evolution of produced signal space",
       x = "Total rounds (cumulative)",
       bins = "type", y = "y") +
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.2,0.5,0.8,1)) +
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  scale_color_viridis_d(begin = 0.1, end = 0.9) +
  theme_minimal() +
  theme(legend.position = "bottom")


## GRID search
compute_iconicity <- function(history, cutoff = 0.8) {
  
  d.iconicity <- history %>%
    mutate(total_round = (generation - 1) * 10 + round) %>%
    group_by(simulation, total_round) %>%
    summarise(
      evidence = mean(evidence),
      .groups = "drop"
    )
  
  # define late stage
  max_round <- max(d.iconicity$total_round)
  threshold <- max_round * cutoff
  
  # average evidence in late stage
  d.iconicity %>%
    filter(total_round >= threshold) %>%
    summarise(mean_iconicity = mean(evidence, na.rm = TRUE)) %>%
    pull(mean_iconicity)
}

# Generate a parameter grid to explore
d.param_grid <- expand.grid(
  iconicity_weight = seq(0, 0.2, length.out = 15),
  learning_strength = seq(0, 0.1, length.out = 15),
  drift_sd_y = seq(0.01, 0.6, length.out = 15))

# Prepare results container
d.grid_results <- d.param_grid %>%
  mutate(iconicity = NA_real_,
         history = vector("list", n()))

for (i in seq_len(nrow(d.param_grid))) {
  
  params <- d.param_grid[i, ]
  
  # empty structure for simulation log
  empty_df <- data.frame(
    sim = integer(), gen = integer(), round = integer(), trial = integer(), 
    referent = integer(), speaker = character(), listener = character(), type = character(), lexeme = character(),
    produced_x = numeric(), produced_y = numeric(), prob = integer(), success = integer(), evidence = integer(),
    old_guess_A = integer(), new_guess_A = integer(), old_guess_B = integer(), new_guess_B = integer(),
    old_stored_y = integer(), new_stored_y = integer(), stringsAsFactors = FALSE)
  
  # run simulation
  history <- run_interaction_sim(
    data = empty_df,
    n_sim = 10, 
    n_referents = 4,
    n_generations = 10,
    n_rounds = 10,
    drift_sd_x = 0.01,   
    drift_sd_y = params$drift_sd_y,
    learning_strength = params$learning_strength,
    iconicity_weight = params$iconicity_weight,
    success_scale = 7.5, 
    failure_scale = 1,
    lapse = 0.05)
  
  # store full dataframe
  d.grid_results$history[[i]] <- history
  
  # compute iconicity score
  d.grid_results$iconicity[i] <- compute_iconicity(history)
  
  message("Completed parameter set ", i, " of ", nrow(d.param_grid))
}

d.full.history <- d.grid_results %>%
  unnest(history)

d.grid_results %<>%
  mutate(across(where(is.numeric), ~ round(.x, digits = 3)))

p.grid.sd.set <- d.grid_results %>%
  filter(drift_sd_y == 0.221) %>%
  ggplot(
    aes(
      x = factor(learning_strength),
      y = factor(iconicity_weight),
      fill = iconicity)) +
  geom_tile() +
  geom_point(
    data = . %>%
      filter(iconicity == max(iconicity)),
    shape = 4) +
  scale_fill_viridis_c() +
  #facet_grid(`articulatory bias` ~ `corrective rate`, labeller = label_both) +
  theme_minimal() +
  guides(color = "none") +
  labs(x = "Learning strength", y = "Iconicity weighting", fill = "Iconicity")

p.grid.learning.set <- p.grid.sd.set %+%
  (d.grid_results %>%
     filter(learning_strength == 0.014)) +
  aes(x = factor(drift_sd_y)) +
  labs(x = "SD along y")

p.grid.iconicity.set <- p.grid.sd.set %+%
  (d.grid_results %>%
     filter(iconicity_weight == 0.043)) +
  aes(y = factor(drift_sd_y)) +
  labs(y = "SD along y")

p.grid.sd.set
p.grid.learning.set
p.grid.iconicity.set

