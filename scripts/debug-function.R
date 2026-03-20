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

# 3. ADD LOOP OF 10 ROUNDS AND THEN GENERATIONAL OVERTURN ----------------------------------------
# Each interaction consists of 10 rounds. At each round, all referents are being communicated.
# The output of the final round is stored to be the input for the next generation. 
# Learning bias only applies between generations, while noise along the y-dimension is reset to the initial value of first generation.

n_sim = 1
n_rounds = 10 # 10 rounds per generation
n_generations = 4 # no of generations
lapse = 0.05
drift_sd_x = 0.01
drift_sd_y = 0.05  # this should be the same at each generational overturn
clamp01 <- function(x) pmax(0, pmin(1, x))
clamp02 <- function(x) pmax(0, pmin(0.95, x))
iconicity_weight = 0.0004

# INTERACTION LOOP (within and across generations)
# Stored Y for each referent
referents_info <- tibble(
  id = seq_len(n_referents),
  lexeme = LETTERS[1:n_referents],
  type = rep(c("small","large"), length.out = n_referents),
  lex_prototypes = seq(0, 1, length.out = n_referents),
  size_prototypes = ifelse(type == "small", 0, 1),
  stored_y = rep(0.5, n_referents))

simulation_log <- list()

for (gen in 1:n_generations) {
  
  # Initialize agents; reset guessing rate at each generation
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
      old_stored_y <- referents_info$stored_y[ref_id]
      
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
      
      # Update listener's guess for this referent
      if (listener == "A") {
        agentA_guess[ref_id] <- prob
      } else {
        agentB_guess[ref_id] <- prob
      }
      
      # Success or failure
      success <- rbinom(1, 1, prob)
      
      # Update stored_y if success
      if (success == 1) {
        referents_info$stored_y[ref_id] <- produced_y
      }
      
      # Store POST-UPDATE values
      new_guess_A <- agentA_guess[ref_id]
      new_guess_B <- agentB_guess[ref_id]
      new_stored_y <- referents_info$stored_y[ref_id]
      
      # Log everything
      simulation_log[[length(simulation_log) + 1]] <- tibble(
        generation = gen, round = round, trial = trial, referent = ref_id, speaker = speaker, listener = listener, 
        lexeme = referents_info$lexeme[ref_id], type = referents_info$type[ref_id], produced_x = produced_x, produced_y = produced_y,
        prob = prob, evidence = recognition$evidence_val, success = success,
        old_guess_A = old_guess_A, new_guess_A = new_guess_A, old_guess_B = old_guess_B, new_guess_B = new_guess_B,
        old_stored_y = old_stored_y, new_stored_y = new_stored_y
      )
    }
  }
}

# Combine
full_history <- bind_rows(simulation_log)


# 4. ADJUST FUNCTIONS BETWEEN GENERATIONS ----------------------------------------
# LEARNING UPDATE -- only between generations (independent of success???)
learning_strength = 0.0004
#success_scale <- 1.2 # slightly more learning with success
#failure_scale <- 0.8 # also increase in learning with failure, but less so
#delta <- learning_strength * ifelse(success==1, success_scale, failure_scale)

referents_info$speaker_guess[r] <- plogis(qlogis(clamp02(probs + delta))) ## still no separate memories, so here speaker guess = current listener guess??
referents_info$listener_guess[r] <- plogis(qlogis(clamp02(probs + delta)))

# MAKE SURE SD_Y IS RESET AT INITIAL STAGE





