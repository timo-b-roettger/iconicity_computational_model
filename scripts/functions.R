# Constants ------------------------------------------------------------------

# Functions ------------------------------------------------------------------
normalize_01 <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}


## Iconicity simulation function ----------------------------------------
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
    failure_scale = 1 # also increase in learning with failure, but less so; 60% accuracy at the end of 10th round
) {
  
  # HELPER FUNCTIONS CALLED IN SIMULATION LOOP
  # SIGNAL PRODUCTION + MECHANISMS AFFECTING Y-DIMENSION
  clamp01 <- function(x) pmax(0, pmin(1, x))
  
  # At signal production, amount of noise is dependent on guess rate
  # k controls how strongly sd react to speaker_guess; mindful that if p = 1 and k >2, it will return negative sd's
  produce_signal <- function(stored_y, lex_prototypes, size_prototypes, speaker_guess, k_production) {
    x <- rnorm(1, mean = lex_prototypes, sd = drift_sd_x)
    y <- rnorm(1, mean = stored_y,
               sd = drift_sd_y * (1 + k_production * (0.5 - speaker_guess))) ## when prob = .5, sd = drift_sd_y, prob < .5, sd increase and vv
    c(x, clamp01(y)) ## I think X also needs to be clamped, as it otherwise also produces negative values with noise?
  }
  
  # RECOGNIZE A SIGNAL
  # The probability of correct guess refers to the probability of form-meaning mapping = the signal representing the semantic prototype
  clamp02 <- function(x) pmax(0, pmin(0.95, x))
  
  # Signal evidence for iconicity bias
  # Measures proximity of Y to its size prototype
  signal_evidence <- function(produced_y, target, k_perception = 5) {
    dist <- abs(produced_y - target)
    
    if (dist >= 0.2 & dist <= 0.8) {
      evidence <- 0
    } else {
      edge_dist <- ifelse(dist < 0.2, dist - 0.8, 0.2 - dist)
      
      magnitude <- exp(-k_perception * edge_dist)
      #calculate bounds dynamically based on k
      min_magnitude <- exp(-k_perception * -0.6)
      max_magnitude <- exp(-k_perception * -0.8)
      magnitude_norm <- (magnitude - min_magnitude) / (max_magnitude - min_magnitude)
      
      evidence <- ifelse(dist > 0.8, -magnitude_norm, magnitude_norm)
    }
    
    return(evidence)
  }
  
  # LISTENER RECOGNITION PROBABILITY UPDATED
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

      # expressive agent assignment--20% chance that there is an expressive agent in this generation
      expressive_agent <- if (runif(1) < 0.2) sample(c("A", "B"), 1) else NA
      # Total number of trials in this generation
      n_trials_gen <- n_rounds * n_referents
      # Pre-sample which trials will be expressive (20% of all trials)
      expressive_trials <- if (!is.na(expressive_agent)) {
        sample(1:n_trials_gen, size = ceiling(0.2 * n_trials_gen))
      } else {
        integer(0)
      }
      # Counter to track trial index within generation
      trial_counter <- 0
      
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
          old_stored_y_A <- referents_info$agentA_stored_y[ref_id]
          old_stored_y_B <- referents_info$agentB_stored_y[ref_id]
          
          old_stored_y <- if (speaker == "A") old_stored_y_A else old_stored_y_B
          
          # Signal production (speaker knows lexeme & size)
          signal <- produce_signal(
            old_stored_y,
            referents_info$lex_prototypes[ref_id],
            referents_info$size_prototypes[ref_id],
            speaker_guess[ref_id],
            k_production = 1.5)
          produced_x <- signal[1]
          produced_y <- signal[2]
          # Override y production if agent is expressive
          is_expressive_trial <- FALSE
          if (!is.na(expressive_agent) &&
              speaker == expressive_agent &&
              trial_counter %in% expressive_trials) {
            is_expressive_trial <- TRUE
            # Override y depending on referent type
            if (referents_info$type[ref_id] == "large") {
              produced_y <- 0.9
            } else {
              produced_y <- 0.1
            }
          }
          
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
            simulation = sim, generation = gen, round = round, trial = trial, trial_counter = trial_counter, referent = ref_id, speaker = speaker, listener = listener, 
            lexeme = referents_info$lexeme[ref_id], type = referents_info$type[ref_id], produced_x = produced_x, produced_y = produced_y,
            prob = prob, evidence = recognition$evidence, success = success, expressive_agent = expressive_agent, is_expressive_trial = is_expressive_trial,
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


## Iconicity simulation function with 4 params ----------------------------------------
# Main interaction loop function
run_interaction_sim_4params <- function(
    data,
    n_sim = 1, # number of simulations
    n_referents = 4, # number of unique referents in guessing game
    n_rounds = 100, # number of interaction rounds
    drift_sd_x = 0.01,   # tiny drift to simulate less than perfect production; motor noise
    drift_sd_y = 0.05,       # amount of variation introduced during production; motor noise 
    learning_strength = 0.0004, # amount of added memory strengthening for words per round; corresponding to 0.1 % increase for a probability of 0.5
    iconicity_weight = 0.0004,  # multiplicator for iconicity; corresponding to 0.1 % increase for a probability of 0.5
    articulatory_production_bias = 0.15, # baseline production bias toward prototype
    corrective_rate = 0.01, # how much stored signal moves toward prototype on failure;
    lapse = 0.05 # soft lapse in probability space
) {
  
  # assign input data frame to history internally
  history <- data
  #clamp to [0,1] for signal
  clamp01 <- function(x) pmax(0, pmin(1, x))
  #clamp to [0,0.95] for accuracy
  clamp02 <- function(x) pmax(0, pmin(0.95, x))
  
  
  # SETTING UP SIGNAL-MEANING INFORMATION
  # Semantic prototypes
  size_prototypes <- c(small = 0, large = 1)
  
  # Define no of lexemes in language + their prototypical values
  lexemes <- LETTERS[1:n_referents]
  lex_prototypes <- seq(0, 1, length.out = n_referents) # Prototype positions
  names(lex_prototypes) <- lexemes
  
  # Referent info: lexeme + size
  types <- rep(c("small","large"), length.out = n_referents)
  referents_info <- tibble(
    id = seq_len(n_referents),
    lexeme = lexemes,
    type = types)
  
  # HELPER FUNCTIONS CALLED IN SIMULATION LOOP
  # SIGNAL PRODUCTION + MECHANISMS AFFECTING Y-DIMENSION
  # Produce a token (produced signal) given stored signal and speaker skill (previous associative strength in probability space); 
  # x = fixed lexeme + tiny noise
  # y = stored y + articulatory bias
  produce_signal <- function(stored_y, lexeme, referent_type, speaker_guess) {
    x <- rnorm(1, mean = lex_prototypes[lexeme], sd = drift_sd_x)
    bias_strength <- articulatory_production_bias * (1 - speaker_guess)
    target_y <- size_prototypes[referent_type]
    y <- rnorm(1, mean = (1 - bias_strength) * stored_y + bias_strength * target_y,
               sd = drift_sd_y)
    # Return produced signal [x,y]
    c(x, clamp01(y))
  }
  
  # Adaptive communicative bias; when failure occurs, nudge stored signal toward prototype along y
  correct_Y <- function(stored_y, referent_type) {
    target_y <- size_prototypes[referent_type]
    clamp01(
      stored_y + corrective_rate * (target_y - stored_y) +
        rnorm(1, 0, drift_sd_y/2))
  }
  
  # Signal evidence for iconicity bias
  # Measures proximity of Y to its size prototype
  signal_evidence <- function(signal_y, referent_type, k=5) { # change k for steeper or more shallow decay
    # TR changed this and it broke
    target <-  size_prototypes[referent_type] # 0 or 1
    dist <- abs(signal_y - target)
    # exponential decay for stronger effect near or far away from target
    # TR: this function is the problem in the way it scales IMO
    # evidence <- exp(-k * dist)
    # TR: alternative linear for now, flip distance to reflect iconicity and center on 0
    #flip <- abs(dist - 1)
    #evidence <- flip - 0.5
    # rescale to [-1,1] so that far-away signals are punished while close signals are rewarded
    #2 * evidence - 1
    # For signals in the 'neutral' mid-part of the space, make evidence = 0; define boundaries for when evidence begin to matter
    if(dist >= 0.2 & dist <= 0.8) {
      evidence <- 0
      } else {
      # calculate distance from signal to boundaries
      # TR: You are calculating the distance to the 0.2 and 0.8 y value here, which basically makes more iconic patterns (e.g. 0.9) less boosted then 0.8  
      #edge_dist <- ifelse(dist < 0.2, 0.2 - dist, dist - 0.8)
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

  ## LISTENER INFERENCE
  # Listener inference in log-odds space
  listener_guess_probability <- function(agent_guess_vec, signal_x, signal_y, referents_info) {
    n <- nrow(referents_info)
    
    # Scalable perceptual noise (moderate overlap between neighbouring categories)
    delta_x <- mean(diff(sort(lex_prototypes))) # spacing between prototypes
    sigma_x <- 0.5 * delta_x # moderate overlap
    
    # Lexical likelihood P(lexeme | perceived X); loop over all referents and calculate the likelihood of each under the signal
    # lex_likelihood <- sapply(1:n, function(k) {
    #   mu <- lex_prototypes[ referents_info$lexeme[k] ]
    #   exp(- (signal_x - mu)^2 / (2 * sigma_x^2))
    # })
    
    # Iconicity bias
    icon_ev <- sapply(1:n, function(k) {
      # changed this and it broke
      signal_evidence(signal_y, referents_info$type[k])
    })
      
    # Combine in logodds space; lexical likelihood, the learned associative strength between lexeme and referent (agent_guess_vec), and iconicity
    #logits <- log(lex_likelihood + 1e-12) + qlogis(agent_guess_vec) + (iconicity_weight * icon_ev)
    # comment TR: likelihood is a probability so log(likelihood) is not in logit space
    # comment TR: but I also still not sure what lex-likelihood does here and why
    # comment TR: we need to clamp02 it here (clamp adjusted to 0.95 max)
    logits <- qlogis(clamp02(agent_guess_vec + (iconicity_weight * icon_ev)))
    
    # final output in probability space for success/failure calculation and storing of speaker listener guess
    probs <- plogis(logits)
    # apply lapse
    #probs <- (lapse/n) + (1 - lapse) * probs
    probs
  }

  # MAIN SIMULATION LOOP
  for (sim in 1:n_sim) {
    
    # Stored Y for each referent
    stored_y <- rep(0.5, n_referents)
    
    # initial learning status of referents (after training)
    ## each agent has initial probability ~ 0.3 ± noise
    agentA_guess <- rbeta(n_referents, 3, 9)
    agentB_guess <- rbeta(n_referents, 3, 9)
    
    for (t in 1:n_rounds) {
      # speakers/listeners are taking turns
      if (t %% 2 == 1) {
        speaker <- "A"; listener <- "B"
        speaker_guess <- agentA_guess; listener_guess <- agentB_guess
      } else {
        speaker <- "B"; listener <- "A"
        speaker_guess <- agentB_guess; listener_guess <- agentA_guess
      }
      # randomly pick one referent
      r <- sample(1:n_referents, 1)
      ref <- referents_info[r,]
      
      # Production (speaker knows lexeme & size)
      sig <- produce_signal(stored_y[r], ref$lexeme, ref$type, speaker_guess[r])
      x_prod <- sig[1]
      y_prod <- sig[2]
      
      # TR add evidence as variable into loop
      evidence <- signal_evidence(y_prod, referents_info$type[r][[1]], k=5)
      
      # Listener inference (x+y)
      probs <- listener_guess_probability(listener_guess, x_prod, y_prod, referents_info)
      
      # actual outcome, binomial sampling
      success <- rbinom(1,1,probs[r])
      
      # Learning in lexical mapping depends on success/failure
      success_scale <- 1.2 # slightly more learning with success
      failure_scale <- 0.8 # also increase in learning with failure, but less so
      delta <- learning_strength * ifelse(success==1, success_scale, failure_scale)
      # learning: speaker improves guess rate, in logodds space, but final output in probability space
      # TR here we disconnected iconicity from p_guess, iconicity affected success rate, success rate affects p_guess, 
      # TR now iconicity affects success but also p_guess directly which can be additionally boosted by learning strength
      #speaker_guess[r] <- plogis(qlogis(speaker_guess[r]) + delta)
      speaker_guess[r] <- plogis(qlogis(clamp02(probs[r] + delta)))
      # listener also learns due to feedback, in logodds space, but final output in probability space
      #listener_guess[r] <- plogis(qlogis(listener_guess[r]) + delta)
      listener_guess[r] <- plogis(qlogis(clamp02(probs[r] + delta)))
      
      #Adaptive communicative bias in Y, signal memory updates: success -> keep stored signal identical to produced signal, failure -> nudge toward prototype
      if(success==1) {
        stored_y[r] <- y_prod
      } else {
        stored_y[r] <- correct_Y(stored_y[r], ref$type)
      }
      
      # Update agent states based on learning
      if (speaker == "A") {
        agentA_guess <- speaker_guess
        agentB_guess <- listener_guess
      } else {
        agentB_guess <- speaker_guess
        agentA_guess <- listener_guess
      }
      
      # Log trials
      history <- rbind(history, data.frame(
        sim = sim, 
        round = t,
        referent = r, 
        lexeme = ref$lexeme, 
        type = ref$type,
        speaker = speaker, 
        listener = listener,
        produced_x = x_prod, 
        # remove the indexing
        produced_y = y_prod,
        stored_y = stored_y,
        p_guess = probs, 
        success = success,
        # add evidence
        evidence = evidence
      ))
    }
  }
  
  return(history)
}


## Grid search functions----------------------------------------

# Code for generating grid search in the parameter space
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

