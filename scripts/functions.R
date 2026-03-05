# Constants ------------------------------------------------------------------


# Functions ------------------------------------------------------------------
## Iconicity simulation function ----------------------------------------
# Main interaction loop function
run_interaction_sim <- function(
    data,
    n_sim = 1, # number of simulations
    n_referents = 4, # number of unique referents in guessing game
    n_rounds = 100, # number of interaction rounds
    drift_sd_x = 0.01,   # tiny drift to simulate less than perfect production; motor noise
    drift_sd_y = 0.05,       # amount of variation introduced during production; motor noise 
    learning_strength = 0.04, # amount of added memory strengthening for words per round; corresponding to an 0.01 increase for a probability of 0.5
    iconicity_weight = 0.5,  # multiplicator for iconicity
    articulatory_production_bias = 0.15, # baseline production bias toward prototype
    reinforcement_rate = 0.05, # how much stored signals move toward produced signal on success
    corrective_rate = 0.03, # how much stored signal moves toward prototype on failure
    lapse = 0.05 # soft lapse in probability space
) {
  
  # assign input data frame to history internally
  history <- data
  #clamp to [0,1] for signal
  clamp01 <- function(x) pmax(0, pmin(0.95, x))
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
  
  # Adaptive communicative bias
  # reinforce stored signal toward produced signal after success
  reinforce_Y <- function(stored_y, produced_y) {
    ((1 - reinforcement_rate) * stored_y + reinforcement_rate * produced_y)
  }
  # when failure occurs, nudge stored signal toward prototype
  correct_Y <- function(stored_y, referent_type) {
    target_y <- size_prototypes[referent_type]
    clamp01(
      stored_y + corrective_rate * (target_y - stored_y) +
        rnorm(1, 0, drift_sd_y/2))
  }
  
  # Signal evidence for iconicity bias
  # Measures proximity of Y to its size prototype
  signal_evidence <- function(signal_y, referent_type, k=5) { # change k for steeper or more shallow decay
    target <- size_prototypes[referent_type]   # 0 or 1
    dist <- abs(signal_y - target)
    # exponential decay for stronger effect near or far away from target
    evidence <- exp(-k * dist)
    # rescale to [-1,1] so that far-away signals are punished while close signals are rewarded
    # TR comment: but this does not rescale to [-1,1], it ranges from 0.007 to -0.98
    2 * evidence - 1
  }
  
  ## LISTENER INFERENCE
  # Listener inference in log-odds space
  listener_guess_probability <- function(agent_guess_vec, signal_x, signal_y, referents_info) {
    n <- nrow(referents_info)
    
    # Scalable perceptual noise (moderate overlap between neighbouring categories)
    delta_x <- mean(diff(sort(lex_prototypes))) # spacing between prototypes
    sigma_x <- 0.5 * delta_x # moderate overlap
    
    # Lexical likelihood P(lexeme | perceived X); loop over all referents and calculate the likelihood of each under the signal
    lex_likelihood <- sapply(1:n, function(k) {
      mu <- lex_prototypes[ referents_info$lexeme[k] ]
      exp(- (signal_x - mu)^2 / (2 * sigma_x^2))
    })
    
    # Iconicity bias
    icon_ev <- sapply(1:n, function(k) {
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
      
      # Listener inference (x+y)
      probs <- listener_guess_probability(listener_guess, x_prod, y_prod, referents_info)
      
      # actual outcome, binomial sampling
      success <- rbinom(1,1,probs[r])
      
      # Learning in lexical mapping depends on success/failure
      success_scale <- 1.2 # slightly more learning with success
      failure_scale <- 0.8 # also increase in learning with failure, but less so
      delta <- learning_strength * ifelse(success==1, success_scale, failure_scale)
      # learning: speaker improves guess rate, in logodds space, but final output in probability space
      speaker_guess[r] <- plogis(qlogis(speaker_guess[r]) + delta)
      # listener also learns due to feedback, in logodds space, but final output in probability space
      listener_guess[r] <- plogis(qlogis(listener_guess[r]) + delta)
      
      # Adaptive communicative bias in Y, signal memory updates: success -> reinforce toward produced form; failure -> nudge toward prototype
      if(success==1) {
        stored_y[r] <- reinforce_Y(stored_y[r], y_prod)
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
        sim = sim, round = t,
        referent = r, lexeme = ref$lexeme, type = ref$type,
        speaker = speaker, listener = listener,
        produced_x = x_prod, produced_y = y_prod,
        stored_y = stored_y[r],
        p_guess = probs[r], success = success
      ))
    }
  }
  
  return(history)
}


## Grid search functions----------------------------------------

# Code for generating grid search in the parameter space
compute_iconicity <- function(history, n_bins = 20, cutoff = 0.8) {
  
  # bin rounds
  hist_agg <- history %>%
    mutate(bins = cut(round, breaks = n_bins, labels = FALSE)) %>%
    group_by(bins, type, sim) %>%
    summarise(
      y = mean(stored_y),
      .groups = "drop"
    )
  # compute distance effect
  hist_icon <- hist_agg %>%
    mutate(
      target_y = ifelse(type == "small", 0, 1),
      dist      = abs(y - target_y),
      iconicity = exp(-2 * dist)
    ) %>%
    group_by(bins, sim) %>%
    summarise(iconicity = mean(iconicity), .groups = "drop")
  # final bins threshold
  last_bin <- max(hist_icon$bins)
  threshold <- last_bin * cutoff
  # return mean iconicity in final 20% of bins
  mean(hist_icon$iconicity[hist_icon$bins >= threshold])
}


# Cut-outs -------------------------------------------------------------------------------------

# Main interaction loop function
run_interaction_sim_old <- function(
  data,
  n_sim = 10, # number of simulations
  n_referents = 6, # number of unique referents in guessing game
  n_small = n_referents/2, # number of small referents
  n_large = n_referents/2, # number of large referents
  n_rounds = 1000, # number of interaction rounds
  drift_sd = 0.05, # amount of variation introduced during production
  learning_strength = 0.005, # amount of added memory strengthening for words per round
  prototype_weight = 0.2, # multiplicator for distance to iconic prototypes
  articulatory_production_bias = 0.15, # baseline production bias toward prototype
  reinforcement_rate = 0.05, #how strongly stored signals move toward produced signal on success
  corrective_rate = 0.03, # how much stored signal moves toward prototype on failure
  lapse = 0.05 # soft lapse in guess_probability
) {

  # assign input data frame to history internally
  history <- data
  
  #clamp to [0,1]
  clamp01 <- function(x) pmax(0, pmin(1, x))
  # prototype point for a referent type
  prototype_point <- function(type) if (type == "small") c(0,0) else c(1,1)
  # distance-based ease function
  ## small referents easier to guess near [0,0]
  ## large referents easier to guess near [1,1]
  ## currently distance measures range from 0.014 (max distance), to 1 (min distance) and is sharply dropping off when away from targets
  ### NOTE: Discuss and justify distance function (maybe euclidean)
  signal_evidence <- function(signal, type, distance_scale = sqrt(2)) {
    target <- prototype_point(type)
    dist <- sqrt((signal[1] - target[1])^2 + (signal[2] - target[2])^2)
    # Normalized distance [0,1]
    d_norm <- dist / distance_scale
    # Smooth boost + punishment: +1 at prototype, -1 at max distance
    evidence <- 1 - 2 * (dist / distance_scale)
    #clamp to [-1, 1] just in case
    evidence <- pmax(-1, pmin(1, evidence))
    evidence
  }
  # interpretation probability
  ## final guess probability = previous guessing rate + signal fit (iconicity bias)
  guess_probability <- function(agent_guess_prob, signal_xy, referent_type) {
    evidence <- signal_evidence(signal_xy, referent_type)
    # learned ability plus signal evidence, capped at 1
    # the added learning boost of evidence is reduced by a factor 'prototype_weight' which represents the strength of iconicity affecting guessing
    # convert probability to logodds
    p_logit <- qlogis(agent_guess_prob) + prototype_weight * evidence
    p_raw <- plogis(p_logit)
    p_final <- (lapse/2) + (1 - lapse) * p_raw
    p_final
  }
  # produce a token (produced signal) given stored signal and speaker skill
  # - speaker_guess_prob in [0,1] can modulate how biased production is; speaker skill = the speaker's current probability of correctly identifying that referent
  produce_signal <- function(previous_signal, referent_type, speaker_guess_prob) {
    target <- prototype_point(referent_type)
    # Example: less-skilled speakers produce more prototypical forms (you can flip this)
    bias_strength <- articulatory_production_bias * (1 - speaker_guess_prob)
    mu <- (1 - bias_strength) * previous_signal + bias_strength * target
    clamp01(rnorm(2, mean = mu, sd = drift_sd))
  }
  # reinforce stored signal toward produced signal after success
  reinforce_signal <- function(previous_signal, produced_signal) {
    clamp01((1 - reinforcement_rate) * previous_signal + reinforcement_rate * produced_signal)
  }
  # # when failure occurs, nudge stored signal toward prototype (or could do random drift)
  update_signal_on_failure <- function(previous_signal, referent_type) {
    target <- prototype_point(referent_type)
    clamp01(previous_signal + corrective_rate * (target - previous_signal) + rnorm(2, 0, drift_sd/2))
  }
  # # drift on failure
  # apply_drift <- function(previous_signal) {
  #   pmax(pmin(previous_signal + rnorm(2, 0, drift_sd), 1), 0)
  # }
  
  # log-odds learning update for guessing probabilities (additive in logit space)
  update_guess_logodds <- function(p_old, delta) {
    plogis(qlogis(p_old) + delta)
  }
  
  # convert learning_strength from probability to logit space
  p0 <- 0.3
  learning_strength_logit <- qlogis(p0 + learning_strength) - qlogis(p0)
  
  referents <- tibble(
    id = 1:n_referents,
    type = c(rep("small", n_small), rep("large", n_large))
  )

  # MAIN SIMULATION LOOP
  for (n in 1:n_sim) {
    # initial signals center of space
    signals <- tibble(
      id = referents$id,
      x = rep(0.5, n_referents),
      y = rep(0.5, n_referents)
    )
    # initial learning status of referents (after training)
    ## each agent has initial probability ~ 0.3 ± noise
    agentA_guess <- rbeta(n_referents, 2, 4)
    agentB_guess <- rbeta(n_referents, 2, 4)
    ## alternatives: average prob 0.5  = (2,2)
    ## alternatives: average prob 0.4  = (2,3)
    ## alternatives: average prob 0.33 = (2,4)
    
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
      r_type <- referents$type[r]
      
      # get stored signal, shared by both agents (one representation per referent)
      previous_signal <- c(signals$x[r], signals$y[r])
      # produce a token (speaker generates a signal)
      sig_prod <- produce_signal(previous_signal, r_type, speaker_guess[r])
      # listener computes probability of correct guess
      p_correct <- guess_probability(listener_guess[r], sig_prod, r_type)
      # actual outcome, binomial sampling
      success <- rbinom(1, 1, p_correct)
      # learning magnitude depends on success/failure
      success_scale <- 1.2
      failure_scale <- 0.8
      delta <- learning_strength_logit * ifelse(success == 1, success_scale, failure_scale)
      
      # learning: speaker improves guess rate, in logodds space
      speaker_guess[r] <- update_guess_logodds(speaker_guess[r], delta)
      # listener also learns due to feedback
      listener_guess[r] <- update_guess_logodds(listener_guess[r], delta)

      # signal memory updates: success -> reinforce toward produced form; failure -> nudge toward prototype (or drift)
      new_sig <- if(success == 1) {
        reinforce_signal(previous_signal, sig_prod)
      } else {
        #apply_drift(previous_signal)
        update_signal_on_failure(previous_signal, r_type)
      }
      signals$x[r] <- new_sig[1]
      signals$y[r] <- new_sig[2]
      # update guessing probability based on learning
      if (speaker == "A") {
        agentA_guess <- speaker_guess
        agentB_guess <- listener_guess
      } else {
        agentB_guess <- speaker_guess
        agentA_guess <- listener_guess
      }
      # log trials
      history <- rbind(
        history,
        data.frame(
          sim = n, round = t, referent = r,
          speaker = speaker, listener = listener, type = r_type,
          p_correct = p_correct, success = success,
          stored_x = previous_signal[1], stored_y = previous_signal[2],
          produced_x = sig_prod[1], produced_y = sig_prod[2]
        )
      )
    }
  }

  return(history)
}


# Main interaction loop function with separate memories for agents
run_interaction_sim_separate_memory <- function(
  data,
  n_sim = 10,
  n_referents = 6,
  n_small = n_referents/2,
  n_large = n_referents/2,
  n_rounds = 1000,
  drift_sd = 0.05,
  learning_strength = 0.05,
  iconicity_boost = 0.2,
  articulatory_production_bias = 0.15,
  reinforcement_rate = 0.05,
  corrective_rate = 0.03,
  lapse = 0.05
) {
  
  history <- data
  
  clamp01 <- function(x) pmax(0, pmin(1, x))
  
  prototype_point <- function(type)
    if (type == "small") c(0,0) else c(1,1)
  
  signal_ease <- function(sig, type) {
    target <- prototype_point(type)
    dist <- sqrt((sig[1] - target[1])^2 + (sig[2] - target[2])^2)
    exp(-dist)
  }
  
  guess_probability <- function(agent_guess_prob, signal_xy, referent_type) {
    ease <- signal_ease(signal_xy, referent_type)
    p_logit <- qlogis(agent_guess_prob) + iconicity_boost * ease
    p_raw <- plogis(p_logit)
    p_final <- (lapse/2) + (1 - lapse) * p_raw
    p_final
  }
  
  produce_signal <- function(previous_signal, referent_type, skill) {
    target <- prototype_point(referent_type)
    bias_strength <- articulatory_production_bias * (1 - skill)
    mu <- (1 - bias_strength) * previous_signal + bias_strength * target
    clamp01(rnorm(2, mu, drift_sd))
  }
  
  reinforce_signal <- function(previous_signal, produced_signal) {
    clamp01((1 - reinforcement_rate)*previous_signal + reinforcement_rate*produced_signal)
  }
  
  update_signal_on_failure <- function(previous_signal, referent_type) {
    target <- prototype_point(referent_type)
    clamp01(previous_signal + corrective_rate * (target - previous_signal) +
              rnorm(2, 0, drift_sd/2))
  }
  
  update_guess_logodds <- function(p_old, learning_strength)
    plogis(qlogis(p_old) + learning_strength)
  
  referents <- tibble(
    id = 1:n_referents,
    type = c(rep("small", n_small), rep("large", n_large))
  )
  
  for (n in 1:n_sim) {
    
    # separate memories for A and B
    signals_A <- tibble(id = 1:n_referents,
                        x = rep(0.5, n_referents),
                        y = rep(0.5, n_referents))
    
    signals_B <- tibble(id = 1:n_referents,
                        x = rep(0.5, n_referents),
                        y = rep(0.5, n_referents))
    
    # separate guess rates
    agentA_guess <- rbeta(n_referents, 2, 4)
    agentB_guess <- rbeta(n_referents, 2, 4)
    
    for (t in 1:n_rounds) {
      
      if (t %% 2 == 1) {
        speaker <- "A"; listener <- "B"
        speaker_guess <- agentA_guess
        listener_guess <- agentB_guess
        speaker_memory <- signals_A
        listener_memory <- signals_B
      } else {
        speaker <- "B"; listener <- "A"
        speaker_guess <- agentB_guess
        listener_guess <- agentA_guess
        speaker_memory <- signals_B
        listener_memory <- signals_A
      }
      
      # choose referent
      r <- sample(1:n_referents, 1)
      r_type <- referents$type[r]
      
      # retrieve speaker's stored signal
      previous_signal <- c(speaker_memory$x[r], speaker_memory$y[r])
      
      # produce signal
      sig_prod <- produce_signal(previous_signal, r_type, speaker_guess[r])
      
      # listener computes probability of correct guess
      p_correct <- guess_probability(listener_guess[r], sig_prod, r_type)
      success <- rbinom(1, 1, p_correct)
      
      # update speaker skill
      speaker_guess[r] <- update_guess_logodds(speaker_guess[r], learning_strength)
      
      # listener skill update
      listener_guess[r] <- update_guess_logodds(listener_guess[r], learning_strength)
      
      # speaker memory update
      new_sig_speaker <- if (success == 1) {
        reinforce_signal(previous_signal, sig_prod)
      } else {
        update_signal_on_failure(previous_signal, r_type)
      }
      
      # listener memory update
      # Theoretically: listener encodes the form they just heard.
      previous_signal_listener <- c(listener_memory$x[r], listener_memory$y[r])
      new_sig_listener <- if (success == 1) {
        reinforce_signal(previous_signal_listener, sig_prod)
      } else {
        update_signal_on_failure(previous_signal_listener, r_type)
      }
      
      
      # apply updates to the correct tables
      if (speaker == "A") {
        signals_A$x[r] <- new_sig_speaker[1]
        signals_A$y[r] <- new_sig_speaker[2]
        agentA_guess <- speaker_guess
        
        signals_B$x[r] <- new_sig_listener[1]
        signals_B$y[r] <- new_sig_listener[2]
        agentB_guess <- listener_guess
        
      } else {
        signals_B$x[r] <- new_sig_speaker[1]
        signals_B$y[r] <- new_sig_speaker[2]
        agentB_guess <- speaker_guess
        
        signals_A$x[r] <- new_sig_listener[1]
        signals_A$y[r] <- new_sig_listener[2]
        agentA_guess <- listener_guess
      }
      
      history <- rbind(
        history,
        data.frame(
          sim = n,
          round = t,
          referent = r,
          speaker = speaker,
          listener = listener,
          type = r_type,
          p_correct = p_correct,
          success = success,
          speaker_stored_x = previous_signal[1],
          speaker_stored_y = previous_signal[2],
          produced_x = sig_prod[1],
          produced_y = sig_prod[2],
          listener_stored_x = previous_signal_listener[1],
          listener_stored_y = previous_signal_listener[2]
        )
      )
    }
  }
  
  return(history)
}

