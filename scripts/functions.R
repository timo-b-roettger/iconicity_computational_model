# Constants ------------------------------------------------------------------


# Functions ------------------------------------------------------------------
## Functions for iconicity simulation ----------------------------------------
# Have this outside main function for plotting reasons
prototype_point <- function(type) if (type == "small") c(0,0) else c(1,1)
signal_ease <- function(sig, type) {
  target <- prototype_point(type)
  dist <- sqrt((sig[1] - target[1])^2 + (sig[2] - target[2])^2)
  exp(-1 * dist)
}

# Main interaction loop function
run_interaction_sim <- function(
  data,
  n_sim = 10, # number of simulations
  n_referents = 6, # number of unique referents in guessing game
  n_small = n_referents/2, # number of small referents
  n_large = n_referents/2, # number of large referents
  n_rounds = 1000, # number of interaction rounds
  drift_sd = 0.05, # amount of variation introduced during production
  learn_rate = 0.005, # amount of added memory strengthening for words per round
  iconicity_boost = 0.2, # multiplicator for distance to iconic prototypes
  prod_bias = 0.15, # baseline production bias toward prototype
  reinforcement_rate = 0.05, #how strongly stored signals move toward produced signal on success
  failure_step = 0.03, # how much stored signal moves toward prototype on failure
  lapse = 0.05 # soft lapse in guess_probability
) {

  # assign input data frame to history internally
  history <- data

  n_small <- n_referents / 2
  n_large <- n_referents / 2
  
  #clamp to [0,1]
  clamp01 <- function(x) pmax(0, pmin(1, x))
  # prototype point for a referent type
  prototype_point <- function(type) if (type == "small") c(0,0) else c(1,1)
  # distance-based ease function
  ## small referents easier to guess near [0,0]
  ## large referents easier to guess near [1,1]
  ## currently distance measures range from 0.014 (max distance), to 1 (min distance) and is sharply dropping off when away from targets
  ### NOTE: Discuss and justify distance function (maybe euclidean)
  signal_ease <- function(sig, type) {
    target <- prototype_point(type)
    dist <- sqrt((sig[1] - target[1])^2 + (sig[2] - target[2])^2)
    exp(-1 * dist)
  }
  # interpretation probability
  ## final guess probability = previous guessing rate + signal fit (iconicity bias)
  guess_probability <- function(agent_guess_prob, signal_xy, ref_type) {
    ease <- signal_ease(signal_xy, ref_type)
    # learned ability plus signal ease, capped at 1
    # the added learning boost of ease is reduced by a factor 'iconicity_boost' which represents the strength of iconicity affecting guessing
    # convert probability to logodds
    p_logit <- qlogis(agent_guess_prob) + iconicity_boost * ease
    p_raw <- plogis(p_logit)
    p_final <- (lapse/2) + (1 - lapse) * p_raw
    p_final
  }
  # produce a token (produced signal) given stored signal and speaker skill
  # - speaker_skill in [0,1] can modulate how biased production is; speaker skill = the speaker's current probability of correctly identifying that referent
  produce_signal <- function(sig_prev, ref_type, speaker_skill) {
    target <- prototype_point(ref_type)
    # Example: less-skilled speakers produce more prototypical forms (you can flip this)
    bias_strength <- prod_bias * (1 - speaker_skill)
    mu <- (1 - bias_strength) * sig_prev + bias_strength * target
    clamp01(rnorm(2, mean = mu, sd = drift_sd))
  }
  # reinforce stored signal toward produced signal after success
  reinforce_signal <- function(sig_prev, produced_sig) {
    clamp01((1 - reinforcement_rate) * sig_prev + reinforcement_rate * produced_sig)
  }
  # # when failure occurs, nudge stored signal toward prototype (or could do random drift)
  update_signal_on_failure <- function(sig_prev, ref_type) {
    target <- prototype_point(ref_type)
    clamp01(sig_prev + failure_step * (target - sig_prev) + rnorm(2, 0, drift_sd/2))
  }
  # # drift on failure
  # apply_drift <- function(sig_prev) {
  #   pmax(pmin(sig_prev + rnorm(2, 0, drift_sd), 1), 0)
  # }
  
  # log-odds learning update for guessing probabilities (additive in logit space)
  update_guess_logodds <- function(p_old, learn_rate) plogis(qlogis(p_old) + learn_rate)
  
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
      
      # retrieve its signal: 
      ## either just pick the signal from last round (perfect memory of signal)
      #sig <- signals %>% filter(id == r) %>% select(x, y) %>% as.numeric()
      ## alternatively: retrieve it's previous signal if memory strength good enough
      # if (speaker_guess[r] > 0.5) {
      #   sig <- signals %>% filter(id == r) %>% select(x, y) %>% as.numeric()
      #   # if not randomly draw around center
      #   } else {sig <- c(rnorm(1, 0.5, 0.1), c(rnorm(1, 0.5, 0.1)))
      #   ## another alternative could be to draw from existing categories, which would make the signal bounce around
      # }
      # # this resets the signal drift until learned 
      
      # get stored signal
      sig_prev <- c(signals$x[r], signals$y[r])
      # produce a token (speaker generates a signal)
      sig_prod <- produce_signal(sig_prev, r_type, speaker_guess[r])
      # listener computes probability of correct guess
      p_correct <- guess_probability(listener_guess[r], sig_prod, r_type)
      # actual outcome, binomial sampling
      success <- rbinom(1, 1, p_correct)
      # learning: speaker improves guess rate, in logodds space
      speaker_guess[r] <- update_guess_logodds(speaker_guess[r], learn_rate)
      # listener also learns due to feedback
      if (success == 1) listener_guess[r] <- update_guess_logodds(listener_guess[r], learn_rate)

      # signal memory updates: success -> reinforce toward produced form; failure -> nudge toward prototype
      new_sig <- if(success == 1) {
        reinforce_signal(sig_prev, sig_prod)
      } else {
        #apply_drift(sig_prev)
        update_signal_on_failure(sig_prev, r_type)
      }
      signals$x[r] <- new_sig[1]
      signals$y[r] <- new_sig[2]
      # update guessing probability based on learning
      if (speaker == "A") {
        agentA_guess <- speaker_guess; agentB_guess <- listener_guess
      } else {
        agentB_guess <- speaker_guess; agentA_guess <- listener_guess
      }
      # log trials
      history <- rbind(
        history,
        data.frame(
          sim = n, round = t, referent = r,
          speaker = speaker, listener = listener, type = r_type,
          p_correct = p_correct, success = success,
          stored_x = sig_prev[1], stored_y = sig_prev[2],
          produced_x = sig_prod[1], produced_y = sig_prod[2]
        )
      )
    }
  }

  return(history)
}

