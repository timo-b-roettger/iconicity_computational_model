# LIBRARIES AND HELPERS----
library(tidyverse)  # data wrangling and plotting
library(magrittr)   # for all pipes
library(ggplot2)    # for plotting
library(patchwork)  # combining plots
library(broom)      # for regression analysis
library(ggdist)     # for plotting
library(ggside)

normalize_01 <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

# MAIN SIM FUNCTION----
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
    run_interaction_sim(n_sim = 50, n_rounds = 50, n_generations = 10, phonological_attractors = FALSE, expressive_agents = FALSE) %>%
    mutate(model_type = "semanticAttractors"),
  d.empty %>% 
    run_interaction_sim(n_sim = 50, n_rounds = 50, n_generations = 10, phonological_attractors = FALSE, expressive_agents = TRUE) %>%
    mutate(model_type = "semanticAttractors_expressiveAgents"),
  d.empty %>% 
    run_interaction_sim(n_sim = 50, n_rounds = 50, n_generations = 10, phonological_attractors = TRUE, expressive_agents = FALSE) %>%
    mutate(model_type = "semanticPhonAttractors"),
  d.empty %>% 
    run_interaction_sim(n_sim = 50, n_rounds = 50, n_generations = 10, phonological_attractors = TRUE, expressive_agents = TRUE) %>%
    mutate(model_type = "allAttractors_expressiveAgents"))

# Signal space use across simulations
d_signal_mean <- d.simulation %>%
  mutate(total_round = (generation - 1) * 10 + round,
         x = map_dbl(produced_signal, 1),
         y = map_dbl(produced_signal, 2)) %>%
  group_by(model_type, total_round, type, generation) %>%
  summarise(
    mean_x = mean(x, na.rm = TRUE),
    mean_y = mean(y, na.rm = TRUE),
    .groups = "drop")


# Signal space use across simulations
d_signal <- d.simulation %>%
  mutate(total_round = (generation - 1) * 50 + round,
  #mutate(total_round = (generation - 1) * 10 + round,
         x = map_dbl(produced_signal, 1),
         y = map_dbl(produced_signal, 2))

# PLOT ICONICITY----
d.iconicity <- d.simulation %>%
  mutate(model_type = factor(
    model_type, 
    levels = c("semanticAttractors", "semanticAttractors_expressiveAgents", "semanticPhonAttractors", "allAttractors_expressiveAgents"),
    labels = c("Semantic attractors", "Semantic attractors, expressive agents", "Semantic and phonological attractors", "Semantic and phonological attractors, expressive agents"),
    ordered = TRUE),
    #total_round = (generation - 1) * 10 + round,
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
  # Add lines at generational overturn
  geom_vline(xintercept = seq(0, 100, by = 10), 
             color = "grey", 
             lty = "dotted") +
  scale_color_viridis_c(begin = 0,
                        end = 1,
                        values = seq(0,1,0.1))+
  scale_y_continuous(limits = c(-1,1), breaks = seq(-1,1,0.25)) +
  scale_x_continuous(breaks = seq(0, 500, 100),
                      labels = seq(0, 50, 10)) +
  labs(title = "Iconicity over generations",
       y = "Iconicity\n(above 0 = iconic,\nbelow zero = anti-iconic)", 
       x = "Generation (each with 50 rounds)") +
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
  scale_x_continuous(breaks = seq(0, 500, 100),
                     labels = seq(0, 50, 10)) +
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
  filter(model_type == "semanticAttractors") |> 
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
    levels = c("semanticAttractors", "semanticAttractors_expressiveAgents", "semanticPhonAttractors", "allAttractors_expressiveAgents"),
    labels = c("Semantic attractors", "+ expressive agents", "+ phonological distractors", "phonological distractors and expressive agents"),
    ordered = TRUE)) |> 
  mutate(iconic_ed = ifelse(type == "small",
                            sqrt((x - 0.15)^2 + (y - 0.85)^2),
                            sqrt((x - 0.85)^2 + (y - 0.15)^2)),
         attractor_1_ed = sqrt((x - 0.15)^2 + (y - 0.15)^2),
         attractor_2_ed = sqrt((x - 0.85)^2 + (y - 0.85)^2)
         )

d_signal_ed_agg <- d_signal_ed |> 
  group_by(model_type, simulation, type, total_round) |> 
  summarise(iconic_ed = mean(iconic_ed),
            attractor_1_ed = mean(attractor_1_ed),
            attractor_2_ed = mean(attractor_2_ed))

d_signal_ed_agg_all <- d_signal_ed_agg |> 
  group_by(model_type, type, total_round) |> 
  summarise(iconic_ed = mean(iconic_ed),
            attractor_1_ed = mean(attractor_1_ed),
            attractor_2_ed = mean(attractor_2_ed))

# get reference values
## if small where is border for attractor?
iconic_border = euc_dist(c(0.15,0.85), c(0.45,0.55)) ## 0.42
neutral = euc_dist(c(0.15,0.85), c(0.5,0.5)) ## ~0.5
    
# plot average ED
d_signal_ed_agg_all |> 
  ggplot(aes(x = total_round, y = iconic_ed,
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
           x = 250,
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
  ggplot(aes(x = total_round, y = attractor_1_ed,
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
  group_by(model_type, simulation, type, total_round) |> 
  summarise(iconic_ed = mean(iconic_ed),
            attractor_1_ed = mean(attractor_1_ed),
            attractor_2_ed = mean(attractor_2_ed),
            in_pocket = mean(in_pocket))

d_signal_ed_agg_all <- d_signal_ed_agg |> 
  group_by(model_type, type, total_round) |> 
  summarise(iconic_ed = mean(iconic_ed),
            attractor_1_ed = mean(attractor_1_ed),
            attractor_2_ed = mean(attractor_2_ed),
            in_pocket = mean(in_pocket))

# calculate when attractor was entered and when 
# plot average ED
d_signal_ed_agg_all |> 
  ggplot(aes(x = total_round, y = in_pocket,
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
      levels = c("semanticAttractors", "semanticAttractors_expressiveAgents", "semanticPhonAttractors", "allAttractors_expressiveAgents"),
      labels = c("Semantic attractors", "+ expressive agents", "+ phonological distractors", "phonological distractors and expressive agents"),
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
  
