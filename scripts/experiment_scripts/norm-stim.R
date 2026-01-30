# Simple script to select best token for stim
## There are three recorded items for each word, we should select the item that best represents a 'Norwegian schwa'.
## There is only one reference value based on one recording from one male talker.
## One option is to use the entire vowel space from that talker together with the female talker's all schwa to get a normalized value for schwa.
## Or, even better, use as many vowels there is from the female talker, extracted from carrier sentences and establish dialect template based on all.

## Constants and libraries
install.packages('phonTools', repos = c('https://santiagobarreda.r-universe.dev', 'https://cloud.r-project.org'))

library(tidyverse)
library(ggplot2)
library(phonTools)

levels.vowel.NoFA = c("ii1", "ih1", "yy1", "yh1", "uu1", "uh1", "ee1", "eh1", "ae1", "aeh1", "oe1", "oeh1", "@", "aa1", "ah1", "oa1", "oah1", "oo1", "oh1")
levels.vowel.IPA.no = c("[iː]", "[i]", "[yː]", "[y]", "[ʉː]", "[ʉ]", "[eː]", "[ɛ]", "[æː]", "[æ]", "[øː]", "[œ]", "[ə]", "[ɑː]", "[a]", "[oː]", "[ɔ]", "[uː]", "[u]")

## Read in formant data from stimuli talker


## Manually add formant data from the reference talker (from Kristoffersen, 2007)
d.ref.vowels <- tibble(
  vowel = c(levels.vowel.IPA.no),
  F1 = c(274, 272, 231, 261, 239, 266, 376, 465, 778, 695, 337, 404, 433, 620, 602, 341, 360, 250, 287),
  F2 = c(2083, 2090, 2091, 1806, 1509, 1337, 2006, 1730, 1360, 1353, 1254, 1326, 1334, 924, 999, 671, 778, 567, 685),
  speaker = "ref")

## Plot reference vowel space
p.vowels <- d.ref.vowels %>%
  ggplot(
    aes(
      x = F2,
      y = F1)) +
  geom_label(
    aes(
      colour = vowel,
      label = vowel,
      fontface = ifelse(speaker == "ref", 1, 2)),
    alpha = .4,
    label.size = NA,
    label.padding = unit(.06, "cm")) +
  scale_x_reverse("F2", position = "top") +
  scale_y_reverse("F1", position = "right") +
  guides(color = "none")
p.vowels

## Normalize all vowels according to Nearey's uniform scaling
d.vowels.normalized <- 
  group_by(speaker) %>%
  normalize(
    formants = as.matrix(d.ref.vowels[, c("F1", "F2")]),
    speakers = d.ref.vowels$speaker,
    vowels   = d.ref.vowels$vowel,
    method   = "neareyE") %>%
  as_tibble() %>%
  # join in original dataset to get raw ffs
  left_join(d.ref.vowels %>%
              rename(F1_Hz = F1, F2_Hz = F2) , 
            by = c("speaker", "vowel")) %>%
  select(vowel, speaker, everything()) %>%
  ungroup()

# Visualise the normalized space
p.vowels %+%
  (d.vowels.normalized)

## Compare schwa's of the two talkers
## Use visualizations + distance based metric (euclidean distance?)
## Generate dialect target schwa based on the mean of the two values?
d.schwa <- d.vowels.normalized %>%
  filter(vowel == "[ə]") %>%
  mutate(F1_template = mean(F1),
         F2_template = mean(F2)) %>%
  group_by(speaker) %>%
  mutate(
    # compute euclidean distance to target schwa
    distance_to_target = sqrt((F1 - F1_template)^2 + (F2 - F2_template)^2))

