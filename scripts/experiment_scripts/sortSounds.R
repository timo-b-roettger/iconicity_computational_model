# Script for automatically sorting wav files by sentence type after experiment round
## Two scenarios: 1) sentence type encoded in filename; 2) sentence type read in from metadata csv file

## Alternative 1
library(stringr)

# directory with wav files
input_dir <- "path/to/wavs"
output_dir <- "path/to/sorted_wavs"

files <- list.files(input_dir, pattern = "\\.wav$", full.names = TRUE)

for (f in files) {
  fname <- basename(f)
  
  # extract sentence type (adjust regex!)
  type <- str_extract(fname, "Type\\d+")
  
  if (is.na(type)) {
    warning("No type found for ", fname)
    next
  }
  
  target_dir <- file.path(output_dir, type)
  
  if (!dir.exists(target_dir)) {
    dir.create(target_dir, recursive = TRUE)
  }
  
  file.rename(f, file.path(target_dir, fname))
}

## Alternative 2
library(dplyr)
library(stringr)

# Parse metadata from filename
parse_filename <- function(fname) {
  tibble(
    filename = fname,
    block = as.integer(str_extract(fname, "(?<=Block)\\d+")),
    trial = as.integer(str_extract(fname, "(?<=Trial)\\d+")),
    user  = as.integer(str_extract(fname, "(?<=User)\\d+"))
  )
}

# Load metadata and join
wav_dir <- "path/to/wavs"
meta <- read.csv("trial_metadata.csv")

files <- list.files(wav_dir, pattern = "\\.wav$", full.names = FALSE)

file_info <- bind_rows(lapply(files, parse_filename))

file_info <- file_info %>%
  left_join(meta, by = c("block" = "Block",
                         "trial" = "Trial",
                         "user"  = "User"))


# Move files into sentence-type folders
for (i in seq_len(nrow(file_info))) {
  f <- file_info$filename[i]
  type <- file_info$SentenceType[i]
  
  if (is.na(type)) {
    warning("No sentence type for ", f)
    next
  }
  
  target_dir <- file.path(wav_dir, type)
  if (!dir.exists(target_dir)) dir.create(target_dir)
  
  file.rename(
    from = file.path(wav_dir, f),
    to   = file.path(target_dir, f)
  )
}



