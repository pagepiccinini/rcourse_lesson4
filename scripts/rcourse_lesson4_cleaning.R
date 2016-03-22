## LOAD PACKAGES ####
library(dplyr)
library(purrr)


## READ IN DATA ####
data = list.files(path = "data", full.names = T) %>%
  # Run read.table call on all files
  map(read.table, header = T, sep = "\t", na.strings = c("", NA)) %>%
  # Combine all data frames into a single data frame by row
  reduce(rbind)


## CLEAN DATA ####
data_clean = data %>%
  # Drop any data from "The Animated Series"
  filter(series != "tas") %>%
  mutate(series = factor(series)) %>%
  # Drop any data that is not a foe or friend
  filter(alignment == "foe" | alignment == "friend") %>%
  mutate(alignment = factor(alignment)) %>%
  # Drop any data this an NA for conservation
  filter(!is.na(conservation)) %>%
  # Create a new column for logistic regression
  mutate(extinct = ifelse(conservation == "LC", 0, 1)) %>%
  # Drop any data besides the first appearance of each alien with series and alignment
  group_by(series, alignment, alien) %>%
  arrange(episode) %>%
  filter(row_number() == 1) %>%
  ungroup()