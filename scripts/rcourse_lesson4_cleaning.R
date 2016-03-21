## LOAD PACKAGES ####
library(dplyr)
library(purrr)


## READ IN DATA ####
data = list.files(path = "data", full.names = T) %>%
  map(read.table, header = T, sep = "\t", na.strings = c("", NA)) %>%
  reduce(rbind)


## CLEAN DATA ####
data_clean = data %>%
  filter(series != "tas") %>%
  mutate(series = factor(series)) %>%
  filter(alignment == "foe" | alignment == "friend") %>%
  mutate(alignment = factor(alignment)) %>%
  filter(!is.na(conservation)) %>%
  mutate(extinct = ifelse(conservation == "LC", 0, 1)) %>%
  group_by(series, alignment, alien) %>%
  arrange(episode) %>%
  filter(row_number() == 1) %>%
  ungroup()