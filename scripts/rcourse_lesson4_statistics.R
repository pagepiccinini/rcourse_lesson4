## READ IN DATA ####
source("scripts/rcourse_lesson4_cleaning.R")


## LOAD PACKAGES ####
# [none currently needed]


## ORGANIZE DATA ####
data_stats = data_clean %>%
  # Change order of levels so "tos" is first
  mutate(series = factor(series, levels = c("tos", "tng")))


## BUILD MODELS ####
# One variable (series)
extinct_series.glm = glm(extinct ~ series, family = "binomial", data = data_stats)

extinct_series.glm_sum = summary(extinct_series.glm)
extinct_series.glm_sum

# One variable (alignment)
extinct_alignment.glm = glm(extinct ~ alignment, family = "binomial", data = data_stats)

extinct_alignment.glm_sum = summary(extinct_alignment.glm)
extinct_alignment.glm_sum

# Two variables additive
extinct_seriesalignment.glm = glm(extinct ~ series + alignment, family = "binomial", data = data_stats)

extinct_seriesalignment.glm_sum = summary(extinct_seriesalignment.glm)
extinct_seriesalignment.glm_sum

# Two variables interaction (pre-determined baselines)
extinct_seriesxalignment.glm = glm(extinct ~ series * alignment, family = "binomial", data = data_stats)

extinct_seriesxalignment.glm_sum = summary(extinct_seriesxalignment.glm)
extinct_seriesxalignment.glm_sum

# Two variables interaction (change baseline for series)
extinct_seriesxalignment_tng.glm = glm(extinct ~ relevel(series, "tng") * alignment, family = "binomial", data = data_stats)

extinct_seriesxalignment_tng.glm_sum = summary(extinct_seriesxalignment_tng.glm)
extinct_seriesxalignment_tng.glm_sum

# Two variables interaction (change baseline for alignment)
extinct_seriesxalignment_friend.glm = glm(extinct ~ series * relevel(alignment, "friend"), family = "binomial", data = data_stats)

extinct_seriesxalignment_friend.glm_sum = summary(extinct_seriesxalignment_friend.glm)
extinct_seriesxalignment_friend.glm_sum


