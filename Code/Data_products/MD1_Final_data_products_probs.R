### final data products

library(tidyr)
library(tidyverse)
## WRP scenarios
getwd()

finalwithmin <- read.csv("ignore/results/scenarios/probs/X2a_mean_probs_all_spp_best_slices_WRP_scenarios_updatedApril2021.csv")

## remove scaled probability column
names(finalwithmin)

finalwithmin <- finalwithmin %>%
  select(-ScaledProbability, -X, -Code, -CodeMin, - SDProbability)


write.csv(finalwithmin, "ignore/results/Final/DP1_Species_habitat_probabilities_WRP_Scenarios_May2021.csv")


## Stormwater

finalwithmin <- read.csv("ignore/results/scenarios/probs/SW2a_mean_probs_all_spp_best_slices_stormwater_scenarios_updatedApril2021.csv")

## remove scaled probability column
names(finalwithmin)

finalwithmin <- finalwithmin %>%
  select(-ScaledProbability, -X, -Code, -CodeMin, - SDProbability)


write.csv(finalwithmin, "ignore/results/Final/DP1_Species_habitat_probabilities_Stormwater_Scenarios_May2021.csv")


## baseline

finalwithmin <- read.csv("ignore/results/scenarios/probs/A2a_mean_probs_all_spp_best_slices_baseline_updatedApril2021.csv")

## remove scaled probability column
names(finalwithmin)

finalwithmin <- finalwithmin %>%
  select(-ScaledProbability, -X, -Code, -CodeMin, - SDProbability, -Scenario, -MaxProbability, -Number) %>%
  mutate(Scenario = 0)


write.csv(finalwithmin, "ignore/results/Final/DP1_Species_habitat_probabilities_Baseline_May2021.csv")

