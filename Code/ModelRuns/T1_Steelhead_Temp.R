library(tidyverse)
library(tidyr)
# library(sm)
# library(lubridate) # work with dates
# library(dplyr)     # data manipulation (filter, summarize, mutate)
# library(ggplot2)   # graphics
# library(gridExtra) # tile several plots next to each other
# library(scales)
# library(data.table)
# library(zoo)
# library(scales)
getwd()
## temp data

setwd("/Users/katieirving/Documents/git/flow_eco_mech")

### change wd
setwd("input_data/LAR_WaterTemperature")
tp <- list.files(pattern="Sim_Avg")
tp
## change wd back 
setwd("/Users/katieirving/Documents/git/flow_eco_mech")
all_probsx <- NULL
n=1

## start loop
for(n in 1: length(tp)) {
  ## temp data
  TempData <- read.csv(file=paste("input_data/LAR_WaterTemperature/", tp[n], sep=""))
  TempData ## mean_max_temp
  
  ### species data
  fitdata <- read.csv("output_data/adult_temp_prob_curve_data.csv")
  
  ## predict 
  # all_data <- hyd_dep %>%
  #   mutate(prob_fit = predict(clad_depth_mod, newdata = hyd_dep, type="response")) %>%
  #   mutate(prob_fit = ifelse(prob_fit<=0, 0, prob_fit)) ## predicts negative percentages - cut off at 0 for quick fix
  # 
  
  all_data_max <- TempData %>%
    mutate(Thresh = ifelse(Max_Temp >= 8 & Max_Temp <= 11, "Suitable", "Unsuitable")) %>%
    mutate(Suitable = sum(Thresh == "Suitable"), Unsuitable = sum(Thresh == "Unsuitable")) %>%
    select(Node, Suitable, Unsuitable) %>%
    mutate(Metric = "MaxTemp") %>%
    distinct()
  
  
  all_data_max_mean <- TempData %>%
    # group_by(variable) %>%
    mutate(Thresh = ifelse(Mean_Max_Temp >= 8 & Mean_Max_Temp <= 11, "Suitable", "Unsuitable")) %>%
    mutate(Suitable = sum(Thresh == "Suitable"), Unsuitable = sum(Thresh == "Unsuitable")) %>%
    select(Node, Suitable, Unsuitable) %>%
    mutate(Metric = "MeanMaxTemp") %>%
    distinct()
  
  all_data_min <- TempData %>%
    # group_by(variable) %>%
    mutate(Thresh = ifelse(Min_Temp >= 8 & Min_Temp <= 11, "Suitable", "Unsuitable")) %>%
    mutate(Suitable = sum(Thresh == "Suitable"), Unsuitable = sum(Thresh == "Unsuitable")) %>%
    select(Node, Suitable, Unsuitable) %>%
    mutate(Metric = "MinTemp") %>%
    distinct()
  
  all_data_mean <- TempData %>%
    # group_by(variable) %>%
    mutate(Thresh = ifelse(Mean_Temp >= 8 & Mean_Temp <= 11, "Suitable", "Unsuitable")) %>%
    mutate(Suitable = sum(Thresh == "Suitable"), Unsuitable = sum(Thresh == "Unsuitable")) %>%
    select(Node, Suitable, Unsuitable) %>%
    mutate(Metric = "MeanTemp") %>%
    distinct()
  
  all_probs <- rbind(all_data_max, all_data_max_mean, all_data_min, all_data_mean)
  
  
  all_probsx <- rbind(all_probsx, all_probs)
}

all_probsx <- all_probsx %>%
  mutate(Species = "Steelhead", Life_Stage = "Migration")

all_probsx

write.csv(all_probsx, file="output_data/T1_Steelhead_Migration_Temperature_probs.csv")


# Smolts ------------------------------------------------------------------

all_probsx <- NULL


## start loop
for(n in 1: length(tp)) {
  ## temp data
  TempData <- read.csv(file=paste("input_data/LAR_WaterTemperature/", tp[n], sep=""))
  TempData
  
  ### species data
  fitdata <- read.csv("output_data/adult_temp_prob_curve_data.csv")
  
  ## predict 
  # all_data <- hyd_dep %>%
  #   mutate(prob_fit = predict(clad_depth_mod, newdata = hyd_dep, type="response")) %>%
  #   mutate(prob_fit = ifelse(prob_fit<=0, 0, prob_fit)) ## predicts negative percentages - cut off at 0 for quick fix
  # 
  
  all_data_max <- TempData %>%
    mutate(Thresh = ifelse(Max_Temp >= 8 & Max_Temp <= 11, "Suitable", "Unsuitable")) %>%
    mutate(Suitable = sum(Thresh == "Suitable"), Unsuitable = sum(Thresh == "Unsuitable")) %>%
    select(Node, Suitable, Unsuitable) %>%
    mutate(Metric = "MaxTemp") %>%
    distinct()
  
  
  all_data_max_mean <- TempData %>%
    # group_by(variable) %>%
    mutate(Thresh = ifelse(Mean_Max_Temp >= 8 & Mean_Max_Temp <= 11, "Suitable", "Unsuitable")) %>%
    mutate(Suitable = sum(Thresh == "Suitable"), Unsuitable = sum(Thresh == "Unsuitable")) %>%
    select(Node, Suitable, Unsuitable) %>%
    mutate(Metric = "MeanMaxTemp") %>%
    distinct()
  
  all_data_min <- TempData %>%
    # group_by(variable) %>%
    mutate(Thresh = ifelse(Min_Temp >= 8 & Min_Temp <= 11, "Suitable", "Unsuitable")) %>%
    mutate(Suitable = sum(Thresh == "Suitable"), Unsuitable = sum(Thresh == "Unsuitable")) %>%
    select(Node, Suitable, Unsuitable) %>%
    mutate(Metric = "MinTemp") %>%
    distinct()
  
  all_data_mean <- TempData %>%
    # group_by(variable) %>%
    mutate(Thresh = ifelse(Mean_Temp >= 8 & Mean_Temp <= 11, "Suitable", "Unsuitable")) %>%
    mutate(Suitable = sum(Thresh == "Suitable"), Unsuitable = sum(Thresh == "Unsuitable")) %>%
    select(Node, Suitable, Unsuitable) %>%
    mutate(Metric = "MeanTemp") %>%
    distinct()
  
  all_probs <- rbind(all_data_max, all_data_max_mean, all_data_min, all_data_mean)
  
  
  all_probsx <- rbind(all_probsx, all_probs)
}

all_probsx <- all_probsx %>%
  mutate(Species = "Steelhead", Life_Stage = "Smolts")

all_probsx

write.csv(all_probsx, file="output_data/T1_Steelhead_Smolts_Temperature_probs.csv")
