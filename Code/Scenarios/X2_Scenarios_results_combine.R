## combine all scenario data
### merge results
library(tidyverse)
library(tidyr)
### work flow
## upload time stats
## converge all nodes per species
## converge all species
## calculate suitability per node position
## calculate suitability per node (if 1 position suitable, node is suitable)

## threshold to change to annual 
curve_stats <- c("Low", "Medium", "High")
thresh_stats <- c("Seasonal")

# concrete <- c("F34D", "F37B_High", "F45B", "F319", "LA13", "LA1")

setwd("/Users/katieirving/Documents/git/flow_eco_mech")
## upload all time stats csvs

## time stats
ts <- list.files("output_data/scenarios", pattern="time_stats_updated_hyd")
length(ts) ## 219
ts
j=1

time_statsx <- NULL

for(j in 1: length(ts)) {
  
  time_stats <- read.csv(file=paste("output_data/scenarios/", ts[j], sep=""))

  
  ######## juvenile
  time_stats <- time_stats %>% 
    select(-X) %>%
    # filter(Probability_Threshold == ann_stats) %>%
    rename(TimePeriod = season, TimePercentage = value) %>%
    distinct()
  
  time_statsx <- rbind(time_statsx, time_stats)
  
}

head(time_statsx)


# ## change time period to seasonal and add bottom and water year type
# 
# time_statsx <- time_statsx %>%
#   group_by(Species, Life_Stage, Hydraulic)

time_stats_critical_willow <- time_statsx %>%
  filter(TimePeriod == "critical") %>% ## change to critical
  filter(Species == "Willow", Life_Stage == "Germination") %>%
  distinct()

time_stats_willow <- time_stats_critical_willow %>%
  filter(Probability_Threshold %in% thresh_stats) %>%
  pivot_wider(names_from = Probability_Threshold, values_from = TimePercentage) %>%
  mutate(Suitability_Class = NA) %>%
  distinct()

time_stats_willow$Seasonal[is.na(time_stats_willow$Seasonal)] <- 0
# group_by(Node, position, Species, Life_Stage, water_year) %>%

probs <- seq(1, dim(time_stats_willow)[1], 1)  


for(p in 1: length(probs)) {
   
  time_stats_willow$Suitability_Class[p] = if(time_stats_willow$Seasonal[p] >=30 && 
                                              time_stats_willow$Seasonal[p] <= 76) {
    paste("High")
  } else  if(time_stats_willow$Seasonal[p] <= 30 || 
             time_stats_willow$Seasonal[p] >= 76 ){
    paste("Low")

  } else {
    paste("Partial")
  }
  
}

time_stats_willow <- time_stats_willow %>%
  pivot_longer(Seasonal, names_to= "Probability_Threshold", values_to = "TimePercentage")
warnings()


time_stats_critical <- time_statsx %>%
  filter(TimePeriod == "critical") %>% ## change to critical
  filter(!Species == "Willow", !Life_Stage == "Germination") %>%
  distinct()

time_stats_ann <- time_stats_critical %>%
  filter(Probability_Threshold %in% curve_stats) %>%
  pivot_wider(names_from = Probability_Threshold, values_from = TimePercentage) %>%
  mutate(Suitability_Class = NA) %>%
  distinct()
# group_by(Node, position, Species, Life_Stage, water_year) %>%

probs <- seq(1, dim(time_stats_ann)[1], 1)  


for(p in 1: length(probs)) {
  
  time_stats_ann$Suitability_Class[p] = if(time_stats_ann$High[p] >= 75) {
    paste("High")
  } else  if(time_stats_ann$Medium[p] >= 25 & time_stats_ann$Medium[p] <= 75 ){
    paste("Partial")
  } else  if(time_stats_ann$Low[p] < 25){
    paste("Low")
  } else {
    paste("Partial")
  }
  
}

time_stats_ann <- time_stats_ann %>%
  pivot_longer(Low:High, names_to= "Probability_Threshold", values_to = "TimePercentage")

## calculate suitability

time_stats_seas <- time_stats_critical %>%
  filter(Probability_Threshold %in% thresh_stats) %>%
  filter(!Species == "Willow", !Life_Stage == "Germination") %>%
  pivot_wider(names_from = Probability_Threshold, values_from = TimePercentage) %>%
  mutate(Suitability_Class = NA) %>%
  distinct()
  
time_stats_seas$Seasonal[is.na(time_stats_seas$Seasonal)] <- 0
# group_by(Node, position, Species, Life_Stage, water_year) %>%

probs <- seq(1, dim(time_stats_seas)[1], 1)  



for(p in 1: length(probs)) {
  
  time_stats_seas$Suitability_Class[p] = if(time_stats_seas$Seasonal[p] >= 75) {
    paste("High")
  } else  if(time_stats_seas$Seasonal[p] >= 25 & time_stats_seas$Seasonal[p] <= 75 ){
    paste("Partial")
  } else  if(time_stats_seas$Seasonal[p] < 25){
    paste("Low")
  } else {
    paste("Partial")
  }
  
}

time_stats_seas <- time_stats_seas %>%
  pivot_longer(Seasonal, names_to= "Probability_Threshold", values_to = "TimePercentage")
warnings()

head(time_stats_seas)

## join back together and save
time_stats_all <- rbind(time_stats_seas, time_stats_ann, time_stats_willow)
write.csv(time_stats_all, "results/scenarios/X2_senarios_all_species_time_stats_updated_hyd.csv")

# Days per month ----------------------------------------------------------
# 
# ### do this later!!!!
# ### days per month
# td <- list.files("output_data/scenarios/", pattern="total_days_long_updated_hyd")
# length(td) ## 153
# td
# head(total_days)
# total_daysx <- NULL
# 
# 
# for(j in 1: length(td)) {
#   
#   
#   total_days <- read.csv(file=paste("output_data/scenarios/", td[1], sep=""))
#   head(total_days)
#   
#   total_days1 <- read.csv(file=paste("output_data/scenarios/", td[2], sep=""))
#   head(total_days1)
#   
# 
#   total_days <- total_days %>% 
#     select(-X, -month_year) %>%
#     # filter(Probability_Threshold == ann_stats) %>%
#     rename(TimePeriod = season,  DaysPerMonth = n_days) %>%
#     # mutate(Species = "SAS", Life_Stage = "Juvenile", Hydraulic = "Velocity") %>%
#     distinct()
#   
#   total_daysx <- rbind(total_daysx, total_days)
#   
# }
# 
# head(total_daysx)
# unique(total_daysx$Probability_Threshold)
# 
# 
# # ## change time period to seasonal and add bottom and water year type
# # 
# total_days_seas <- total_daysx %>%
#   filter(TimePeriod == "summer") %>%
#   mutate(bottom_type = ifelse(Node %in% concrete, "Concrete", "Soft")) %>%
#   distinct()
# total_days_seas
# 
# 
# total_days_seas <- total_days_seas %>%
#   pivot_wider(names_from = Probability_Threshold, values_from = DaysPerMonth) %>%
#   mutate(Suitability_Class = NA)
# 
# probs <- seq(1, dim(total_days_seas)[1], 1)  
# 
# 
# for(p in 1: length(probs)) {
#   
#   total_days_seas$Suitability_Class[p] = if(total_days_seas$High[p] >= 21) {
#     paste("High")
#   } else  if(total_days_seas$Medium[p] >= 7 & total_days_seas$Medium[p] <= 21 ){
#     paste("Partial")
#   } else  if(total_days_seas$Low[p] < 7){
#     paste("Low")
#   } else {
#     paste("Partial")
#   }
#   
# }
# total_days_seas
# # 
# # ### bind together and save
# total_days_all <- total_days_seas
# write.csv(total_days_all,"results/SAS_Velocity_Juvenile_total_days_updated_hyd.csv")
# 
# 
# 
# 
# 
# 
