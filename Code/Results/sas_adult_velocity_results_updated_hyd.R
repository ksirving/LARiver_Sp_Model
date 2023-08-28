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

ann_stats <- c("Low", "Medium", "High")

crit_stats <- c("Low.Seasonal", "Medium.Seasonal", "High.Seasonal")

concrete <- c("F34D", "F37B_High", "F45B", "F319", "LA13", "LA1")

### santa ana sucker
## velocity

## upload all time stats csvs

## time stats
ts <- list.files("ignore/Probs/", pattern="time_stats_updated_hyd")
length(ts) ## 219
ts
ts <- Filter(function(x) grepl("adult", x), ts)
ts <- Filter(function(x) grepl("SAS", x), ts)
ts <- Filter(function(x) grepl("velocity", x), ts)
ts
time_statsx <- NULL

for(j in 1: length(ts)) {
  
  time_stats <- read.csv(file=paste("ignore/Probs/", ts[j], sep=""))

  
  ######## juvenile
  time_stats <- time_stats %>% 
    select(-X) %>%
    # filter(Probability_Threshold == ann_stats) %>%
    rename(TimePeriod = season, TimePercentage = value) %>%
    # mutate(Species = "Santa Ana Sucker", Life_Stage = "Adult", Hydraulic = "Velocity", Node = paste(stringx[2])) %>%
    distinct()
  
  time_statsx <- rbind(time_statsx, time_stats)
  
}

head(time_statsx)
## change time period to anual and add bottom and water year type

time_stats_ann <- time_statsx %>%
  filter(Probability_Threshold %in% ann_stats) %>%
  mutate(TimePeriod = "Annual") %>%
  mutate(bottom_type = ifelse(Node %in% concrete, "Concrete", "Soft")) %>%
  distinct()

## calculate suitability

time_stats_ann <- time_stats_ann %>%
  pivot_wider(names_from = Probability_Threshold, values_from = TimePercentage) %>%
  mutate(Suitability_Class = NA)
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
time_stats_ann

## join back together and save
time_stats_all <- time_stats_ann
write.csv(time_stats_all, "results/SAS_Velocity_Adult_time_stats_updated_hyd.csv")

# Days per month ----------------------------------------------------------


### days per month
td <- list.files("ignore/Probs/", pattern="total_days_long_updated_hyd")
length(td) ## 153

td <- Filter(function(x) grepl("SAS", x), td)
td <- Filter(function(x) grepl("velocity", x), td)
td <- Filter(function(x) grepl("adult", x), td)
td

total_daysx <- NULL


for(j in 1: length(td)) {
  
  total_days <- read.csv(file=paste("ignore/Probs/", td[j], sep=""))

  
  ######## juvenile
  total_days <- total_days %>% 
    select(-X, -month_year) %>%
    # filter(Probability_Threshold == ann_stats) %>%
    rename(TimePeriod = season,  DaysPerMonth = n_days) %>%
    # mutate(Species = "Santa Ana Sucker", Life_Stage = "Adult", Hydraulic = "Velocity", Node = paste(stringx[2])) %>%
    distinct()
  
  total_daysx <- rbind(total_daysx, total_days)
  
}

head(total_daysx)
unique(total_daysx$Probability_Threshold)

## change time period to annual and add bottom and water year type

total_days_ann <- total_daysx %>%
  filter(Probability_Threshold %in% ann_stats) %>%
  mutate(TimePeriod = "Annual") %>%
  mutate(bottom_type = ifelse(Node %in% concrete, "Concrete", "Soft")) %>%
  distinct()


total_days_ann <- total_days_ann %>%
  pivot_wider(names_from = Probability_Threshold, values_from = DaysPerMonth) %>%
  mutate(Suitability_Class = NA)

probs <- seq(1, dim(total_days_ann)[1], 1)  


for(p in 1: length(probs)) {
  
  total_days_ann$Suitability_Class[p] = if(total_days_ann$High[p] >= 21) {
    paste("High")
  } else  if(total_days_ann$Medium[p] >= 7 & total_days_ann$Medium[p] <= 21 ){
    paste("Partial")
  } else  if(total_days_ann$Low[p] < 7){
    paste("Low")
  } else {
    paste("Partial")
  }
  
}
total_days_ann

total_days_all <- total_days_ann
write.csv(total_days_all,"results/SAS_Velocity_Adult_total_days_updated_hyd.csv")




