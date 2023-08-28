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
ts <- Filter(function(x) grepl("juvenile", x), ts)
ts <- Filter(function(x) grepl("SAS", x), ts)
ts <- Filter(function(x) grepl("depth", x), ts)
ts
time_statsx <- NULL
names(time_stats)

for(j in 1: length(ts)) {

  time_stats <- read.csv(file=paste("ignore/Probs/", ts[j], sep=""))
  ######## juvenile
  time_stats <- time_stats %>% 
    select(-X) %>%
    # filter(Probability_Threshold == ann_stats) %>%
    rename(TimePeriod = season, TimePercentage = value) %>%
    # mutate(Species = "Santa Ana Sucker", Life_Stage = "Juvenile", Hydraulic = "Velocity") %>%
    distinct()
  
  time_statsx <- rbind(time_statsx, time_stats)
  
}

head(time_statsx)

# ## change time period to seasonal and add bottom and water year type
# 
time_stats_seas <- time_statsx %>%
  filter(Probability_Threshold %in% crit_stats) %>%
  filter(TimePeriod == "summer") %>%
  mutate(TimePeriod = "critical") %>%
  mutate(bottom_type = ifelse(Node %in% concrete, "Concrete", "Soft")) %>%
  distinct()
time_stats_seas


## calculate suitability

time_stats_seas <- time_stats_seas %>%
  pivot_wider(names_from = Probability_Threshold, values_from = TimePercentage) %>%
  mutate(Suitability_Class = NA)
# group_by(Node, position, Species, Life_Stage, water_year) %>%

probs <- seq(1, dim(time_stats_seas)[1], 1)  


for(p in 1: length(probs)) {
  
  time_stats_seas$Suitability_Class[p] = if(time_stats_seas$High.Seasonal[p] >= 75) {
    paste("High")
  } else  if(time_stats_seas$Medium.Seasonal[p] >= 25 & time_stats_seas$Medium.Seasonal[p] <= 75 ){
    paste("Partial")
  } else  if(time_stats_seas$Low.Seasonal[p] < 25){
    paste("Low")
  } else {
    paste("Partial")
  }
  
}
time_stats_seas


head(time_stats_seas)

## join back together and save
time_stats_all <- time_stats_seas
write.csv(time_stats_all, "results/SAS_Depth_Juvenile_time_stats_updated_hyd.csv")

# Days per month ----------------------------------------------------------


### days per month
td <- list.files("ignore/Probs/", pattern="total_days_long_updated_hyd")
length(td) ## 153

td <- Filter(function(x) grepl("SAS", x), td)
td <- Filter(function(x) grepl("depth", x), td)
td <- Filter(function(x) grepl("juvenile", x), td)
td

total_daysx <- NULL


for(j in 1: length(td)) {

  total_days <- read.csv(file=paste("ignore/Probs/", td[j], sep=""))

  ######## juvenile
  total_days <- total_days %>% 
    select(-X, -month_year) %>%
    # filter(Probability_Threshold == ann_stats) %>%
    rename(TimePeriod = season,  DaysPerMonth = n_days) %>%
    # mutate(Species = "Santa Ana Sucker", Life_Stage = "Juvenile", Hydraulic = "Depth") %>%
    distinct()
  
  total_daysx <- rbind(total_daysx, total_days)
  
}

head(total_daysx)
unique(total_daysx$Probability_Threshold)


# ## change time period to seasonal and add bottom and water year type
# 
total_days_seas <- total_daysx %>%
  filter(TimePeriod == "summer") %>%
  mutate(TimePeriod = "critical") %>%
  mutate(bottom_type = ifelse(Node %in% concrete, "Concrete", "Soft")) %>%
  distinct()
total_days_seas


total_days_seas <- total_days_seas %>%
  pivot_wider(names_from = Probability_Threshold, values_from = DaysPerMonth) %>%
  mutate(Suitability_Class = NA)

probs <- seq(1, dim(total_days_seas)[1], 1)  


for(p in 1: length(probs)) {
  
  total_days_seas$Suitability_Class[p] = if(total_days_seas$High[p] >= 21) {
    paste("High")
  } else  if(total_days_seas$Medium[p] >= 7 & total_days_seas$Medium[p] <= 21 ){
    paste("Partial")
  } else  if(total_days_seas$Low[p] < 7){
    paste("Low")
  } else {
    paste("Partial")
  }
  
}
total_days_seas
# 
# ### bind together and save
total_days_all <- total_days_seas
write.csv(total_days_all,"results/SAS_Depth_Juvenile_total_days_updated_hyd.csv")




