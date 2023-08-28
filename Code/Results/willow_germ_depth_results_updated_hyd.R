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

## upload all time stats csvs

## time stats
ts <- list.files("ignore/Probs/", pattern="time_stats_updated_hyd")
length(ts) ## 219
ts
ts <- Filter(function(x) grepl("Germination", x), ts)
ts <- Filter(function(x) grepl("Willow", x), ts)


time_statsx <- NULL

head(time_stats)


for(j in 1: length(ts)) {
  
  time_stats <- read.csv(file=paste("ignore/Probs/", ts[j], sep=""))
  
  ######## juvenile
  time_stats <- time_stats %>% 
    select(-X) %>%
    # filter(Probability_Threshold == ann_stats) %>%
    rename(TimePeriod = season, TimePercentage = value) %>%
    # mutate(Species = "Willow", Life_Stage = "Germination", Hydraulic = "Depth", Node = paste(stringx[2])) %>%
    distinct()
  
  time_statsx <- rbind(time_statsx, time_stats)
  
}

head(time_statsx)
unique(time_statsx$TimePeriod)
## change time period to seasonal and add bottom and water year type

time_stats_seas <- time_statsx %>%
  filter(TimePeriod == "critical") %>%
  # rename(TimePercentage = Seasonal) %>%
  mutate(bottom_type = ifelse(Node %in% concrete, "Concrete", "Soft")) %>%
  distinct()
time_stats_seas

## calculate suitability

probs <- seq(1, length(time_stats_seas$TimePeriod), 1)  

for(p in 1: length(probs)) {
  
  time_stats_seas$Suitability_Class[p] <- if(time_stats_seas$TimePercentage[p] >=24 && 
                                             time_stats_seas$TimePercentage[p] <= 76) {
    paste("High") 
    
  } else if (time_stats_seas$TimePercentage[p] <=24 || 
             time_stats_seas$TimePercentage[p] >= 76) {
    paste("Low") 

  } else {
    paste("Partial")
  }
  
}



head(time_stats_seas)
head(time_stats_ann)

## join back together and save
time_stats_all <- time_stats_seas
write.csv(time_stats_all, "results/Willow_Germination_Depth_time_stats_updated_hyd.csv")

# Days per month ----------------------------------------------------------


### days per month
td <- list.files("ignore/Probs/", pattern="total_days_long_updated_hyd")
length(td) ## 153

td <- Filter(function(x) grepl("Germination", x), td)
td <- Filter(function(x) grepl("Willow", x), td)

td

total_daysx <- NULL
head(total_days)

unique(total_days$season)


for(j in 1: length(td)) {
  
  total_days <- read.csv(file=paste("ignore/Probs/", td[j], sep=""))
  
  ######## juvenile
  total_days <- total_days %>% 
    select(-X, -month_year) %>%
    # filter(Probability_Threshold == ann_stats) %>%
    # mutate(season = ifelse(month %in% non_critical, "non_critical", "critical")) %>%
    rename(TimePeriod = season,  DaysPerMonth = n_days) %>%
    # mutate(Species = "Willow", Life_Stage = "Germination", Hydraulic = "Depth", Node = paste(stringx[2])) %>%
    distinct()
  
  total_daysx <- rbind(total_daysx, total_days)
  
}

head(total_daysx)

## change time period to seasonal and add bottom and water year type

total_days_seas <- total_daysx %>%
  filter(TimePeriod == "critical") %>%
  mutate(bottom_type = ifelse(Node %in% concrete, "Concrete", "Soft")) %>%
  distinct()

## add water year 
total_days_seas <- full_join(total_days_seas, water_year_type, by = "water_year")
total_days_seas <- na.omit(total_days_seas)

## suitability
head(total_days_seas)

probs <- seq(1, length(total_days_seas$TimePeriod), 1)  

for(p in 1: length(probs)) {
  
  total_days_seas$Suitability_Class[p] <- if(total_days_seas$DaysPerMonth[p] >= 8 &&
                                            total_days_seas$DaysPerMonth[p] <= 23) {
    paste("High") 
    
  } else if (total_days_seas$DaysPerMonth[p] <= 8 ||
             total_days_seas$DaysPerMonth[p] >= 23) {
    
    paste("Low") 
    
  } else {
    paste("Partial")
  }
  
}

head(total_days_ann)
head(total_days_seas)
### bind together and save
total_days_all <- total_days_seas
write.csv(total_days_all, "results/Willow_Germination_Depth_total_days_updated_hyd.csv")

total_days_all
tail(total_days_all)

