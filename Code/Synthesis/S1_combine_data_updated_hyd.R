### combine for overall 
library(tidyr)
library(tidyverse)
getwd()
## upload all species suitability data
## upload data

getwd()
setwd("/Users/katieirving/Library/CloudStorage/OneDrive-SCCWRP/Documents - Katie’s MacBook Pro/git/LARiver_Sp_Model/results/")
## time stats
ts <- list.files(pattern="time_stats_updated_hyd")
length(ts) ## 15
ts
## curves
ts <- ts[c(1,3,4,5,6,11,12,14,15)] 
all_data <- NULL

s
## first 3 dfs have different format than 2nd 3, combine in sections 
for(s in 1: length(ts)) {
  
  time_stats <- read.csv(ts[s])
  head(time_stats)
  colnames(time_stats)[10:12] <- c("Low", "Medium", "High")
  all_data <- rbind(all_data, time_stats)
  
}
head(all_data)
head(time_stats)
all_data_first <- all_data

## time stats
ts <- list.files(pattern="time_stats_updated_hyd")
length(ts) ## 21

## thresholds
ts <- ts[-c(1,3,4,5,6,11,12,14,15)] 
all_data <- NULL

for(s in 1: length(ts)) {
  
  time_stats <- read.csv(ts[s])
  
  all_data <- rbind(all_data, time_stats)
  
}

## reformat and combine the 2 dfs together 

head(all_data)
head(all_data_first)


names(all_data_first)
names(all_data)
## reformat


all_data_first <- all_data_first %>%
  pivot_longer(Low:High, names_to = "Type", values_to="TimePercentage") %>%
  select(-X)

all_data <- all_data %>%
  rename(Type = Season) %>%
  select(-X)


all_datax <- bind_rows(all_data_first, all_data)

write.csv(all_datax, "S1_all_suitability_all_years.csv")
head(all_datax)
head(time_stats)
time_stats <- all_datax  %>%
  select(Species, Life_Stage, Node,Hydraulic, water_year,TimePeriod, position,
         Suitability_Class) %>%
  distinct()

time_stats$Suitability_Class <- as.factor(time_stats$Suitability_Class)

SuitClassOverYears <- time_stats %>%
  group_by(Species, Life_Stage, Hydraulic, TimePeriod, position, Node) %>%
  summarise(Overall_Class = tail(names(sort(table(Suitability_Class))),1))

SuitClassOverYears


write.csv(SuitClassOverYears, "S1_all_suitability_combined_years.csv")

## find highest suitability for each slice
head(all_datax)

SuitabilityPerSlice <- all_datax %>%
  group_by(Species, Life_Stage, Hydraulic, position, Node) %>%
  mutate(MaxPercentage = max(TimePercentage)) %>%
  mutate(MeanPercentage = mean(TimePercentage)) %>%
  select(-water_year,  - TimePercentage, -Suitability_Class) %>%
  distinct()

head(SuitabilityPerSlice)
unique(SuitabilityPerSlice$Life_Stage)

write.csv(SuitabilityPerSlice, "S1_suitability_per_slice.csv")

SuitabilityPerSlice <- read.csv("S1_suitability_per_slice.csv")
