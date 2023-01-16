### temp analysis

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
n=1
## change wd back 
setwd("/Users/katieirving/Documents/git/flow_eco_mech")
all_probsx <- NULL
n

## start loop
for(n in 1: length(tp)) {
  ## temp data
  TempData <- read.csv(file=paste("input_data/LAR_WaterTemperature/", tp[n], sep=""))
  TempData
  
  ### species data
  fitdata <- read.csv("output_data/adult_temp_prob_curve_data.csv")
  
  ## use smooth spline to predict on new data set
  new_values <-smooth.spline(fitdata$temp_fit, fitdata$prob_fit)
  
  all_data_max <- TempData %>%
    # group_by(variable) %>%
    mutate(prob_fit = predict(new_values, Max_Temp)$y)%>%
    mutate(Low = sum(prob_fit > 0.1), Med=   sum(prob_fit > 0.2), High = sum(prob_fit > 0.3)) %>%
    select(Node, Low, Med, High) %>%
    mutate(Metric = "MaxTemp", No_of_Weeks = length(Node)) %>%
    distinct()
  
  
  all_data_max_mean <- TempData %>%
    # group_by(variable) %>%
    mutate(prob_fit = predict(new_values, Mean_Max_Temp)$y) %>% 
    mutate(Low = sum(prob_fit > 0.1), Med=   sum(prob_fit > 0.2), High = sum(prob_fit > 0.3)) %>%
    select(Node, Low, Med, High) %>%
    mutate(Metric = "MeanMaxTemp", No_of_Weeks = length(Node)) %>%
    distinct()
  
  all_data_min <- TempData %>%
    # group_by(variable) %>%
    mutate(prob_fit = predict(new_values, Min_Temp)$y) %>% 
    mutate(Low = sum(prob_fit > 0.1), Med=   sum(prob_fit > 0.2), High = sum(prob_fit > 0.3)) %>%
    select(Node, Low, Med, High) %>%
    mutate(Metric = "MinTemp", No_of_Weeks = length(Node)) %>%
    distinct()
  
  all_data_mean <- TempData %>%
    # group_by(variable) %>%
    mutate(prob_fit = predict(new_values, Mean_Temp)$y) %>% 
    mutate(Low = sum(prob_fit > 0.1), Med=   sum(prob_fit > 0.2), High = sum(prob_fit > 0.3)) %>%
    select(Node, Low, Med, High) %>%
    mutate(Metric = "MeanTemp", No_of_Weeks = length(Node)) %>%
    distinct()
  
  all_probs <- rbind(all_data_max, all_data_max_mean, all_data_min, all_data_mean)
  
  
  all_probsx <- rbind(all_probsx, all_probs)
}

all_probsx <- all_probsx %>%
  mutate(Species = "SAS", Life_Stage = "Adult")

all_probsx

write.csv(all_probsx, file="output_data/T1_SAS_Adult_Temperature_probs.csv")



# Juvenile ----------------------------------------------------------------

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
n=1
## change wd back 
setwd("/Users/katieirving/Documents/git/flow_eco_mech")
all_probsx <- NULL
n

## start loop
for(n in 1: length(tp)) {
  ## temp data
  TempData <- read.csv(file=paste("input_data/LAR_WaterTemperature/", tp[n], sep=""))
  TempData
  
  ### species data
  fitdata <- read.csv("output_data/juvenile_temp_prob_curve_data.csv")
  
  ## use smooth spline to predict on new data set
  new_values <-smooth.spline(fitdata$temp_fit, fitdata$prob_fit)
  
  all_data_max <- TempData %>%
    # group_by(variable) %>%
    mutate(prob_fit = predict(new_values, Max_Temp)$y)%>%
    mutate(Low = sum(prob_fit > 0.1), Med=   sum(prob_fit > 0.2), High = sum(prob_fit > 0.3)) %>%
    select(Node, Low, Med, High) %>%
    mutate(Metric = "MaxTemp", No_of_Weeks = length(Node)) %>%
    distinct()
  
  
  all_data_max_mean <- TempData %>%
    # group_by(variable) %>%
    mutate(prob_fit = predict(new_values, Mean_Max_Temp)$y) %>% 
    mutate(Low = sum(prob_fit > 0.1), Med=   sum(prob_fit > 0.2), High = sum(prob_fit > 0.3)) %>%
    select(Node, Low, Med, High) %>%
    mutate(Metric = "MeanMaxTemp", No_of_Weeks = length(Node)) %>%
    distinct()
  
  
  all_data_min <- TempData %>%
    # group_by(variable) %>%
    mutate(prob_fit = predict(new_values, Min_Temp)$y) %>% 
    mutate(Low = sum(prob_fit > 0.1), Med=   sum(prob_fit > 0.2), High = sum(prob_fit > 0.3)) %>%
    select(Node, Low, Med, High) %>%
    mutate(Metric = "MinTemp", No_of_Weeks = length(Node)) %>%
    distinct()
  
  all_data_mean <- TempData %>%
    # group_by(variable) %>%
    mutate(prob_fit = predict(new_values, Mean_Temp)$y) %>% 
    mutate(Low = sum(prob_fit > 0.1), Med=   sum(prob_fit > 0.2), High = sum(prob_fit > 0.3)) %>%
    select(Node, Low, Med, High) %>%
    mutate(Metric = "MeanTemp", No_of_Weeks = length(Node)) %>%
    distinct()
  
  all_probs <- rbind(all_data_max, all_data_max_mean, all_data_min, all_data_mean)
  
  
  all_probsx <- rbind(all_probsx, all_probs)
}

all_probsx <- all_probsx %>%
  mutate(Species = "SAS", Life_Stage = "Juvenile")

all_probsx

write.csv(all_probsx, file="output_data/T1_SAS_Juvenile_Temperature_probs.csv")


# Fry ----------------------------------------------------------------

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
n=1
## change wd back 
setwd("/Users/katieirving/Documents/git/flow_eco_mech")
all_probsx <- NULL
n

## start loop
for(n in 1: length(tp)) {
  ## temp data
  TempData <- read.csv(file=paste("input_data/LAR_WaterTemperature/", tp[n], sep=""))
  TempData


  all_data_max <- TempData %>%
    # group_by(variable) %>%
    mutate(Thresh = ifelse(Max_Temp <= 24, "Suitable", "Unsuitable")) %>%
    mutate(Suitable = sum(Thresh == "Suitable"), Unsuitable = sum(Thresh == "Unsuitable")) %>%
    select(Node, Suitable, Unsuitable) %>%
    mutate(Metric = "MaxTemp") %>%
    distinct()
  
  
  all_data_max_mean <- TempData %>%
    # group_by(variable) %>%
    mutate(Thresh = ifelse(Mean_Max_Temp <= 24, "Suitable", "Unsuitable")) %>%
    mutate(Suitable = sum(Thresh == "Suitable"), Unsuitable = sum(Thresh == "Unsuitable")) %>%
    select(Node, Suitable, Unsuitable) %>%
    mutate(Metric = "MeanMaxTemp") %>%
    distinct()
  

  all_data_min <- TempData %>%
    # group_by(variable) %>%
    mutate(Thresh = ifelse(Min_Temp <= 24, "Suitable", "Unsuitable")) %>%
    mutate(Suitable = sum(Thresh == "Suitable"), Unsuitable = sum(Thresh == "Unsuitable")) %>%
    select(Node, Suitable, Unsuitable) %>%
    mutate(Metric = "MinTemp") %>%
    distinct()
  
  all_data_mean <- TempData %>%
    # group_by(variable) %>%
    mutate(Thresh = ifelse(Mean_Temp <= 24, "Suitable", "Unsuitable")) %>%
    mutate(Suitable = sum(Thresh == "Suitable"), Unsuitable = sum(Thresh == "Unsuitable")) %>%
    select(Node, Suitable, Unsuitable) %>%
    mutate(Metric = "MeanTemp") %>%
    distinct()
  
  all_probs <- rbind(all_data_max, all_data_max_mean, all_data_min, all_data_mean)

  
  all_probsx <- rbind(all_probsx, all_probs)
}

all_probsx <- all_probsx %>%
  mutate(Species = "SAS", Life_Stage = "Fry")

all_probsx

write.csv(all_probsx, file="output_data/T1_SAS_Fry_Temperature_probs.csv")



