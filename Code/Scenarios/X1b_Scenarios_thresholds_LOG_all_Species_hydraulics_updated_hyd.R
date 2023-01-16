

library(tidyverse)
library(tidyr)
library(sm)
library(lubridate) # work with dates
library(dplyr)     # data manipulation (filter, summarize, mutate)
library(ggplot2)   # graphics
library(gridExtra) # tile several plots next to each other
library(scales)
library(data.table)

## upload hydraulic data
setwd("/Users/katieirving/SCCWRP/LA River Eflows Study - General/Data/RawData/Results_Hydraulics/WRP_Scenarios/results-hydraulics_postprocessed")
dir <- "/Users/katieirving/SCCWRP/LA River Eflows Study - General/Data/RawData/Results_Hydraulics/WRP_Scenarios/results-hydraulics_postprocessed/"

h <- list.files(pattern="predictions")
length(h) ## 18
h
h <- h[-c(9,13)] ## remove LA1/ LA2

n=8
probs_function <- function(Model) {
  if(Model == "willow_StreamPower_adult") {
    probs <- expression(pnorm(willow_StreamPower_adultx, mean=mean(log(StreamPower)),
                              sd=sd(log(StreamPower))))
    
  } else if(Model == "Cladophora_Shear_adult" ) {
    probs <- expression(pnorm(Cladophora_Shear_adultx, mean=mean(log(shear)),
                              sd=sd(log(shear))))
    
  } else if(Model == "Steelhead_Depth_Prolonged") {
    probs <-  expression(pnorm(Steelhead_Depth_Prolongedx, mean=mean(log(depth_cm)),
                               sd=sd(log(depth_cm)), lower.tail = FALSE)) 
    
  } else if(Model == "Steelhead_Velocity_Prolonged") {
    probs <- expression(pnorm(Steelhead_Velocity_Prolongedx, mean=mean(log(Velocity)),
                              sd=sd(log(Velocity))))
    
  } else if(Model == "Steelhead_Depth_Burst") {
    probs <- expression(pnorm(Steelhead_Depth_Burstx, mean=mean(log(depth_cm)),
                              sd=sd(log(depth_cm)), lower.tail = FALSE))
    
  } else if (Model == "Steelhead_Velocity_Burst") {
    probs <- expression(pnorm(Steelhead_Velocity_Burstx, mean=mean(log(Velocity)),
                              sd=sd(log(Velocity))))
    
  } else if(Model == "Steelhead_Depth_smolts") {
    probs <- expression(pnorm(Steelhead_Depth_smoltsx, mean=mean(log(depth_cm)),
                              sd=sd(log(depth_cm)), lower.tail = FALSE)) 
    
  } else if(Model == "min_depth_limit") {
    probs <- expression(pnorm(min_depth_limitx, mean=mean(log(depth_cm)),
                              sd=sd(log(depth_cm)), lower.tail = FALSE)) 
  }
  return(probs)
  
}



## set wd back to main
setwd("/Users/katieirving/Documents/git/flow_eco_mech")
getwd()
## list thresholds

willow_StreamPower_adultx <- log(4000)
Cladophora_Shear_adultx <- log(16.9)
Steelhead_Depth_Prolongedx <- log(23)
Steelhead_Velocity_Prolongedx <- log(2)
Steelhead_Depth_Burstx <- log(18)
Steelhead_Velocity_Burstx <- log(3.1)
Steelhead_Depth_smoltsx <- log(12)

min_depth_limitx <- log(3)
h
n=3
# Combine with hydraulic data -------------------------------------------


for(n in 1: length(h)) {
  
  NodeData <- read.csv(file=paste(dir, h[n], sep=""))
  
  # head(NodeData)
  NodeName <- str_split(h[n], "_", 3)[[1]]
  NodeName <- NodeName[1]
  ## format hydraulic data
  
  
  cat(paste("Running Node", NodeName))
  
  # hydraul <-NodeData[,-1]
  
  ## change some names
  hydraul <- NodeData %>%
    rename(Q = Flow) %>%
    mutate(node = NodeName)
  
  # names(hydraul)
  ## convert units and change names - depending on concrete/soft bottom. if/else to determine changes to data
  
  if(length(NodeData) <= 9) {
    hyd_dep <- hydraul %>%
      mutate(depth_cm_MC = (Max..Depth..ft..MC*0.3048)*100) %>%
      mutate(Shear_pa_MC = (Shear..lb.sq.ft..MC/0.020885)) %>%
      mutate(StreamPower_w_MC = (Stream.Power..lb.ft.s..MC*4.44822)/0.3048) %>%
      mutate(velocity_m_MC = (Avg..Vel...ft.s..MC*0.3048)) %>%
      select(-contains("ft")) %>%
      mutate(date_num = seq(1,length(DateTime), 1))
  } else {
    hyd_dep <- hydraul %>%
      mutate(depth_cm_LOB = (Max..Depth..ft..LOB*0.3048)*100,
             depth_cm_MC = (Max..Depth..ft..MC*0.3048)*100,
             depth_cm_ROB = (Max..Depth..ft..ROB*0.3048)*100) %>%
      mutate(Shear_pa_LOB = (Shear..lb.sq.ft..LOB/0.020885),
             Shear_pa_MC = (Shear..lb.sq.ft..MC/0.020885),
             Shear_pa_ROB = (Shear..lb.sq.ft..ROB/0.020885)) %>%
      mutate(StreamPower_w_LOB = (Stream.Power..lb.ft.s..LOB*4.44822)/0.3048,
             StreamPower_w_MC = (Stream.Power..lb.ft.s..MC*4.44822)/0.3048,
             StreamPower_w_ROB = (Stream.Power..lb.ft.s..LOB*4.44822)/0.3048) %>%
      mutate(velocity_m_LOB = (Avg..Vel...ft.s..LOB*0.3048),
             velocity_m_MC = (Avg..Vel...ft.s..MC*0.3048),
             velocity_m_ROB = (Avg..Vel...ft.s..ROB*0.3048)) %>%
      select(-contains("ft")) %>%
      mutate(date_num = seq(1,length(DateTime), 1))
    
  }
  
  
  # ## melt channel position data
  all_data<-reshape2::melt(hyd_dep, id=c("Scenario", "DateTime","Q", "node", "date_num"))
  
  ## separate variable to get hydraulic, and position
  all_data <- all_data %>%
    separate(variable, c("Hydraulic", "Unit", "Position")) %>%
    unite(variable, Hydraulic:Unit, sep="_" )
  
  
  # format probability time series ------------------------------------------
  
  ## format date time
  all_data$DateTime<-as.POSIXct(all_data$DateTime,
                                format = "%m/%d/%Y",
                                tz = "GMT")
  # head(all_data)
  ## create year, month, day and hour columns and add water year
  
  all_data <- all_data %>%
    mutate(month = month(DateTime)) %>%
    mutate(year = year(DateTime)) %>%
    mutate(day = day(DateTime)) %>%
    # mutate(hour = hour(DateTime)) %>%
    mutate(water_year = ifelse(month == 10 | month == 11 | month == 12, year, year-1))
  
  
  
  ### define dataframes for 2nd loop
  
  # head(all_data)
  
  scenarios <- unique(all_data$Scenario)
  prob_statsx <- NULL
  
  s=315
  for(s in 1:length(scenarios)) {
    
    scen_data <- all_data %>% 
      filter(Scenario  == scenarios[s]) %>%
      pivot_wider(names_from = variable, values_from = value) %>%
      rename(shear = Shear_pa, Velocity = velocity_m, StreamPower = StreamPower_w) 
    
    
    # head(scen_data)
    # names(scen_data)
    
    scen_datax <- scen_data %>%
      mutate(willow_StreamPower_adult = willow_StreamPower_adultx, Cladophora_Shear_adult = Cladophora_Shear_adultx,
             Steelhead_Depth_Prolonged = Steelhead_Depth_Prolongedx,Steelhead_Velocity_Prolonged = Steelhead_Velocity_Prolongedx,
             Steelhead_Depth_Burst = Steelhead_Depth_Burstx, Steelhead_Velocity_Burst = Steelhead_Velocity_Burstx,
             Steelhead_Depth_smolts = Steelhead_Depth_smoltsx, min_depth_limit = min_depth_limitx)
    
    
    positions <- unique(scen_datax$Position)
    
    # probability as a function of discharge -----------------------------------
    
    p=1
    for(p in 1:length(positions)) {
      
      new_data <- scen_datax %>% 
        filter(Position  == positions[p])
      head(new_data)
      
      ## define position
      PositionName <- positions[p]
      # PositionName
      
      ### make long and split model name
      
      new_datax <- new_data %>%
        pivot_longer(willow_StreamPower_adult:min_depth_limit, names_to = "Type", values_to = "Threshold") %>%
        separate(Type, c("SpeciesName", "HydraulicName", "LifeStageName"), remove=F) 
      
      thresholds <- unique(new_datax$Type)
      thresholds
      
      # dataframe for stats -----------------------------------------------------
      l=3
      for (l in 1:length(thresholds)) {
        
        ## define critical period or season for adult as all year is critical
        thresholds_data <- new_datax %>%
          filter(Type == thresholds[l])
        
        Model <- thresholds[l]
        Model
        # LifeStageName <- unique(thresholds_data$LifeStageName)
        # # LifeStageName
        # ## define critical period 
        # if(LifeStageName == "Burst" || LifeStageName == "Prolonged") {
        #   critical <- c(12, 1,2,3,4, 5, 6) 
        #   # non_critical <- c(7:11)
        # } else if (LifeStageName == "smolts") {
        #   critical <- c(12, 1,2,3,4, 5, 6, 7) 
        #   # non_critical <- c(8:11)
        # } else if (LifeStageName == "fry" || LifeStageName == "juvenile")    {
        #   critical <- c(3:7) 
        #   # non_critical <- c(8:12, 1,2)
        # }  else if (LifeStageName == "adult" || LifeStageName == "Adult" || LifeStageName == "limit" ) {
        #   critical <- c(1:12) 
        #   # non_critical <- NA
        # } else { 
        #   critical <- c(4:9) 
        #   # non_critical <- c(10:12, 1:3)
        # }
        
        dry <- c(5,6,7,8,9)
        # critical## filter by season
        thresholds_data <- thresholds_data %>%
          mutate(season = ifelse(month %in% dry, "Dry", "Wet"))
        
        ## define hydraulic
        HydraulicName <- unique(thresholds_data$HydraulicName)
        # HydraulicName
        
        ## select only hydraulic from main df
        thresholds_datax <-  thresholds_data %>% 
          select(Scenario:water_year,  contains(HydraulicName), Type:season) 
        head(thresholds_datax)
        
        
        ## define probs function
        probs <- probs_function(Model)
        probs
        
        
        # head(prob_stats)
        ## caluclate probability
        prob_stats <- thresholds_datax %>%
          # filter(season == "critical") %>%
          group_by(water_year,SpeciesName, HydraulicName, LifeStageName, Type, season) %>%
          mutate(MeanProbabilityx = eval(probs)) %>%
          summarise(MeanProbability = mean(MeanProbabilityx), SDProbability = sd(MeanProbabilityx)) %>%
          mutate(Position = PositionName, Scenario = scenarios[s]) %>%
          distinct()
        
        prob_statsx <- rbind(prob_statsx, prob_stats)
        
      } ## end life stage loop
      
      
      
    } ## end positions loop
    
    
    
  }   ## end scenario loop
  
  
  
  write.csv(prob_statsx, paste("results/scenarios/probs/X1b",NodeName, "all_species_thresholds_WRP_scenarios_probs.csv", sep="_"))
  
} ## end node loop

