

library(tidyverse)
library(tidyr)
library(sm)
library(lubridate) # work with dates
library(dplyr)     # data manipulation (filter, summarize, mutate)
library(ggplot2)   # graphics
library(gridExtra) # tile several plots next to each other
library(scales)
library(data.table)

all_na <- function(x) any(!is.na(x))

## upload hydraulic data
setwd("/Users/katieirving/SCCWRP/LA River Eflows Study - General/Data/RawData/Results_Hydraulics/Tidal_LA1LA2/WRP/results-hydraulics_postprocessed")
dir <- "/Users/katieirving/SCCWRP/LA River Eflows Study - General/Data/RawData/Results_Hydraulics/Tidal_LA1LA2/WRP/results-hydraulics_postprocessed/"

h <- list.files(pattern="_predictions")
length(h) ## 18
h
# h <- h[c(9,13)] ## LA1/ LA2

# n=7

## set wd back to main
setwd("/Users/katieirving/Documents/git/flow_eco_mech/models")

## list models
M <- list.files()

load(file=M[1])
clad_depth_mod <- depth_lmq
clad_depth_mod ## depth_cm
load(file=M[2]) ## clad_vel_mod
clad_vel_mod ## Velocity
load(file=M[3]) 
dep_ptch_mdl ## depth_cm
load(file=M[4]) 
dep_sdlng_mdl ## depth_cm
load(file=M[5]) 
depth_seedling_mod ## depth_cm
load(file=M[6]) 
shear_seedling ## shear
load(file=M[7]) 
vel_ptch_mdl ## vel_m_s

setwd("/Users/katieirving/Documents/git/flow_eco_mech")

adultdepth <- read.csv("output_data/old_data/adult_depth_prob_curve_data.csv")
juvdepth <- read.csv("output_data/old_data/juvenile_depth_prob_curve_data.csv")
adultvelocity <- read.csv("output_data/old_data/adult_velocity_prob_curve_data.csv")
adultvelocity
## use smooth spline to predict on new data set
SAS_Depth_adult_mod <-smooth.spline(adultdepth$depth_fit, adultdepth$prob_fit)
SAS_Velocity_adult_mod <-smooth.spline(adultvelocity$velocity_fit, adultvelocity$prob_fit)
SAS_Depth_juvenile_mod <-smooth.spline(juvdepth$depth_fit, juvdepth$prob_fit)
head(SAS_Depth_adult_mod)

# Combine with hydraulic data -------------------------------------------
n=1

for(n in 1: length(h)) {
  
  # NodeData <- read.csv(file=paste("input_data/HecRas/", h[n], sep=""))
  NodeData <- read.csv(file=paste(dir, h[n], sep=""))
  ## remove columns all NAs
  NodeData <- NodeData %>% select_if(all_na) 
  head(NodeData)
  dim(NodeData)
  # F34D <- read.csv("input_data/HecRas/hydraulic_ts_F34D.csv") ## for dates
  
  NodeName <- str_split(h[n], "_", 3)[[1]]
  NodeName <- NodeName[1]
  ## format hydraulic data
  
  
  cat(paste("Running Node", NodeName))
  
  # NodeData <- NodeData %>%
  #   mutate(DateTime = F34D$Q_ts.datetime)
  
  hydraul <-NodeData
  
  ## change some names
  hydraul <- hydraul %>%
    rename(Q = Flow) %>%
    mutate(node = NodeName)
  
  hydraul[is.na(hydraul)] = 0
  ## convert units and change names - depending on concrete/soft bottom. if/else to determine changes to data
  
  if(length(hydraul) == 14) {
    hyd_dep <- hydraul %>%
      mutate(depth_cm_MC = (Max..Depth..ft..MC*0.3048)*100) %>%
      mutate(shear_pa_MC = (Shear..lb.sq.ft..MC/0.020885)) %>%
      mutate(StreamPower_w_MC = abs(Stream.Power..lb.ft.s..MC*4.44822)/0.3048) %>%
      mutate(vel_m_MC = abs(Avg..Vel...ft.s..MC*0.3048)) %>%
      select(-contains("ft")) %>%
      mutate(date_num = seq(1,length(DateTime), 1)) %>%
      select(Q, DateTime, node, Scenario, depth_cm_MC:date_num)
  } else {
    hyd_dep <- hydraul %>%
      mutate(depth_cm_LOB = (Max..Depth..ft..LOB*0.3048)*100,
             depth_cm_MC = (Max..Depth..ft..MC*0.3048)*100,
             depth_cm_ROB = (Max..Depth..ft..ROB*0.3048)*100) %>%
      mutate(shear_pa_LOB = (Shear..lb.sq.ft..LOB/0.020885),
             shear_pa_MC = (Shear..lb.sq.ft..MC/0.020885),
             shear_pa_ROB = (Shear..lb.sq.ft..ROB/0.020885)) %>%
      mutate(StreamPower_w_LOB = abs(Stream.Power..lb.ft.s..LOB*4.44822)/0.3048,
             StreamPower_w_MC = abs(Stream.Power..lb.ft.s..MC*4.44822)/0.3048,
             StreamPower_w_ROB = abs(Stream.Power..lb.ft.s..LOB*4.44822)/0.3048) %>%
      mutate(vel_m_LOB = abs(Avg..Vel...ft.s..LOB*0.3048),
             vel_m_MC = abs(Avg..Vel...ft.s..MC*0.3048),
             vel_m_ROB = abs(Avg..Vel...ft.s..ROB*0.3048)) %>%
      select(-contains("ft")) %>%
      mutate(date_num = seq(1,length(DateTime), 1)) %>%
      select(Q, DateTime, node, Scenario, depth_cm_LOB:date_num)
    
    
  }
  
  # unique(hyd_dep$DateTime)
  
  # ## melt channel position data
  all_data<-reshape2::melt(hyd_dep, id=c("Scenario", "DateTime","Q", "node", "date_num"))
  
  ## separate variable to get hydraulic, and position
  all_data <- all_data %>%
    separate(variable, c("Hydraulic", "Unit", "Position")) %>%
    unite(variable, Hydraulic:Unit, sep="_" )
  
  
  # format probability time series ------------------------------------------
  
  ## format date time
  all_data$DateTime<-as.POSIXct(all_data$DateTime,
                                format = "%m/%d/%Y %H:%M",
                                tz = "GMT")
  head(all_data)
  
  ## create year, month, day and hour columns and add water year
  
  all_data <- all_data %>%
    mutate(month = month(DateTime)) %>%
    mutate(year = year(DateTime)) %>%
    mutate(day = day(DateTime)) %>%
    mutate(hour = hour(DateTime)) %>%
    mutate(water_year = ifelse(month == 10 | month == 11 | month == 12, year, year-1))
  
  # sum(is.na(all_data))
  ### define dataframes for 2nd loop
  
  # head(all_data)
  unique(all_data$variable)
  scenarios <- unique(all_data$Scenario)
  prob_statsx <- NULL
  
  s=1
  for(s in 1:length(scenarios)) {
    
    scen_data <- all_data %>% 
      filter(Scenario  == scenarios[s]) %>%
      pivot_wider(names_from = variable, values_from = value) %>%
      rename(shear = shear_pa, Velocity = vel_m, StreamPower = StreamPower_w) %>%
      mutate(vel_m_s = Velocity)
    
    
    ## typha, willow, cladophora
    scen_datax <- scen_data %>%
      # group_by(Position) %>%
      
      mutate(Cladophora_depth_adult = predict(clad_depth_mod, newdata = scen_data, type="response")) %>%
      mutate(Cladophora_depth_adult = ifelse(Cladophora_depth_adult<=0, 0, Cladophora_depth_adult)) %>%## predicts negative percentages - cut off at 0 for quick fix
      mutate(Cladophora_depth_adult = Cladophora_depth_adult/100) %>%## standardise percentages
      
      mutate(Cladophora_velocity_adult = predict(clad_vel_mod, newdata = scen_data, type="response")) %>%
      mutate(Cladophora_velocity_adult = ifelse(Cladophora_velocity_adult<=0, 0, Cladophora_velocity_adult)) %>%
      
      mutate(Typha_depth_adult = predict(dep_ptch_mdl, newdata = scen_data, type="response")) %>%
      mutate(Typha_depth_adult = ifelse(Typha_depth_adult<=0, 0, Typha_depth_adult)) %>%
      
      mutate(Typha_depth_seedling = predict(dep_sdlng_mdl, newdata = scen_data, type="response")) %>%
      mutate(Typha_depth_seedling = ifelse(Typha_depth_seedling<=0, 0, Typha_depth_seedling)) %>%
      mutate(Typha_depth_seedling = Typha_depth_seedling/100) %>%
      
      mutate(willow_depth_seedling = predict(depth_seedling_mod, newdata = scen_data, type="response")) %>%
      mutate(willow_depth_seedling = ifelse(willow_depth_seedling<=0, 0, willow_depth_seedling)) %>%
      mutate(willow_depth_seedling = ifelse(willow_depth_seedling >=100 , 100, willow_depth_seedling)) %>%
      mutate(willow_depth_seedling = 1-(willow_depth_seedling/100)) %>%## standardise and covert to occurrence/survival
      
      mutate(willow_shear_seedling = predict(shear_seedling, newdata = scen_data, type="response")) %>%
      mutate(willow_shear_seedling = ifelse(willow_shear_seedling<=0, 0, willow_shear_seedling)) %>%
      mutate(willow_shear_seedling = ifelse(willow_shear_seedling >=100 , 100, willow_shear_seedling)) %>%
      mutate(willow_shear_seedling = 1-(willow_shear_seedling/100)) %>%
      
      mutate(Typha_velocity_adult = predict(vel_ptch_mdl, newdata = scen_data, type="response")) %>%
      mutate(Typha_velocity_adult = ifelse(Typha_velocity_adult<=0, 0, Typha_velocity_adult)) #%>%
    
    
    ## SAS
    scen_datax <- scen_datax %>%
      group_by(Position) %>%
      mutate(SAS_Depth_adult = predict(SAS_Depth_adult_mod, depth_cm)$y) %>%
      mutate(SAS_Depth_adult = ifelse(SAS_Depth_adult < 0.1, 0.1, SAS_Depth_adult)) %>%
      mutate(SAS_Depth_juvenile = predict(SAS_Depth_juvenile_mod, depth_cm)$y) %>%
      mutate(SAS_Depth_juvenile = ifelse(SAS_Depth_juvenile < 0.1, 0.1, SAS_Depth_juvenile)) %>%
      mutate(SAS_Velocity_adult = predict(SAS_Velocity_adult_mod, Velocity)$y) %>%
      mutate(SAS_Velocity_adult = ifelse(SAS_Velocity_adult < 0, 0, SAS_Velocity_adult))
    
    names(scen_datax)
    
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
        pivot_longer(Cladophora_depth_adult:SAS_Velocity_adult, names_to = "Type", values_to = "Probability") %>%
        separate(Type, c("SpeciesName", "HydraulicName", "LifeStageName"), remove=F) %>%
        mutate(Probability = ifelse(Probability >1, 1, Probability))
      # head(new_datax)
      # lifestages <- unique(new_datax$LifeStageName)
      
      # dataframe for stats -----------------------------------------------------
      # l=1
      # for (l in 1:length(lifestages)) {
      
      ## define critical period or season for adult as all year is critical
      # lifestage_data <- new_datax %>%
      #   filter(LifeStageName == lifestages[l])
      # 
      # LifeStageName <- unique(lifestage_data$LifeStageName)
      # LifeStageName
      # ## define critical period 
      # if(LifeStageName == "Migration" || LifeStageName == "Prolonged") {
      #   critical <- c(12, 1,2,3,4, 5, 6) 
      #   # non_critical <- c(7:11)
      # } else if (LifeStageName == "Smolts") {
      #   critical <- c(12, 1,2,3,4, 5, 6, 7) 
      #   # non_critical <- c(8:11)
      # } else if (LifeStageName == "fry" || LifeStageName == "juvenile")    {
      #   critical <- c(3:7) 
      #   # non_critical <- c(8:12, 1,2)
      # }  else if (LifeStageName == "adult" || LifeStageName == "Adult") {
      #   critical <- c(1:12) 
      #   # non_critical <- NA
      # } else { 
      #   critical <- c(4:9) 
      #   # non_critical <- c(10:12, 1:3)
      # }
      
      dry <- c(5,6,7,8,9)
      
      new_datax <- new_datax %>%
        mutate(season = ifelse(month %in% dry, "Dry", "Wet"))
      
      # head(new_datax)
      
      prob_stats <- new_datax %>%
        # filter(season == "critical") %>%
        group_by(water_year,SpeciesName, HydraulicName, LifeStageName, Type, season) %>%
        summarise(MeanProbability = mean(Probability), SDProbability = sd(Probability)) %>%
        # summarise(SDProbability = sd(Probability)) %>%
        mutate(Position = PositionName, Scenario = scenarios[s])
      
      head(prob_stats)
      prob_statsx <- rbind(prob_statsx, prob_stats)
      
      # } ## end life stage loop
      
      
      
    } ## end positions loop
    
    
    
  }   ## end scenario loop
  
  
  write.csv(prob_statsx, paste("results/scenarios/probs/X1a",NodeName, "all_species_probs_WRP_scenarios_probs.csv", sep="_"))
  
} ## end node loop


# thresholds --------------------------------------------------------------



# n=1
probs_function <- function(Model) {
  if(Model == "willow_StreamPower_adult") {
    probs <- expression(pnorm(willow_StreamPower_adultx, mean=mean(StreamPower),
                              sd=sd(StreamPower)))
    
  } else if(Model == "Cladophora_Shear_adult" ) {
    probs <- expression(pnorm(Cladophora_Shear_adultx, mean=mean(shear),
                              sd=sd(shear)))
    
  } else if(Model == "Steelhead_Depth_Prolonged") {
    probs <-  expression(pnorm(Steelhead_Depth_Prolongedx, mean=mean(depth_cm),
                               sd=sd(depth_cm), lower.tail = FALSE)) 
    
  } else if(Model == "Steelhead_Velocity_Prolonged") {
    probs <- expression(pnorm(Steelhead_Velocity_Prolongedx, mean=mean(Velocity),
                              sd=sd(Velocity)))
    
  } else if(Model == "Steelhead_Depth_Burst") {
    probs <- expression(pnorm(Steelhead_Depth_Burstx, mean=mean(depth_cm),
                              sd=sd(depth_cm), lower.tail = FALSE))
    
  } else if (Model == "Steelhead_Velocity_Burst") {
    probs <- expression(pnorm(Steelhead_Velocity_Burstx, mean=mean(Velocity),
                              sd=sd(Velocity)))
    
  } else if(Model == "Steelhead_Depth_smolts") {
    probs <- expression(pnorm(Steelhead_Depth_smoltsx, mean=mean(depth_cm),
                              sd=sd(depth_cm), lower.tail = FALSE)) 
    
  } else if(Model == "min_depth_limit") {
    probs <- expression(pnorm(min_depth_limitx, mean=mean(depth_cm),
                              sd=sd(depth_cm), lower.tail = FALSE)) 
  }
  return(probs)
  
}

min_limit <- expression(pnorm(min_depth_limitx, mean=mean(depth_cm),
                              sd=sd(depth_cm))) 

## set wd back to main
setwd("/Users/katieirving/Documents/git/flow_eco_mech")
getwd()
## list thresholds

willow_StreamPower_adultx <- 4000
Cladophora_Shear_adultx <- 16.9
Steelhead_Depth_Prolongedx <- 23
Steelhead_Velocity_Prolongedx <- 2
Steelhead_Depth_Burstx <- 18
Steelhead_Velocity_Burstx <- 3.1
Steelhead_Depth_smoltsx <- 12

min_depth_limitx <- 3


# Combine with hydraulic data -------------------------------------------


for(n in 1: length(h)) {
  
  # NodeData <- read.csv(file=paste("input_data/HecRas/", h[n], sep=""))
  NodeData <- read.csv(file=paste(dir, h[n], sep=""))
  ## remove columns all NAs
  NodeData <- NodeData %>% select_if(all_na) 
  head(NodeData)
  dim(NodeData)
  # F34D <- read.csv("input_data/HecRas/hydraulic_ts_F34D.csv") ## for dates
  
  NodeName <- str_split(h[n], "_", 3)[[1]]
  NodeName <- NodeName[1]
  ## format hydraulic data
  
  
  cat(paste("Running Node", NodeName))
  
  # NodeData <- NodeData %>%
  #   mutate(DateTime = F34D$Q_ts.datetime)
  
  hydraul <-NodeData
  
  ## change some names
  hydraul <- hydraul %>%
    rename(Q = Flow) %>%
    mutate(node = NodeName)
  
  hydraul[is.na(hydraul)] = 0
  ## convert units and change names - depending on concrete/soft bottom. if/else to determine changes to data
  
  if(length(hydraul) == 14) {
    hyd_dep <- hydraul %>%
      mutate(depth_cm_MC = (Max..Depth..ft..MC*0.3048)*100) %>%
      mutate(shear_pa_MC = (Shear..lb.sq.ft..MC/0.020885)) %>%
      mutate(StreamPower_w_MC = abs(Stream.Power..lb.ft.s..MC*4.44822)/0.3048) %>%
      mutate(vel_m_MC = abs(Avg..Vel...ft.s..MC*0.3048)) %>%
      select(-contains("ft")) %>%
      mutate(date_num = seq(1,length(DateTime), 1)) %>%
      select(Q, DateTime, node, Scenario, depth_cm_MC:date_num)
  } else {
    hyd_dep <- hydraul %>%
      mutate(depth_cm_LOB = (Max..Depth..ft..LOB*0.3048)*100,
             depth_cm_MC = (Max..Depth..ft..MC*0.3048)*100,
             depth_cm_ROB = (Max..Depth..ft..ROB*0.3048)*100) %>%
      mutate(shear_pa_LOB = (Shear..lb.sq.ft..LOB/0.020885),
             shear_pa_MC = (Shear..lb.sq.ft..MC/0.020885),
             shear_pa_ROB = (Shear..lb.sq.ft..ROB/0.020885)) %>%
      mutate(StreamPower_w_LOB = abs(Stream.Power..lb.ft.s..LOB*4.44822)/0.3048,
             StreamPower_w_MC = abs(Stream.Power..lb.ft.s..MC*4.44822)/0.3048,
             StreamPower_w_ROB = abs(Stream.Power..lb.ft.s..LOB*4.44822)/0.3048) %>%
      mutate(vel_m_LOB = abs(Avg..Vel...ft.s..LOB*0.3048),
             vel_m_MC = abs(Avg..Vel...ft.s..MC*0.3048),
             vel_m_ROB = abs(Avg..Vel...ft.s..ROB*0.3048)) %>%
      select(-contains("ft")) %>%
      mutate(date_num = seq(1,length(DateTime), 1)) %>%
      select(Q, DateTime, node, Scenario, depth_cm_LOB:date_num)
    
    
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
                                format = "%m/%d/%Y %H:%M",
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
  
  # s=1
  for(s in 1:length(scenarios)) {
    
    scen_data <- all_data %>% 
      filter(Scenario  == scenarios[s]) %>%
      pivot_wider(names_from = variable, values_from = value) %>%
      rename(shear = shear_pa, Velocity = vel_m, StreamPower = StreamPower_w)
    
    
    # head(scen_data)
    # names(scen_data)
    
    scen_datax <- scen_data %>%
      mutate(willow_StreamPower_adult = willow_StreamPower_adultx, Cladophora_Shear_adult = Cladophora_Shear_adultx,
             Steelhead_Depth_Prolonged = Steelhead_Depth_Prolongedx,Steelhead_Velocity_Prolonged = Steelhead_Velocity_Prolongedx,
             Steelhead_Depth_Burst = Steelhead_Depth_Burstx, Steelhead_Velocity_Burst = Steelhead_Velocity_Burstx,
             Steelhead_Depth_smolts = Steelhead_Depth_smoltsx, min_depth_limit = min_depth_limitx)
    
    
    positions <- unique(scen_datax$Position)
    
    # probability as a function of discharge -----------------------------------
    
    # p=1
    for(p in 1:length(positions)) {
      
      new_data <- scen_datax %>% 
        filter(Position  == positions[p])
      # head(new_data)
      
      ## define position
      PositionName <- positions[p]
      # PositionName
      
      ### make long and split model name
      
      new_datax <- new_data %>%
        pivot_longer(willow_StreamPower_adult:min_depth_limit, names_to = "Type", values_to = "Threshold") %>%
        separate(Type, c("SpeciesName", "HydraulicName", "LifeStageName"), remove=F) 
      
      thresholds <- unique(new_datax$Type)
      # thresholds
      
      # dataframe for stats -----------------------------------------------------
      # l=5
      for (l in 1:length(thresholds)) {
        
        ## define critical period or season for adult as all year is critical
        thresholds_data <- new_datax %>%
          filter(Type == thresholds[l])
        
        Model <- thresholds[l]
        
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
        # 
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
        # head(thresholds_datax)
        
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


