## all species probabilities

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

n=4

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
  
  # ?unite
  # head(all_data)
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
  
  s=1
  for(s in 1:length(scenarios)) {
    
    scen_data <- all_data %>% 
      filter(Scenario  == scenarios[s]) %>%
      pivot_wider(names_from = variable, values_from = value) %>%
      rename(shear = Shear_pa, Velocity = velocity_m, StreamPower = StreamPower_w) %>%
      mutate(vel_m_s = Velocity)
      
    
    # head(scen_data)
    # names(scen_data)
    
    ## calculate probability

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
    
    # names(scen_datax)
    
    positions <- unique(scen_datax$Position)

    p=1
    for(p in 1:length(positions)) {
      
      new_data <- scen_datax %>% 
        filter(Position  == positions[p])
      # head(new_data)
      ## define position
      PositionName <- positions[p]
      # PositionName
      
      ### make long and split model name
      
      new_datax <- new_data %>%
        pivot_longer(Cladophora_depth_adult:SAS_Velocity_adult, names_to = "Type", values_to = "Probability") %>%
        separate(Type, c("SpeciesName", "HydraulicName", "LifeStageName"), remove=F) %>%
        mutate(Probability = ifelse(Probability >1, 1, Probability))
    
        
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

