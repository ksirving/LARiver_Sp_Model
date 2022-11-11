## cladophora shear stress

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

n=1

## set wd back to main
setwd("/Users/katieirving/Documents/git/flow_eco_mech")

## function to find roots
load(file="root_interpolation_function.Rdata")

## define root equation
load(file="expression_Q_limit_function.RData")

# Combine with hydraulic data -------------------------------------------

## depth model
load(file="clad_depth_mod.RData")
clad_depth_mod <- depth_lmq


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
  
  names(hydraul)
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
  
  
  ## take only depth variable
  hyd_dep <- hyd_dep %>% select(Scenario, DateTime, node, Q, contains("depth"), date_num)

  
  # ## melt channel position data
  hyd_dep<-reshape2::melt(hyd_dep, id=c("Scenario", "DateTime","Q", "node", "date_num"))
  hyd_dep <- hyd_dep %>% rename(depth_cm = value)
  ## change NAs to 0 in concrete overbanks
  hyd_dep[is.na(hyd_dep)] <- 0
  
  all_data <- hyd_dep %>%
    mutate(prob_fit = predict(clad_depth_mod, newdata = hyd_dep, type="response")) %>%
    mutate(prob_fit = ifelse(prob_fit<=0, 0, prob_fit)) ## predicts negative percentages - cut off at 0 for quick fix
  
  

  # format probability time series ------------------------------------------
  
  ## look at data using lubridate etc
  
  ## format date time
  all_data$DateTime<-as.POSIXct(all_data$DateTime,
                                format = "%m/%d/%Y",
                                tz = "GMT")
  
  ## create year, month, day and hour columns and add water year
  
  all_data <- all_data %>%
    mutate(month = month(DateTime)) %>%
    mutate(year = year(DateTime)) %>%
    mutate(day = day(DateTime)) %>%
    mutate(hour = hour(DateTime)) %>%
    mutate(water_year = ifelse(month == 10 | month == 11 | month == 12, year, year-1))
  
  ### define dataframes for 2nd loop
  
  head(all_data)
  
  scenarios <- unique(all_data$Scenario)
  limitsx <- NULL
  H_limitsx <- NULL
  prob_statsx <- NULL

  s=1
  for(s in 1:length(scenarios)) {
    
    scen_data <- all_data %>% 
      filter(Scenario  == scenarios[s]) #%>%
      # pivot_wider(names_from = variable, values_from = depth_cm)

    head(scen_data)
  
  # define positions
  positions <- unique(scen_data$variable)
  ## Q Limits
  limits <- as.data.frame(matrix(ncol=length(positions), nrow=12)) 
  limits$Type<-c("Q_limit")
  limits$Scenario<-paste(scenarios[s])
  

  H_limits <- as.data.frame(matrix(ncol=length(positions), nrow=12))
  H_limits$Type<-c("Hydraulic_limit")
  H_limits$Scenario<-paste(scenarios[s])
  

  
  # probability as a function of discharge -----------------------------------
  p=1
  
  for(p in 1:length(positions)) {
    
    new_data <- scen_data %>% 
      filter(variable  == positions[p])
    
    ## define position
    PositionName <- str_split(positions[p], "_", 3)[[1]]
    PositionName <- PositionName[3]
    
    peak <- new_data %>%
      filter(prob_fit == max(prob_fit)) #%>%
    
    peakQ  <- max(peak$Q)
    min_limit <- filter(new_data, depth_cm >= 3)
    min_limit <- min(min_limit$Q)
    
    ## Main channel curves
    
    ## find roots for each probability
    
    ## low
    if(min(new_data$prob_fit)>25) {
      newx1a <- min_limit
      hy_lim1 <- min(new_data$depth_cm)
    } else {
      newx1a <- RootLinearInterpolant(new_data$Q, new_data$prob_fit, 25)
      hy_lim1 <- RootLinearInterpolant(new_data$depth_cm, new_data$prob_fit, 25)
    }
    
    if(max(new_data$prob_fit)<25) {
      newx1a <- NA
      hy_lim1 <- NA
    } else {
      newx1a <- newx1a
      hy_lim1 <- hy_lim1
    }
    
    if(length(newx1a) > 2) {
      newx1a <- c(newx1a[1], newx1a[length(newx1a)])
      hy_lim1<- c(hy_lim1[1], hy_lim1[length(hy_lim1)])
    } else {
      newx1a <- newx1a
      hy_lim1 <- hy_lim1
    }
    
    ## medium
    if(max(new_data$prob_fit)<50) {
      newx2a <- NA
      hy_lim2 <- NA
    } else {
      newx2a <- RootLinearInterpolant(new_data$Q, new_data$prob_fit, 50)
      hy_lim2 <- RootLinearInterpolant(new_data$depth_cm, new_data$prob_fit, 50)
    }
    
    if(min(new_data$prob_fit)>50) {
      newx2a <- min_limit
      hy_lim2 <- min(new_data$depth_cm)
    } else {
      newx2a <- newx2a
      hy_lim2 <- hy_lim2
    }
    
    if(length(newx2a) > 2) {
      newx2a <- c(newx2a[1], newx2a[length(newx2a)])
      hy_lim2<- c(hy_lim2[1], hy_lim2[length(hy_lim2)])
    } else {
      newx2a <- newx2a
      hy_lim2 <- hy_lim2
    }
    
    
    ##  high prob of occurrence
    if(min(new_data$prob_fit)>75) {
      newx3a <- min_limit
      hy_lim3 <- min(new_data$depth_cm)
    } else {
      newx3a <- RootLinearInterpolant(new_data$Q, new_data$prob_fit, 75)
      hy_lim3 <- RootLinearInterpolant(new_data$depth_cm, new_data$prob_fit, 75)
    }
    
    if(max(new_data$prob_fit)<75) {
      newx3a <- NA
      hy_lim2 <- NA
    } else {
      newx3a <- newx3a
      hy_lim3 <- hy_lim3
    }
    
    if(length(newx3a) > 2) {
      newx3a <- c(newx3a[1], newx3a[length(newx3a)])
      hy_lim3<- c(hy_lim3[1], hy_lim3[length(hy_lim3)])
    } else {
      newx3a <- newx3a
      hy_lim3 <- hy_lim3
    }
    
    ## MAKE DF OF Q LIMITS
    limits[,p] <- c(newx1a[1], newx1a[2],newx1a[3], newx1a[4],
                    newx2a[1], newx2a[2],newx2a[3], newx2a[4], 
                    newx3a[1], newx3a[2],newx3a[3],newx3a[4])
    
    H_limits[,p] <- c(hy_lim1[1], hy_lim1[2], hy_lim1[3], hy_lim1[4],
                      hy_lim2[1],hy_lim2[2],hy_lim2[3], hy_lim2[4], 
                      hy_lim3[1], hy_lim3[2],hy_lim3[3],hy_lim3[4])
    
    # create year_month column       
    new_datax <- new_data %>% unite(month_year, c(water_year,month), sep="-", remove=F) 
    
    # dataframe for stats -----------------------------------------------------
    
    ## define critical period or season for adult as all year is critical
    # # non_critical <- c(1,2,8:12) 
    # critical <- c(1:12) 
    
    new_datax <- new_datax %>%
      mutate(season =  "critical") 
    head(new_datax)

    prob_stats <- new_datax %>%
      filter(season == "critical") %>%
      group_by(water_year) %>%
      summarise(MeanProbability = mean(prob_fit)) %>%
      mutate(Position = PositionName, Scenario = scenarios[s])
      
      prob_statsx
    
    prob_statsx <- rbind(prob_statsx, prob_stats)
    limitsx <- rbind(limitsx, limits)
    H_limitsx <- rbind(H_limitsx, H_limits)

    
  } ## end positions loop
  

  
  }   ## end scenario loop

  ## limits
  limitsx <- rbind(limitsx, H_limitsx)

  limitsx <- limitsx %>%
    mutate(Species ="Cladophora", Life_Stage = "Adult", Hydraulic = "Depth", Node = NodeName)
  
  write.csv(limits, paste("results/scenarios/probs/X1a_",NodeName, "_Cladophora_adult_depth_Q_limits_WRP_scenarios_probs.csv", sep=""))
  
  prob_statsx <- prob_statsx %>%
    mutate(Species ="Cladophora", Life_Stage = "Adult", Hydraulic = "Depth", Node = NodeName )

  write.csv(limits, paste("results/scenarios/probs/X1a_",NodeName, "_Cladophora_adult_depth_probs_WRP_scenarios_probs.csv", sep=""))
  
} ## end node loop




# Velocity ----------------------------------------------------------------


## upload habitat curve data
load(file="clad_vel_mod.RData")


## set wd back to main
setwd("/Users/katieirving/Documents/git/flow_eco_mech")

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
  
  names(hydraul)
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
  

  
  
  ## take only vel variable
  hyd_dep <- hyd_dep %>% select(Scenario, DateTime, node, Q, contains("velocity"), date_num)
  
  # ## melt channel position data
  hyd_dep<-reshape2::melt(hyd_dep, id=c("Scenario","DateTime","Q", "node", "date_num"))
  
  
  hyd_dep <- hyd_dep %>%
    # filter(variable %in% c("shear_pa_LOB", "shear_pa_MC", "shear_pa_ROB")) %>%
    rename(Velocity = value)
  
  all_data <- hyd_dep %>%
    mutate(prob_fit = predict(clad_vel_mod, newdata = hyd_dep, type="response")) %>%
    mutate(prob_fit = ifelse(prob_fit<=0, 0, prob_fit)) ## predicts negative percentages - cut off at 0 for quick fix
  
  
  # format probability time series ------------------------------------------
  
  ## look at data using lubridate etc
  
  ## format date time
  all_data$DateTime<-as.POSIXct(all_data$DateTime,
                                format = "%m/%d/%Y",
                                tz = "GMT")
  
  ## create year, month, day and hour columns and add water year
  
  all_data <- all_data %>%
    mutate(month = month(DateTime)) %>%
    mutate(year = year(DateTime)) %>%
    mutate(day = day(DateTime)) %>%
    # mutate(hour = hour(DateTime)) %>%
    mutate(water_year = ifelse(month == 10 | month == 11 | month == 12, year, year-1))
  
  ### define dataframes for 2nd loop
  
  head(all_data)
  
  scenarios <- unique(all_data$Scenario)
  limitsx <- NULL
  H_limitsx <- NULL
  prob_statsx <- NULL
  
  s=1
  for(s in 1:length(scenarios)) {
    
    scen_data <- all_data %>% 
      filter(Scenario  == scenarios[s]) #%>%
    # pivot_wider(names_from = variable, values_from = depth_cm)
    scen_hyd <- hyd %>% 
      filter(Scenario  == scenarios[s]) #%>%
    head(scen_data)
    
    # define positions
    positions <- unique(scen_data$variable)
    ## Q Limits
    limits <- as.data.frame(matrix(ncol=length(positions), nrow=12)) 
    limits$Type<-c("Q_limit")
    limits$Scenario<-paste(scenarios[s])
    
    
    H_limits <- as.data.frame(matrix(ncol=length(positions), nrow=12))
    H_limits$Type<-c("Hydraulic_limit")
    H_limits$Scenario<-paste(scenarios[s])
    
    
    
    # probability as a function of discharge -----------------------------------
    p=1
    
    for(p in 1:length(positions)) {
      
      new_data <- scen_data %>% 
        filter(variable  == positions[p])
      
      ## define position
      PositionName <- str_split(positions[p], "_", 3)[[1]]
      PositionName <- PositionName[3]
      
      new_dataD <- scen_hyd %>% 
        select(DateTime, node, Q, contains(PositionName)) 
      head(new_dataD)
      colnames(new_dataD)[4] <- "depth_cm"
      
      ## get peak of curve
      peak <- new_data %>%
        filter(prob_fit == max(prob_fit)) #%>%
      
      peakQ  <- max(peak$Q)
      min_limit <- filter(new_dataD, depth_cm >=3)
      min_limit <- min(min_limit$Q)
      min_limit
      ## Main channel curves
      
      ## find roots for each probability
      
      ## low
      if(min(new_data$prob_fit)>25) {
        newx1a <- min_limit
        hy_lim1 <- min(new_data$depth_cm)
      } else {
        newx1a <- RootLinearInterpolant(new_data$Q, new_data$prob_fit, 25)
        hy_lim1 <- RootLinearInterpolant(new_data$depth_cm, new_data$prob_fit, 25)
      }
      
      if(max(new_data$prob_fit)<25) {
        newx1a <- NA
        hy_lim1 <- NA
      } else {
        newx1a <- newx1a
        hy_lim1 <- hy_lim1
      }
      
      if(length(newx1a) > 2) {
        newx1a <- c(newx1a[1], newx1a[length(newx1a)])
        hy_lim1<- c(hy_lim1[1], hy_lim1[length(hy_lim1)])
      } else {
        newx1a <- newx1a
        hy_lim1 <- hy_lim1
      }
      
      ## medium
      if(max(new_data$prob_fit)<50) {
        newx2a <- NA
        hy_lim2 <- NA
      } else {
        newx2a <- RootLinearInterpolant(new_data$Q, new_data$prob_fit, 50)
        hy_lim2 <- RootLinearInterpolant(new_data$depth_cm, new_data$prob_fit, 50)
      }
      
      if(min(new_data$prob_fit)>50) {
        newx2a <- min_limit
        hy_lim2 <- min(new_data$depth_cm)
      } else {
        newx2a <- newx2a
        hy_lim2 <- hy_lim2
      }
      
      if(length(newx2a) > 2) {
        newx2a <- c(newx2a[1], newx2a[length(newx2a)])
        hy_lim2<- c(hy_lim2[1], hy_lim2[length(hy_lim2)])
      } else {
        newx2a <- newx2a
        hy_lim2 <- hy_lim2
      }
      
      
      ##  high prob of occurrence
      if(min(new_data$prob_fit)>75) {
        newx3a <- min_limit
        hy_lim3 <- min(new_data$depth_cm)
      } else {
        newx3a <- RootLinearInterpolant(new_data$Q, new_data$prob_fit, 75)
        hy_lim3 <- RootLinearInterpolant(new_data$depth_cm, new_data$prob_fit, 75)
      }
      
      if(max(new_data$prob_fit)<75) {
        newx3a <- NA
        hy_lim2 <- NA
      } else {
        newx3a <- newx3a
        hy_lim3 <- hy_lim3
      }
      
      if(length(newx3a) > 2) {
        newx3a <- c(newx3a[1], newx3a[length(newx3a)])
        hy_lim3<- c(hy_lim3[1], hy_lim3[length(hy_lim3)])
      } else {
        newx3a <- newx3a
        hy_lim3 <- hy_lim3
      }
      
      ## MAKE DF OF Q LIMITS
      limits[,p] <- c(newx1a[1], newx1a[2],newx1a[3], newx1a[4],
                      newx2a[1], newx2a[2],newx2a[3], newx2a[4], 
                      newx3a[1], newx3a[2],newx3a[3],newx3a[4])
      
      H_limits[,p] <- c(hy_lim1[1], hy_lim1[2], hy_lim1[3], hy_lim1[4],
                        hy_lim2[1],hy_lim2[2],hy_lim2[3], hy_lim2[4], 
                        hy_lim3[1], hy_lim3[2],hy_lim3[3],hy_lim3[4])
      
      # create year_month column       
      new_datax <- new_data %>% unite(month_year, c(water_year,month), sep="-", remove=F) 
      
      # dataframe for stats -----------------------------------------------------
      
      ## define critical period or season for adult as all year is critical
      # # non_critical <- c(1,2,8:12) 
      # critical <- c(1:12) 
      
      new_datax <- new_datax %>%
        mutate(season =  "critical") 
      head(new_datax)
      
      prob_stats <- new_datax %>%
        filter(season == "critical") %>%
        group_by(water_year) %>%
        summarise(MeanProbability = mean(prob_fit)) %>%
        mutate(Position = PositionName, Scenario = scenarios[s])
  
      
      prob_statsx <- rbind(prob_statsx, prob_stats)
      limitsx <- rbind(limitsx, limits)
      H_limitsx <- rbind(H_limitsx, H_limits)
      
      
    } ## end positions loop
    
    
    
  }   ## end scenario loop
  
  ## limits
  limitsx <- rbind(limitsx, H_limitsx)
  
  limitsx <- limitsx %>%
    mutate(Species ="Cladophora", Life_Stage = "Adult", Hydraulic = "Depth", Node = NodeName)
  limitsx
  write.csv(limits, paste("results/scenarios/probs/X1a_",NodeName, "_Cladophora_adult_depth_Q_limits_WRP_scenarios_probs.csv", sep=""))
  
  prob_statsx <- prob_statsx %>%
    mutate(Species ="Cladophora", Life_Stage = "Adult", Hydraulic = "Depth", Node = NodeName )
  
  write.csv(limits, paste("results/scenarios/probs/X1a_",NodeName, "_Cladophora_adult_depth_probs_WRP_scenarios_probs.csv", sep=""))
  
} ## end node loop

### velocity

for(n in 1: length(h)) {
  
  NodeData <- read.csv(file=paste(dir, h[n], sep=""))
  
  # F34D <- read.csv("input_data/HecRas/hydraulic_ts_F34D.csv") ## for dates
  
  NodeName <- str_split(h[n], "_", 3)[[1]]
  NodeName <- NodeName[1]
  ## format hydraulic data
  
  
  cat(paste("Running Node", NodeName))
  
  NodeData <- NodeData %>%
    mutate(DateTime = F34D$Q_ts.datetime)
  
  hydraul <-NodeData[,-1]
  
  ## change some names
  hydraul <- hydraul %>%
    rename(Q = Flow) %>%
    mutate(node = NodeName)
  
  ## convert units and change names - depending on concrete/soft bottom. if/else to determine changes to data
  
  if(length(NodeData) == 8) {
    hyd_dep <- hydraul %>%
      mutate(depth_cm_MC = (Max..Depth..ft..MC*0.3048)*100) %>%
      mutate(shear_pa_MC = (Shear..lb.sq.ft..MC/0.020885)) %>%
      mutate(sp_w_MC = (Stream.Power..lb.ft.s..MC*4.44822)/0.3048) %>%
      mutate(vel_m_MC = (Avg..Vel...ft.s..MC*0.3048)) %>%
      select(-contains("ft")) %>%
      mutate(date_num = seq(1,length(DateTime), 1))
  } else {
    hyd_dep <- hydraul %>%
      mutate(depth_cm_LOB = (Max..Depth..ft..LOB*0.3048)*100,
             depth_cm_MC = (Max..Depth..ft..MC*0.3048)*100,
             depth_cm_ROB = (Max..Depth..ft..ROB*0.3048)*100) %>%
      mutate(shear_pa_LOB = (Shear..lb.sq.ft..LOB/0.020885),
             shear_pa_MC = (Shear..lb.sq.ft..MC/0.020885),
             shear_pa_ROB = (Shear..lb.sq.ft..ROB/0.020885)) %>%
      mutate(sp_w_LOB = (Stream.Power..lb.ft.s..LOB*4.44822)/0.3048,
             sp_w_MC = (Stream.Power..lb.ft.s..MC*4.44822)/0.3048,
             sp_w_ROB = (Stream.Power..lb.ft.s..LOB*4.44822)/0.3048) %>%
      mutate(vel_m_LOB = (Avg..Vel...ft.s..LOB*0.3048),
             vel_m_MC = (Avg..Vel...ft.s..MC*0.3048),
             vel_m_ROB = (Avg..Vel...ft.s..ROB*0.3048)) %>%
      select(-contains("ft")) %>%
      mutate(date_num = seq(1,length(DateTime), 1))
    
  }
  
  ## take only vel variable
  hyd_shear <- hyd_dep %>% select(DateTime, node, Q, contains( "vel"), date_num)
  
  # ## melt channel position data
  hyd_shear<-reshape2::melt(hyd_shear, id=c("DateTime","Q", "node", "date_num"))
  
  
  ### take only depth variable for min limit
  hyd_dep <- hyd_dep %>% select(DateTime, node, Q, contains("depth"), date_num)
  
  
  
  ## format date time
  hyd_shear$DateTime<-as.POSIXct(hyd_shear$DateTime,
                                 format = "%Y-%m-%d %H:%M",
                                 tz = "GMT")
  
  ## create year, month, day and hour columns and add water year
  
  all_data <- hyd_shear %>%
    mutate(month = month(DateTime)) %>%
    mutate(year = year(DateTime)) %>%
    mutate(day = day(DateTime)) %>%
    mutate(hour = hour(DateTime)) %>%
    mutate(season =  paste("critical"))%>%
    mutate(water_year = ifelse(month == 10 | month == 11 | month == 12, year, year-1))
  
  
  ## save out
  # save(all_data, file=paste("output_data/C1_", NodeName, "_Cladophora_Shear_Stress_Adult_discharge_probs_2010_2017_TS_updated_hyd.RData", sep=""))
  
  # format probability time series ------------------------------------------
  
  ### define dataframes for 2nd loop
  
  scenarios <- unique(all_data$Scenario)
  
  ## define positions
  positions <- unique(all_data$variable)
  
  ## Q limits
  limits <- as.data.frame(matrix(ncol=length(positions), nrow=2)) 
  limits$Type<-c("Q_limit1", "Q_limit2")
  
  H_limits <- as.data.frame(matrix(ncol=length(positions), nrow=2)) 
  H_limits$Type<-c("Hydraulic_limit1", "Hydraulic_limit2")
  
  ## calculation
  Q_Calc <- as.data.frame(matrix(ncol=1, nrow=3 ))
  
  names(Q_Calc) <- "Thresh"
  
  time_statsx <- NULL
  days_data <- NULL
  
  # probability as a function of discharge -----------------------------------
  
  for(p in 1:length(positions)) {
    
    new_data <- all_data %>% 
      filter(variable  == positions[p])
    
    ## define position
    PositionName <- str_split(positions[p], "_", 3)[[1]]
    PositionName <- PositionName[3]
    
    new_dataD <- hyd_dep %>% 
      select(DateTime, node, Q, contains(PositionName)) 
    
    colnames(new_dataD)[4] <- "depth_cm"
    
    min_limit <- filter(new_dataD, depth_cm >= 3)
    min_limit <- min(min_limit$Q)
    min_shear <- min(new_data$value)
    min_limit
    
    ## get roots
    curve <- spline(new_data$Q, new_data$value,
                    xmin = min(new_data$Q), xmax = max(new_data$Q), ties = mean)
    
    
    if(max(curve$y)<16.9) {
      newx2a <- max(curve$x)
    } else {
      newx2a <- approx(x = curve$y, y = curve$x, xout = 16.9)$y
    }
    
    
    ## MAKE DF OF Q LIMITS
    limits[,p] <- c(min_limit, newx2a)
    H_limits[, p] <- c(min_shear, 16.9)
    
    # create year_month column       
    new_datax <- new_data %>% unite(month_year, c(water_year,month), sep="-", remove=F) 
    
    # dataframe for stats -----------------------------------------------------
    
    ## define critical period or season for juvenile as all year is critical
    
    
    ###### calculate amount of time
    time_stats <- new_datax %>%
      dplyr::group_by(water_year, season) %>%
      dplyr::mutate(Seasonal = sum(Q <= newx2a)/length(DateTime)*100) %>%
      distinct(water_year,  Seasonal) %>%
      mutate(position= paste(PositionName), Node = NodeName)
    
    Q_Calc[p,] <- paste("Q <= newx2a")
    
    time_statsx <- rbind(time_statsx, time_stats)
    
    ### count days per month
    new_datax <- new_datax %>% 
      group_by(month, day, water_year, ID = data.table::rleid(Q <= newx2a)) %>%
      mutate(threshold = if_else(Q <= newx2a,  row_number(), 0L)) %>%
      mutate(position= paste(PositionName)) 
    
    
    days_data <- rbind(days_data, new_datax)
    
    
  } ## end 2nd loop
  
  Q_Calc$Position <- positions
  
  Q_Calc <- Q_Calc %>%
    mutate(Species ="Cladophora", Life_Stage = "Adult", Hydraulic = "Shear Stress", Node = NodeName)
  
  write.csv(Q_Calc, paste("output_data/W2_",NodeName,"_Cladophora_Adult_Shear_Stress_Q_calculation_updated_hyd.csv", sep=""))
  
  ## limits
  limits <- rbind(limits, H_limits)
  
  limits <- limits %>%
    mutate(Species ="Cladophora", Life_Stage = "Adult", Hydraulic = "Shear Stress", Node = NodeName)
  
  write.csv(limits, paste("output_data/F4_",NodeName,"_Cladophora_Adult_Shear_Stress_Q_limits_updated_hyd.csv", sep=""))
  
  
  
  ## percentage time
  melt_time<-reshape2::melt(time_statsx, id=c("season", "position", "water_year", "Node"))
  melt_time <- melt_time %>% 
    rename( Season = variable) %>%
    mutate(Species ="Cladophora", Life_Stage = "Adult", Hydraulic = "Shear Stress", Node = NodeName)
  
  write.csv(melt_time, paste("output_data/C1_", NodeName, "_Cladophora_Adult_Shear_Stress_time_stats_updated_hyd.csv", sep=""))
  
  ### days per month
  days_data <- select(days_data,c(Q, month, water_year, month_year, year, day, ID, threshold, position, season, node))
  
  melt_data<-reshape2::melt(days_data, id=c("ID", "day", "month", "year","month_year", "Q", "water_year", "position", "season", "node"))
  melt_data <- melt_data %>% rename(consec_hours = value) %>%
    select(-variable)
  
  ## count how many full days i.e. 24 hours
  total_days01 <- melt_data %>% 
    group_by(ID, day, month, water_year, month_year, position, season) %>%
    summarise(n_hours = max(consec_hours))  %>%
    mutate(n_days = ifelse(n_hours >= 24, 1, 0)) # %>%
  total_days01
  ## count the number of days in each month
  total_days_per_month01 <- total_days01 %>%
    group_by(water_year, position, season, month,  month_year,) %>%
    summarise(days_per_water_month = sum(n_days)) #%>%
  
  ## combine all thresholds
  total_days <- total_days_per_month01
  
  # # create year_month column       
  total_days <- ungroup(total_days) %>%
    unite(month_year, water_year:month, sep="-", remove=F) %>%
    mutate(Node= paste(NodeName)) #%>%
  
  ## convert month year to date format
  
  total_days$month_year <-  zoo::as.yearmon(total_days$month_year)
  # total_days$month_year <- as.Date(total_days$month_year)
  
  ## change names of columns
  # total_days <- rename(total_days, Low = days_per_month_low, Medium = days_per_month_medium, High = days_per_month_high)
  
  
  # ## melt data
  
  melt_days<-reshape2::melt(total_days, id=c("month_year", "water_year", "month", "season", "position", "Node"))
  melt_days <- melt_days %>%
    rename( n_days = value) %>%
    select(-variable) %>%
    mutate(Species ="Cladophora", Life_Stage = "Adult", Hydraulic = "Shear Stress")
  
  
  ## save df
  write.csv(melt_days, paste("output_data/C1_", NodeName, "_Cladophora_Adult_Shear Stress_total_days_long_updated_hyd.csv", sep="") )
  
} ## end 1st loop

