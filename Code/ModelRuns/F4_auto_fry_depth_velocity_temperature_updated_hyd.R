## fry algorithm

## packages

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

## upload hydraulic data
setwd("input_data/HecRas")

h <- list.files(pattern="predictions")
length(h) ## 18
n=1
## set wd back to main
setwd("/Users/katieirving/Documents/git/flow_eco_mech")

for(n in 1: length(h)) {
  
  NodeData <- read.csv(file=paste("input_data/HecRas/", h[n], sep=""))
  F34D <- read.csv("input_data/HecRas/hydraulic_ts_F34D.csv") ## for dates
  
  NodeName <- str_split(h[n], "_", 3)[[1]]
  NodeName <- NodeName[1]
  ## format hydraulic data
  names(NodeData)
  
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
      mutate(sp_w_MC = (Shear..lb.sq.ft..MC*4.44822)/0.3048) %>%
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
      mutate(sp_w_LOB = (Shear..lb.sq.ft..LOB*4.44822)/0.3048,
             sp_w_MC = (Shear..lb.sq.ft..MC*4.44822)/0.3048,
             sp_w_ROB = (Shear..lb.sq.ft..ROB*4.44822)/0.3048) %>%
      mutate(vel_m_LOB = (Avg..Vel...ft.s..LOB*0.3048),
             vel_m_MC = (Avg..Vel...ft.s..MC*0.3048),
             vel_m_ROB = (Avg..Vel...ft.s..ROB*0.3048)) %>%
      select(-contains("ft")) %>%
      mutate(date_num = seq(1,length(DateTime), 1))
    
  }
  
  ## take only depth variable
  hyd_dep <- hyd_dep %>% select(DateTime, node, Q, contains("depth"), date_num)
  
  # ## melt channel position data
  hyd_dep<-reshape2::melt(hyd_dep, id=c("DateTime","Q", "node", "date_num"))
  ## change NAs to 0 in concrete overbanks
  hyd_dep[is.na(hyd_dep)] <- 0
  
  ## format date time
  hyd_dep$DateTime<-as.POSIXct(hyd_dep$DateTime,
                                format = "%Y-%m-%d %H:%M",
                                tz = "GMT")
  
  ## create year, month, day and hour columns and add water year
  
  all_data <- hyd_dep %>%
    mutate(month = month(DateTime)) %>%
    mutate(year = year(DateTime)) %>%
    mutate(day = day(DateTime)) %>%
    mutate(hour = hour(DateTime)) %>%
    mutate(season = ifelse(month == 3 | month == 4 | month == 5 | month == 6 | month == 7, paste("critical"), paste("non_critical")))%>%
    mutate(water_year = ifelse(month == 10 | month == 11 | month == 12, year, year-1))

  
  ## save out
  save(all_data, file=paste("output_data/F4_", NodeName, "_SAS_depth_fry_discharge_probs_2010_2017_TS_updated_hyd.RData", sep=""))

  # format probability time series ------------------------------------------
  
  ### define dataframes for 2nd loop
  
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
  p=1
  # probability as a function of discharge -----------------------------------
  
  for(p in 1:length(positions)) {
    
    new_data <- all_data %>% 
      filter(variable  == positions[p])
    
    ## define position
    PositionName <- str_split(positions[p], "_", 3)[[1]]
    PositionName <- PositionName[3]
  

    ## get roots
    curve <- spline(new_data$Q, new_data$value,
                       xmin = min(new_data$Q), xmax = max(new_data$Q), ties = mean)
    
    if(min(curve$y) ==0 && max(curve$y) ==0) {
      newx1a <- 0
    } else if(min(curve$y)>3 && min(curve$y)<10) {
      newx1a <- min(curve$x)
    } else {
      newx1a <- approx(x = curve$y, y = curve$x, xout = 3)$y
    }
    
    if(min(curve$y) ==0 && max(curve$y) ==0) {
      newx2a <- 0
    } else if(min(curve$y)>10) {
      newx2a <- min(curve$x)
    } else {
      newx2a <- approx(x = curve$y, y = curve$x, xout = 10)$y
    }
    
    
    ## MAKE DF OF Q LIMITS
    limits[,p] <- c(newx1a, newx2a)
    H_limits[, p] <- c(3,10)
    
    # create year_month column       
    new_datax <- new_data %>% unite(month_year, c(water_year,month), sep="-", remove=F) 
    
    # dataframe for stats -----------------------------------------------------
    
    ## define critical period or season for juvenile as all year is critical
  
    ###### calculate amount of time
    time_stats <- new_data %>%
      dplyr::group_by(water_year, season) %>%
      dplyr::mutate(Seasonal = sum(Q >= newx1a & Q <= newx2a)/length(DateTime)*100) %>%
      distinct(water_year,  Seasonal) %>%
      mutate(position= paste(PositionName), Node = NodeName)
    
    Q_Calc[p,] <- paste("Q >= newx1a & Q <= newx2a")
    
    time_statsx <- rbind(time_statsx, time_stats)
    
    ### count days per month
    new_datax <- new_datax %>% 
      group_by(month, day, water_year, ID = data.table::rleid(Q >= newx1a & Q <= newx2a)) %>%
      mutate(threshold = if_else(Q >= newx1a & Q <= newx2a,  row_number(), 0L)) %>%
      mutate(position= paste(PositionName)) 

    
    days_data <- rbind(days_data, new_datax)

    
  } ## end 2nd loop
  
  Q_Calc$Position <- positions
  
  Q_Calc <- Q_Calc %>%
    mutate(Species ="SAS", Life_Stage = "Fry", Hydraulic = "Depth", Node = NodeName)
  
  write.csv(Q_Calc, paste("output_data/F4_",NodeName,"_SAS_fry_depth_Q_calculation_updated_hyd.csv", sep=""))
  
  ## limits
  limits <- rbind(limits, H_limits)
  
  limits <- limits %>%
    mutate(Species ="SAS", Life_Stage = "Fry", Hydraulic = "Depth", Node = NodeName)

  write.csv(limits, paste("output_data/F4_",NodeName,"_SAS_fry_depth_Q_limits_updated_hyd.csv", sep=""))
  
  ## percentage time
  melt_time<-reshape2::melt(time_statsx, id=c("season", "position", "water_year", "Node"))
  melt_time <- melt_time %>% 
    rename( Season = variable) %>%
    mutate(Species ="SAS", Life_Stage = "Fry", Hydraulic = "Depth", Node = NodeName)
  
  write.csv(melt_time, paste("output_data/F4_", NodeName, "_SAS_fry_depth_time_stats_updated_hyd.csv", sep=""))
  
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
    mutate(Node= paste(NodeName)) #%>%
  total_days
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
    mutate(Species ="SAS", Life_Stage = "Fry", Hydraulic = "Depth")

  
  ## save df
  write.csv(melt_days, paste("output_data/F4_", NodeName, "_SAS_fry_depth_total_days_long_updated_hyd.csv", sep="") )
  
} ## end 1st loop


# Velocity ----------------------------------------------------------------

setwd("/Users/katieirving/Documents/git/flow_eco_mech")

for(n in 1: length(h)) {
  
  NodeData <- read.csv(file=paste("input_data/HecRas/", h[n], sep=""))
  F34D <- read.csv("input_data/HecRas/hydraulic_ts_F34D.csv") ## for dates
  
  ## format hydraulic data
  
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
  
  ## convert units and change names
  
  if(length(NodeData) == 8) {
    hyd_vel <- hydraul %>%
      mutate(depth_cm_MC = (Max..Depth..ft..MC*0.3048)*100) %>%
      mutate(shear_pa_MC = (Shear..lb.sq.ft..MC/0.020885)) %>%
      mutate(sp_w_MC = (Shear..lb.sq.ft..MC*4.44822)/0.3048) %>%
      mutate(vel_m_MC = (Avg..Vel...ft.s..MC*0.3048)) %>%
      select(-contains("ft")) %>%
      mutate(date_num = seq(1,length(DateTime), 1))
  } else {
    hyd_vel <- hydraul %>%
      mutate(depth_cm_LOB = (Max..Depth..ft..LOB*0.3048)*100,
             depth_cm_MC = (Max..Depth..ft..MC*0.3048)*100,
             depth_cm_ROB = (Max..Depth..ft..ROB*0.3048)*100) %>%
      mutate(shear_pa_LOB = (Shear..lb.sq.ft..LOB/0.020885),
             shear_pa_MC = (Shear..lb.sq.ft..MC/0.020885),
             shear_pa_ROB = (Shear..lb.sq.ft..ROB/0.020885)) %>%
      mutate(sp_w_LOB = (Shear..lb.sq.ft..LOB*4.44822)/0.3048,
             sp_w_MC = (Shear..lb.sq.ft..MC*4.44822)/0.3048,
             sp_w_ROB = (Shear..lb.sq.ft..ROB*4.44822)/0.3048) %>%
      mutate(vel_m_LOB = (Avg..Vel...ft.s..LOB*0.3048),
             vel_m_MC = (Avg..Vel...ft.s..MC*0.3048),
             vel_m_ROB = (Avg..Vel...ft.s..ROB*0.3048)) %>%
      select(-contains("ft")) %>%
      mutate(date_num = seq(1,length(DateTime), 1))
    
  }
  
  # take only depth variable for min limit
  hyd_dep <- hyd_vel %>% select(DateTime, node, Q, contains("depth"), date_num)
  
  
  ## take only vel variable
  hyd_vel <- hyd_vel %>% select(DateTime, node, Q, contains( "vel"), date_num)
  
  # ## melt channel position data
  hyd_vel<-reshape2::melt(hyd_vel, id=c("DateTime","Q", "node", "date_num"))
  
  

  
  ## format date time
  hyd_vel$DateTime<-as.POSIXct(hyd_vel$DateTime,
                               format = "%Y-%m-%d %H:%M",
                               tz = "GMT")
  
  ## create year, month, day and hour columns and add water year
  
  all_data <- hyd_vel %>%
    mutate(month = month(DateTime)) %>%
    mutate(year = year(DateTime)) %>%
    mutate(day = day(DateTime)) %>%
    mutate(hour = hour(DateTime)) %>%
    mutate(season = ifelse(month == 3 | month == 4 | month == 5 | month == 6 | month == 7, paste("critical"), paste("non_critical")))%>%
    mutate(water_year = ifelse(month == 10 | month == 11 | month == 12, year, year-1))
  
  
  ## save out
  save(all_data, file=paste("output_data/F4_", NodeName, "_SAS_velocity_fry_discharge_probs_2010_2017_TS_updated_hyd.RData", sep=""))
  
  # format probability time series ------------------------------------------
  
  ### define dataframes for 2nd loop
  
  ## define positions
  positions <- unique(all_data$variable)
  
  ## Q Limits
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
    
  
    min_limit <- filter(new_dataD, depth_cm >0.03)
    
    min_limit <- min(min_limit$Q)
    

    head(new_data)
    ## get roots
    curve <- spline(new_data$Q, new_data$value,
                    xmin = min(new_data$Q), xmax = max(new_data$Q), ties = mean)


    ## main channel values
    if(min(curve$y) ==0 && max(curve$y) ==0) {
      newx2a <- 0
    } else if(min(curve$y)>0.05) { 
      newx2a <- min(curve$x)
    } else {
      newx2a <- approx(x = curve$y, y = curve$x, xout = 0.05)$y
    }

   
    
    ## MAKE DF OF Q LIMITS
    limits[,p] <- c(min_limit, newx2a)
    H_limits[, p] <- c(0, 0.05)
    
    # create year_month column       
    new_datax <- new_data %>% unite(month_year, c(water_year,month), sep="-", remove=F) 
    
    # dataframe for stats -----------------------------------------------------
    
    ## define critical period or season for juvenile as all year is critical
    
    
    
    ###### calculate amount of time
    time_stats <- new_data %>%
      dplyr::group_by(water_year, season) %>%
      dplyr::mutate(Seasonal = sum(Q >= min_limit & Q <= newx2a)/length(DateTime)*100) %>%
      distinct(water_year,  Seasonal) %>%
      mutate(position= paste(PositionName), Node = NodeName)

    Q_Calc[p,] <- paste("Q >= min_limit & Q <= newx2a")
    
    time_statsx <- rbind(time_statsx, time_stats)
    
    ### count days per month
    new_datax <- new_datax %>% 
      group_by(month, day, water_year, ID = data.table::rleid(Q >= min_limit & Q <= newx2a)) %>%
      mutate(threshold = if_else(Q >= min_limit & Q <= newx2a,  row_number(), 0L)) %>%
      mutate(position= paste(PositionName)) 
    
    
    days_data <- rbind(days_data, new_datax)
    
    
  } ## end 2nd loop
  
  Q_Calc$Position <- positions
  
  Q_Calc <- Q_Calc %>%
    mutate(Species ="SAS", Life_Stage = "Fry", Hydraulic = "Velocity", Node = NodeName)
  
  write.csv(Q_Calc, paste("output_data/F4_",NodeName,"_SAS_fry_velocity_Q_calculation_updated_hyd.csv", sep=""))
  
  
  ## limits
  limits <- rbind(limits, H_limits)
  ## note that 0.1 upper/lower limit is max/min Q to adhere to 0.1 bound
  limits <- limits %>%
    mutate(Species ="SAS", Life_Stage = "Fry", Hydraulic = "Velocity", Node = NodeName)

  write.csv(limits, paste("output_data/F4_",NodeName,"_SAS_fry_velocity_Q_limits_updated_hyd.csv", sep=""))
  

  ## percentage time
  melt_time<-reshape2::melt(time_statsx, id=c("season", "position", "water_year", "Node"))
  melt_time <- melt_time %>% 
    rename( Season = variable) %>%
    mutate(Species ="SAS", Life_Stage = "Fry", Hydraulic = "Velocity", Node = NodeName)
  
  write.csv(melt_time, paste("output_data/F4_", NodeName, "_SAS_fry_velocity_time_stats_updated_hyd.csv", sep=""))
  
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

  ## count the number of days in each month
  total_days_per_month01 <- total_days01 %>%
    group_by(water_year, position, season, month,  month_year,) %>%
    summarise(days_per_water_month = sum(n_days)) #%>%
  
  
  
  ## combine all thresholds
  total_days <- total_days_per_month01
  
  
  # # create year_month column       
  total_days <- ungroup(total_days) %>%
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
    mutate(Species ="SAS", Life_Stage = "Fry", Hydraulic = "Velocity")
  
  
  ## save df
  write.csv(melt_days, paste("output_data/F4_", NodeName, "_SAS_fry_velocity_total_days_long_updated_hyd.csv", sep="") )
  
} ## end 1st loop



