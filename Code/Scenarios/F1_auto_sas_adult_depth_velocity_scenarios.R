## Depth curves - model and application
## adult 

## takes Q limits from baseline assessment and evaluates suitability under WRP scenarios

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
library(zoo)
library(scales)

## function to find roots
# load(file="root_interpolation_function.Rdata")

## define root equation
load(file="expression_Q_limit_function.RData")

# Combine with hydraulic data -------------------------------------------

## upload habitat curve data
fitdata <- read.csv("output_data/old_data/adult_depth_prob_curve_data.csv")

## upload hydraulic data
setwd("input_data/HecRas/scenarios")

h <- list.files(pattern="_predictions")
length(h) ## 20
h
n=1
## set wd back to main
setwd("/Users/katieirving/Documents/git/flow_eco_mech")

for(n in 1: length(h)) {
  
  NodeData <- read.csv(file=paste("input_data/HecRas/scenarios/", h[n], sep=""))
  # BaseData <- read.csv("input_data/HecRas/hydraulic_ts_F34D.csv") ## for dates
  NodeName <- str_split(h[n], "_", 3)[[1]]
  NodeName <- NodeName[1]
  ## format hydraulic data
  # 
  # load(file=paste("output_data/F1_", NodeName, "_SAS_depth_adult_discharge_probs_2010_2017_TS_updated_hyd.RData", sep=""))
  # BaseData <- all_data
  cat(paste("Running Node", NodeName))
  
  # NodeData <- NodeData %>%
  #   mutate(DateTime = F34D$Q_ts.datetime)
  
  hydraul <-NodeData
 
  ## change some names
  hydraul <- hydraul %>%
    rename(Q = Flow) %>%
    mutate(node = NodeName)
  

  ## convert units and change names - depending on concrete/soft bottom. if/else to determine changes to data
  
  if(length(NodeData) == 9) {
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
  hyd_dep <- hyd_dep %>% select(Scenario, DateTime, node, Q, contains("depth"), date_num)
  
  # ## melt channel position data
  hyd_dep<-reshape2::melt(hyd_dep, id=c("Scenario", "DateTime","Q", "node", "date_num"))
  
  
  # ## change NAs to 0 in concrete overbanks
  # hyd_dep[is.na(hyd_dep)] <- 0
  
  ## use smooth spline to predict on new data set
  new_values <-smooth.spline(fitdata$depth_fit, fitdata$prob_fit)
  
  all_data <- hyd_dep %>%
    group_by(variable) %>%
    mutate(prob_fit = predict(new_values, value)$y) %>%
    rename(depth_cm = value)
  
  
  ## save out
  # save(all_data, file=paste("output_data/F1_", NodeName, "_SAS_adult_depth_discharge_probability_updated_hyd_.RData", sep=""))
  
  
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

  # save(all_data, file=paste("output_data/F1_", NodeName, "_SAS_depth_adult_discharge_probs_2010_2017_TS_updated_hyd.RData", sep=""))
  
  ### define dataframes for 2nd loop
  
  ## define positions
  positions <- unique(all_data$variable)
  scenarios <- unique(all_data$Scenario)
  
  ## upload and format Q limits and calculation
  Q_Calc <- read.csv(paste("output_data/F1_", NodeName, "_SAS_adult_depth_Q_calculation_updated_hyd.csv", sep=""))
  
  if(length(NodeData) == 9) {
    Q_Calc <- Q_Calc %>%
      mutate(Position = c("MC"))
    Q_Calc <- na.omit(Q_Calc)
  } else {
    Q_Calc <- Q_Calc %>%
      mutate(Position = c("LOB", "MC", "ROB"))
  }
 
  
  limits <- read.csv(paste("output_data/F1_", NodeName, "_SAS_adult_depth_Q_limits_updated_hyd.csv", sep=""))  
  
  if(length(NodeData) == 9) {
    limits <- limits %>%
      rename(MC = V1) %>%
      filter(Type == "Q_limit")
  } else {
    limits <- limits %>%
      rename(LOB = V1, MC = V2, ROB = V3) %>%
      filter(Type == "Q_limit")
  }

  
  limits$ProbThresh[1:4] <- "Low"
  limits$ProbThresh[5:8] <- "Medium"
  limits$ProbThresh[9:12] <- "High"
  
  melt_timex <- NULL
  melt_daysx <- NULL

  # Select Scenarios -----------------------------------
  s=1
  for(s in 1:length(scenarios)) {
  
    scen_data <- all_data %>% 
      filter(Scenario  == scenarios[s])
  
    time_statsx <- NULL
    days_data <- NULL
    
    cat(paste("Running Scenario", s))

  for(p in 1:length(positions)) {
    
    new_data <- scen_data %>% 
      filter(variable  == positions[p])
    
    # new_base_data <- BaseData %>% 
    #   filter(variable  == positions[p])
    
    ## define position
    PositionName <- str_split(positions[p], "_", 3)[[1]]
    PositionName <- PositionName[3]
    
    min_limit <- filter(new_data, depth_cm > 0.03)
    min_limit <- min(min_limit$Q)
   
    ## Q limits
    ## low
    newx1ax <- limits %>%
      select(all_of(PositionName), ProbThresh) %>%
      filter(ProbThresh == "Low")
    
    newx1a <- newx1ax[,1]
    newx1a <- na.omit(newx1a)
    # medium
    newx2ax <- limits %>%
      select(all_of(PositionName), ProbThresh) %>%
      filter(ProbThresh == "Medium")
    
    newx2a <- newx2ax[,1]
    newx2a <- na.omit(newx2a)
    
    # high
    newx3ax <- limits %>%
      select(all_of(PositionName), ProbThresh) %>%
      filter(ProbThresh == "High")
    
    newx3a <- newx3ax[,1]
    newx3a <- na.omit(newx3a)
    
    ## Q calculation
   
    
    low_threshx <- Q_Calc %>%
      select(Low, Position) %>%
      filter(Position == PositionName)

    low_thresh <- parse(text = low_threshx[,1])
    
    med_threshx <- Q_Calc %>%
      select(Medium, Position) %>%
      filter(Position == PositionName)
    
    med_thresh <- parse(text = med_threshx[,1])
    
    high_threshx <- Q_Calc %>%
      select(High, Position) %>%
      filter(Position == PositionName)
    
    high_thresh <- parse(text = high_threshx[,1])
    
    # create year_month column       
    new_datax <- new_data %>% unite(month_year, c(water_year,month), sep="-", remove=F) 
    
    # dataframe for stats -----------------------------------------------------
    
    ## define critical period or season for adult as all year is critical
    winter <- c(1,2,3,4,11,12) ## winter months
    summer <- c(5:10) ## summer months
    
    new_datax <- new_datax %>%
      mutate(season = ifelse(month %in% winter, "winter", "summer") )
    
  
    ###### calculate amount of time
    warnings()
    
    time_stats <- new_datax %>%
      dplyr::group_by(water_year) %>%
      dplyr::mutate(Low = sum(eval(low_thresh))/length(DateTime)*100) %>%
      dplyr::mutate(Medium = sum(eval(med_thresh))/length(DateTime)*100) %>%
      dplyr::mutate(High = sum(eval(high_thresh))/length(DateTime)*100) %>%
      ungroup() %>%
      dplyr::group_by(water_year, season) %>%
      dplyr::mutate(Low.Seasonal = sum(eval(low_thresh))/length(DateTime)*100) %>%
      dplyr::mutate(Medium.Seasonal = sum(eval(med_thresh))/length(DateTime)*100) %>%
      dplyr::mutate(High.Seasonal = sum(eval(high_thresh))/length(DateTime)*100) %>%
      distinct(water_year, Low , Medium , High , Low.Seasonal, Medium.Seasonal, High.Seasonal) %>%
      mutate(position= paste(PositionName), Node = NodeName)
    
    time_statsx <- rbind(time_statsx, time_stats)
    
    ### count days per month
    new_datax <- new_datax %>%
      ungroup() %>%
      group_by(month,  water_year, ID01 = data.table::rleid(eval(low_thresh))) %>%
      mutate(Low = if_else(eval(low_thresh), row_number(), 0L)) %>%
      ungroup() %>%
      group_by(month,  water_year, ID02 = data.table::rleid(eval(med_thresh))) %>%
      mutate(Medium = if_else(eval(med_thresh), row_number(), 0L)) %>%
      ungroup() %>%
      group_by(month,  water_year, ID03 = data.table::rleid(eval(high_thresh))) %>%
      mutate(High = if_else(eval(high_thresh), row_number(), 0L)) %>%
      mutate(position= paste(PositionName)) #%>%
    # select(Q, month, water_year, day, ID01, Low, ID02, Medium, ID03, High, position, DateTime, node) 
    
    days_data <- rbind(days_data, new_datax)
    
  } ## end 3rd loop - positions
  
  ## limits
  ## note that 0.1 upper/lower limit is max/min Q to adhere to 0.1 bound
  
  ## percentage time
  melt_time<-reshape2::melt(time_statsx, id=c("season", "position", "water_year", "Node"))
  melt_time <- melt_time %>% 
    rename( Probability_Threshold = variable) %>%
    mutate(Species ="SAS", Life_Stage = "Adult", Hydraulic = "Depth", Node = NodeName, Scenario =scenarios[s])
 
  melt_timex <- rbind(melt_timex, melt_time)
  
  ### days per month
  days_data <- select(days_data, c(Q, month, water_year, day, ID01, Low, ID02, Medium, ID03, High, position, DateTime, node) )# all probs
  
  melt_data<-reshape2::melt(days_data, id=c("ID01", "ID02", "ID03", "day", "month", "water_year", "Q", "position", "node"))
  melt_data <- rename(melt_data, Probability_Threshold = variable, 
                      consec_hours = value)
  
  ## count how many full days i.e. 24 hours
  total_days01 <- melt_data %>% 
    filter(Probability_Threshold == "Low") %>% 
    group_by(ID01, day, month, water_year, position) %>%
    summarise(n_hours = max(consec_hours))  %>%
    mutate(n_days_low = ifelse(n_hours >= 24, 1, 0)) # %>%
  
  ## count the number of days in each month
  total_days_per_month01 <- total_days01 %>%
    group_by(month, water_year, position) %>%
    summarise(days_per_month_low = sum(n_days_low))
  
  
  total_days02 <- melt_data %>% 
    filter(Probability_Threshold == "Medium") %>% 
    group_by(ID02, day, month, water_year, position) %>%
    summarise(n_hours = max(consec_hours))  %>%
    mutate(n_days_medium = ifelse(n_hours >= 24, 1, 0)) # %>%
  
  total_days_per_month02 <- total_days02 %>%
    group_by(month, water_year, position) %>%
    summarise(days_per_month_medium = sum(n_days_medium))
  
  # total_days_per_month02
  
  total_days03 <- melt_data %>% 
    filter(Probability_Threshold == "High") %>% 
    group_by(ID03, day, month, water_year, position) %>%
    summarise(n_hours = max(consec_hours))  %>%
    mutate(n_days_high = ifelse(n_hours >= 24, 1, 0)) # %>%
  
  total_days_per_month03 <- total_days03 %>%
    group_by(month, water_year, position) %>%
    summarise(days_per_month_high = sum(n_days_high))
  
  ## combine all thresholds
  total_days <- cbind( total_days_per_month01,total_days_per_month02[,4], total_days_per_month03[,4])
  total_days
  
  # # create year_month column       
  total_days <- ungroup(total_days) %>%
    unite(month_year, water_year:month, sep="-", remove=F) %>%
    mutate(Node= paste(NodeName)) #%>%
  
  ## convert month year to date format
  
  total_days$month_year <-  zoo::as.yearmon(total_days$month_year)
  total_days$month_year <- as.Date(total_days$month_year)
  
  ## change names of columns
  total_days <- rename(total_days, Low = days_per_month_low, Medium = days_per_month_medium, High = days_per_month_high)
  
  ## define seasons
  winter <- c(1,2,3,4,11,12) ## winter months
  summer <- c(5:10) ## summer months
  
  total_days <- total_days %>%
    mutate(season = ifelse(month %in% winter, "winter", "summer") )
  
  # ## melt data
  
  melt_days<-reshape2::melt(total_days, id=c("month_year", "water_year", "month", "season", "position", "Node"))
  melt_days <- melt_days %>%
    rename(Probability_Threshold = variable, n_days = value) %>%
    mutate(Species ="SAS", Life_Stage = "Adult", Hydraulic = "Depth", Scenario =scenarios[s])
  
  melt_daysx <- rbind(melt_daysx, melt_days)
  
  } ## end 2nd loop scenarios
  
  ## save df
  write.csv(melt_timex, paste("output_data/scenarios/F1_", NodeName, "_SAS_adult_depth_time_stats_updated_hyd_scenarios.csv", sep=""))
  
  write.csv(melt_days, paste("output_data/scenarios/F1_", NodeName, "_SAS_adult_depth_total_days_long_updated_hyd_scenarios.csv", sep="") )
  
  
} ## end 1st loop - node




# Velocity ----------------------------------------------------------------


## upload habitat curve data
fitdata <- read.csv("output_data/old_data/adult_velocity_prob_curve_data.csv")
## upload hydraulic data
setwd("input_data/HecRas/scenarios")

h <- list.files(pattern="_predictions")
length(h) ## 20
h
n=1
## set wd back to main
setwd("/Users/katieirving/Documents/git/flow_eco_mech")

for(n in 1: length(h)) {
  
  NodeData <- read.csv(file=paste("input_data/HecRas/scenarios/", h[n], sep=""))
  # BaseData <- read.csv("input_data/HecRas/hydraulic_ts_F34D.csv") ## for dates
  NodeName <- str_split(h[n], "_", 3)[[1]]
  NodeName <- NodeName[1]
  ## format hydraulic data
  
  # load(file=paste("output_data/F1_", NodeName, "_SAS_velocity_adult_discharge_probs_2010_2017_TS_updated_hyd.RData", sep=""))
  # BaseData <- all_data
  cat(paste("Running Node", NodeName))
  
  # NodeData <- NodeData %>%
  #   mutate(DateTime = F34D$Q_ts.datetime)
  
  hydraul <-NodeData
  
  ## change some names
  hydraul <- hydraul %>%
    rename(Q = Flow) %>%
    mutate(node = NodeName)
  
  
  ## convert units and change names - depending on concrete/soft bottom. if/else to determine changes to data
  
  if(length(NodeData) == 9) {
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
  
  ## take only depth variable for min limit
  hyd_dep <- hyd_vel %>% select(Scenario, DateTime, node, Q, contains("depth"), date_num)
  
  
  hyd_dep<-reshape2::melt(hyd_dep, id=c("Scenario", "DateTime","Q", "node", "date_num"))
  hyd_dep <- hyd_dep %>%
    mutate(depth_cm = value) %>%
    select(date_num, depth_cm)
  
  ## take only depth variable
  hyd_vel <- hyd_vel %>% select(Scenario, DateTime, node, Q, contains("vel"), date_num)
  
  # ## melt channel position data
  hyd_vel<-reshape2::melt(hyd_vel, id=c("Scenario","DateTime","Q", "node", "date_num"))
  ## change NAs to 0 in concrete overbanks
  hyd_vel[is.na(hyd_vel)] <- 0
  
  ## join depth data to vel df
  hyd_vel <- left_join(hyd_vel, hyd_dep, by="date_num")
  
  ## change NAs to 0 in concrete overbanks
  hyd_vel[is.na(hyd_vel)] <- 0
  
  ## use smooth spline to predict on new data set
  new_values <-smooth.spline(fitdata$velocity_fit, fitdata$prob_fit)
  
  all_data <- hyd_vel %>%
    group_by(variable) %>%
    mutate(prob_fit = predict(new_values, value)$y) %>%
    rename(vel_m = value)
  
  
  
  ## save out
  # save(all_data, file=paste("output_data/F1_", NodeName, "_SAS_adult_depth_discharge_probability_updated_hyd_.RData", sep=""))
  
  
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

  # save(all_data, file=paste("output_data/F1_", NodeName, "_SAS_depth_adult_discharge_probs_2010_2017_TS_updated_hyd.RData", sep=""))
  
  ### define dataframes for 2nd loop
  
  ## define positions
  positions <- unique(all_data$variable)
  scenarios <- unique(all_data$Scenario)
  
  ## upload and format Q limits and calculation
  Q_Calc <- read.csv(paste("output_data/F1_", NodeName, "_SAS_adult_velocity_Q_calculation_updated_hyd.csv", sep=""))
  
  if(length(NodeData) == 9) {
    Q_Calc <- Q_Calc %>%
      mutate(Position = c("MC"))
    Q_Calc <- na.omit(Q_Calc)
  } else {
    Q_Calc <- Q_Calc %>%
      mutate(Position = c("LOB", "MC", "ROB"))
  }
  
  
  limits <- read.csv(paste("output_data/F1_", NodeName, "_SAS_adult_velocity_Q_limits_updated_hyd.csv", sep=""))  
  
  if(length(NodeData) == 9) {
    limits <- limits %>%
      rename(MC = V1) %>%
      filter(Type == "Q_limit")
  } else {
    limits <- limits %>%
      rename(LOB = V1, MC = V2, ROB = V3) %>%
      filter(Type == "Q_limit")
  }
  
  limits$ProbThresh[1:4] <- "Low"
  limits$ProbThresh[5:8] <- "Medium"
  limits$ProbThresh[9:12] <- "High"
  
  melt_timex <- NULL
  melt_daysx <- NULL
  s
  # Select Scenarios -----------------------------------
  
  for(s in 1:length(scenarios)) {
    
    scen_data <- all_data %>% 
      filter(Scenario  == scenarios[s])
    
    time_statsx <- NULL
    days_data <- NULL
    
    cat(paste("Running Scenario", s))
    
    for(p in 1:length(positions)) {
      
      new_data <- scen_data %>% 
        filter(variable  == positions[p])
      
      min_limit <- filter(new_data, depth_cm > 0.03)
      min_limit <- min(min_limit$Q)
     
      # new_base_data <- BaseData %>% 
      #   filter(variable  == positions[p])
      
      ## define position
      PositionName <- str_split(positions[p], "_", 3)[[1]]
      PositionName <- PositionName[3]
      
      ## Q limits
      ## low
      newx1ax <- limits %>%
        select(all_of(PositionName), ProbThresh) %>%
        filter(ProbThresh == "Low")
      
      newx1a <- newx1ax[,1]
      newx1a <- na.omit(newx1a)
      # medium
      newx2ax <- limits %>%
        select(all_of(PositionName), ProbThresh) %>%
        filter(ProbThresh == "Medium")
      
      newx2a <- newx2ax[,1]
      newx2a <- na.omit(newx2a)
      
      # high
      newx3ax <- limits %>%
        select(all_of(PositionName), ProbThresh) %>%
        filter(ProbThresh == "High")
      
      newx3a <- newx3ax[,1]
      newx3a <- na.omit(newx3a)
      
      ## Q calculation
      
      
      low_threshx <- Q_Calc %>%
        select(Low, Position) %>%
        filter(Position == PositionName)
      
      low_thresh <- parse(text = low_threshx[,1])
      
      med_threshx <- Q_Calc %>%
        select(Medium, Position) %>%
        filter(Position == PositionName)
      
      med_thresh <- parse(text = med_threshx[,1])
      
      high_threshx <- Q_Calc %>%
        select(High, Position) %>%
        filter(Position == PositionName)
      
      high_thresh <- parse(text = high_threshx[,1])
      
      # create year_month column       
      new_datax <- new_data %>% unite(month_year, c(water_year,month), sep="-", remove=F) 
      
      # dataframe for stats -----------------------------------------------------
      
      ## define critical period or season for adult as all year is critical
      winter <- c(1,2,3,4,11,12) ## winter months
      summer <- c(5:10) ## summer months
      
      new_datax <- new_datax %>%
        mutate(season = ifelse(month %in% winter, "winter", "summer") )
      
      
      
      ###### calculate amount of time
      
     
      time_stats <- new_datax %>%
        dplyr::group_by(water_year) %>%
        dplyr::mutate(Low = sum(eval(low_thresh))/length(DateTime)*100) %>%
        dplyr::mutate(Medium = sum(eval(med_thresh))/length(DateTime)*100) %>%
        dplyr::mutate(High = sum(eval(high_thresh))/length(DateTime)*100) %>%
        ungroup() %>%
        dplyr::group_by(water_year, season) %>%
        dplyr::mutate(Low.Seasonal = sum(eval(low_thresh))/length(DateTime)*100) %>%
        dplyr::mutate(Medium.Seasonal = sum(eval(med_thresh))/length(DateTime)*100) %>%
        dplyr::mutate(High.Seasonal = sum(eval(high_thresh))/length(DateTime)*100) %>%
        distinct(water_year, Low , Medium , High , Low.Seasonal, Medium.Seasonal, High.Seasonal) %>%
        mutate(position= paste(PositionName), Node = NodeName)
     
      time_statsx <- rbind(time_statsx, time_stats)
      
      ### count days per month
      new_datax <- new_datax %>%
        ungroup() %>%
        group_by(month,  water_year, ID01 = data.table::rleid(eval(low_thresh))) %>%
        mutate(Low = if_else(eval(low_thresh), row_number(), 0L)) %>%
        ungroup() %>%
        group_by(month,  water_year, ID02 = data.table::rleid(eval(med_thresh))) %>%
        mutate(Medium = if_else(eval(med_thresh), row_number(), 0L)) %>%
        ungroup() %>%
        group_by(month,  water_year, ID03 = data.table::rleid(eval(high_thresh))) %>%
        mutate(High = if_else(eval(high_thresh), row_number(), 0L)) %>%
        mutate(position= paste(PositionName)) #%>%
      # select(Q, month, water_year, day, ID01, Low, ID02, Medium, ID03, High, position, DateTime, node) 
      
      days_data <- rbind(days_data, new_datax)
      
    } ## end 3rd loop - positions
    
    ## limits
    ## note that 0.1 upper/lower limit is max/min Q to adhere to 0.1 bound
    
    ## percentage time
    melt_time<-reshape2::melt(time_statsx, id=c("season", "position", "water_year", "Node"))
    melt_time <- melt_time %>% 
      rename( Probability_Threshold = variable) %>%
      mutate(Species ="SAS", Life_Stage = "Adult", Hydraulic = "Velocity", Node = NodeName, Scenario =scenarios[s])
    
    melt_timex <- rbind(melt_timex, melt_time)
    
    ### days per month
    days_data <- select(days_data, c(Q, month, water_year, day, ID01, Low, ID02, Medium, ID03, High, position, DateTime, node) )# all probs
    
    melt_data<-reshape2::melt(days_data, id=c("ID01", "ID02", "ID03", "day", "month", "water_year", "Q", "position", "node"))
    melt_data <- rename(melt_data, Probability_Threshold = variable, 
                        consec_hours = value)
    
    ## count how many full days i.e. 24 hours
    total_days01 <- melt_data %>% 
      filter(Probability_Threshold == "Low") %>% 
      group_by(ID01, day, month, water_year, position) %>%
      summarise(n_hours = max(consec_hours))  %>%
      mutate(n_days_low = ifelse(n_hours >= 24, 1, 0)) # %>%
    
    ## count the number of days in each month
    total_days_per_month01 <- total_days01 %>%
      group_by(month, water_year, position) %>%
      summarise(days_per_month_low = sum(n_days_low))
    
    
    total_days02 <- melt_data %>% 
      filter(Probability_Threshold == "Medium") %>% 
      group_by(ID02, day, month, water_year, position) %>%
      summarise(n_hours = max(consec_hours))  %>%
      mutate(n_days_medium = ifelse(n_hours >= 24, 1, 0)) # %>%
    
    total_days_per_month02 <- total_days02 %>%
      group_by(month, water_year, position) %>%
      summarise(days_per_month_medium = sum(n_days_medium))
    
    # total_days_per_month02
    
    total_days03 <- melt_data %>% 
      filter(Probability_Threshold == "High") %>% 
      group_by(ID03, day, month, water_year, position) %>%
      summarise(n_hours = max(consec_hours))  %>%
      mutate(n_days_high = ifelse(n_hours >= 24, 1, 0)) # %>%
    
    total_days_per_month03 <- total_days03 %>%
      group_by(month, water_year, position) %>%
      summarise(days_per_month_high = sum(n_days_high))
    
    ## combine all thresholds
    total_days <- cbind( total_days_per_month01,total_days_per_month02[,4], total_days_per_month03[,4])
    
    
    # # create year_month column       
    total_days <- ungroup(total_days) %>%
      unite(month_year, water_year:month, sep="-", remove=F) %>%
      mutate(Node= paste(NodeName)) #%>%
    
    ## convert month year to date format
    
    total_days$month_year <-  zoo::as.yearmon(total_days$month_year)
    total_days$month_year <- as.Date(total_days$month_year)
    
    ## change names of columns
    total_days <- rename(total_days, Low = days_per_month_low, Medium = days_per_month_medium, High = days_per_month_high)
    
    ## define seasons
    winter <- c(1,2,3,4,11,12) ## winter months
    summer <- c(5:10) ## summer months
    
    total_days <- total_days %>%
      mutate(season = ifelse(month %in% winter, "winter", "summer") )
    
    # ## melt data
    
    melt_days<-reshape2::melt(total_days, id=c("month_year", "water_year", "month", "season", "position", "Node"))
    melt_days <- melt_days %>%
      rename(Probability_Threshold = variable, n_days = value) %>%
      mutate(Species ="SAS", Life_Stage = "Adult", Hydraulic = "Velocity", Scenario =scenarios[s])
    
    melt_daysx <- rbind(melt_daysx, melt_days)
    
  } ## end 2nd loop scenarios
  
  ## save df
  write.csv(melt_timex, paste("output_data/scenarios/F1_", NodeName, "_SAS_adult_velocity_time_stats_updated_hyd_scenarios.csv", sep=""))
  
  write.csv(melt_days, paste("output_data/scenarios/F1_", NodeName, "_SAS_adult_velocity_total_days_long_updated_hyd_scenarios.csv", sep="") )
  
  
} ## end 1st loop - node

