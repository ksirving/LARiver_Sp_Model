## takes Q limits from baseline assessment and evaluates suitability under WRP scenarios

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

setwd("/Users/katieirving/Documents/git/flow_eco_mech")
## upload hydraulic data
setwd("input_data/HecRas/scenarios")

h <- list.files(pattern="_predictions")
length(h) ## 

## set wd back to main
setwd("/Users/katieirving/Documents/git/flow_eco_mech")

## upload and format Q limits and calculation
L <- list.files(path = "output_data", pattern="_limits_updated_hyd")
L

C <- list.files(path = "output_data", pattern="_calculation_updated_hyd")
C
length(L) 
# Q_Calc <- read.csv(paste("output_data/F1_", NodeName, "_SAS_juvenile_depth_Q_calculation_updated_hyd.csv", sep=""))


for(n in 1: length(h)) {
  
  NodeData <- read.csv(file=paste("input_data/HecRas/scenarios/", h[n], sep=""))
  # BaseData <- read.csv("input_data/HecRas/hydraulic_ts_F34D.csv") ## for dates
  NodeName <- str_split(h[n], "_", 3)[[1]]
  NodeName <- NodeName[1]
  
  QL <- Filter(function(x) grepl(paste(NodeName), x), L)

  QC <- Filter(function(x) grepl(paste(NodeName), x), C)

  ## format hydraulic data
  
  cat(paste("Running Node", NodeName))
  
  ## change some names
  hydraul <- NodeData %>%
    rename(Q = Flow) %>%
    mutate(node = NodeName)
  
  
  ## convert units and change names - depending on concrete/soft bottom. if/else to determine changes to data
  
  if(length(NodeData) == 9) {
    hyd_dep <- hydraul %>%
      mutate(depth_cm_MC = (Max..Depth..ft..MC*0.3048)*100) %>%
      mutate(shear_pa_MC = (Shear..lb.sq.ft..MC/0.020885)) %>%
      mutate(StreamPower_w_MC = (Shear..lb.sq.ft..MC*4.44822)/0.3048) %>%
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
      mutate(StreamPower_w_LOB = (Shear..lb.sq.ft..LOB*4.44822)/0.3048,
             StreamPower_w_MC = (Shear..lb.sq.ft..MC*4.44822)/0.3048,
             StreamPower_w_ROB = (Shear..lb.sq.ft..ROB*4.44822)/0.3048) %>%
      mutate(velocity_m_LOB = (Avg..Vel...ft.s..LOB*0.3048),
             velocity_m_MC = (Avg..Vel...ft.s..MC*0.3048),
             velocity_m_ROB = (Avg..Vel...ft.s..ROB*0.3048)) %>%
      select(-contains("ft")) %>%
      mutate(date_num = seq(1,length(DateTime), 1))
    
  }
  
  
  
  # ## take only depth variable
  # hyd_dep <- hyd_dep %>% select(Scenario, DateTime, node, Q, date_num)
  
  # ## melt channel position data
  all_data<-reshape2::melt(hyd_dep, id=c("Scenario", "DateTime","Q", "node", "date_num"))

  # format probability time series ------------------------------------------
  
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
  
  # save(all_data, file=paste("output_data/F1_", NodeName, "_SAS_depth_juvenile_discharge_probs_2010_2017_TS_updated_hyd.RData", sep=""))
  
  ### define dataframes for 2nd loop
  
  ## define positions
  # all_positions <- unique(all_data$variable)
  scenarios <- unique(all_data$Scenario)
  
  for(c in 1: length(QC)) {
    
    ## upload and format Q limits and calculation
    Q_Calc <- read.csv(file=paste("output_data/", QC[c], sep=""))
  
    QName <- str_split(QC[c], "_", 6)[[1]]

    SpeciesName <- QName[3]
    LifeStageName <- QName[4]
    HydraulicName <- QName[5]
    
    cat(paste("Running Species", SpeciesName))
    cat(paste("Running Life Stage", LifeStageName))
    cat(paste("Running Variable", HydraulicName))

  # Q_Calc <- read.csv(paste("output_data/F1_", NodeName, "_SAS_juvenile_depth_Q_calculation_updated_hyd.csv", sep=""))
  
  if(length(NodeData) == 9) {
    Q_Calc <- Q_Calc %>%
      mutate(Position = c("MC"))
    Q_Calc <- na.omit(Q_Calc)
  } else {
    Q_Calc <- Q_Calc %>%
      mutate(Position = c("LOB", "MC", "ROB"))
  }
  
  # limits <- read.csv(paste("output_data/F1_", NodeName, "_SAS_juvenile_depth_Q_limits_updated_hyd.csv", sep=""))  
  
    lim <- Filter(function(x) grepl(paste(SpeciesName), x), QL)
    lim <- Filter(function(x) grepl(paste(LifeStageName), x), lim)
    lim <- Filter(function(x) grepl(paste(HydraulicName), x), lim)
  lim
    limits <- read.csv(file=paste("output_data/", lim, sep=""))
    
 
    
  if(length(NodeData) == 9) {
    limits <- limits %>%
      rename(MC = V1) 
  } else {
    limits <- limits %>%
      rename(LOB = V1, MC = V2, ROB = V3) 
  }
    
    if(dim(limits)[1] > 4) {
      
      limits <- limits %>%
        filter(Type == "Q_limit")
      
      limits$ProbThresh[1:4] <- "Low"
      limits$ProbThresh[5:8] <- "Medium"
      limits$ProbThresh[9:12] <- "High"
      
    } else { 
      limits <- limits %>%
        filter(Type == c("Q_limit1", "Q_limit2") )
      limits$Thresh[1] <- "newx1a"
      limits$Thresh[2] <- "newx2a"
        
    }
  
 
  melt_timex <- NULL
  melt_daysx <- NULL

  # Select Scenarios -----------------------------------

  for(s in 1:length(scenarios)) {
    
    scen_data <- all_data %>% 
      filter(Scenario  == scenarios[s]) %>%
      pivot_wider(names_from = variable, values_from = value)
    
    ## define position
    positions <- scen_data %>% 
      select(starts_with(HydraulicName))
    
    positions <- names(positions)
    
    time_statsx <- NULL
    days_data <- NULL
    
    # cat(paste("Running Scenario", s))
 

    for(p in 1:length(positions)) {
      
      new_data <- scen_data %>% 
        select(Scenario, DateTime, Q, node, date_num, month, year, day, water_year, starts_with(HydraulicName))
      
      
      PositionName <- str_split(positions[p], "_", 3)[[1]]
      PositionName <- PositionName[3]
      
      min_limit <- scen_data %>% 
        select(Q,  contains(PositionName)) %>%
        select(Q, starts_with("depth"))
      
      depth_pos <- names(min_limit)[2]
      min_limit <- filter(min_limit, depth_pos >0.03)
      min_limit <- min(min_limit$Q)
        
   
      ## Q limits
      if(dim(limits)[1] == 12) {
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
      } else {
        
        newx1ax <- limits %>%
          select(all_of(PositionName), Thresh) %>%
          filter(Thresh == "newx1a")
        newx1a <- newx1ax[,1]
        
        newx2ax <- limits %>%
          select(all_of(PositionName), Thresh) %>%
          filter(Thresh == "newx2a")
        newx2a <- newx2ax[,1]
        
        threshx <- Q_Calc %>%
          select(Thresh, Position) %>%
          filter(Position == PositionName)
        
        thresh <- parse(text = threshx[,1])
        
        
      }
      
      
      # create year_month column       
      new_datax <- new_data %>% unite(month_year, c(water_year,month), sep="-", remove=F) 
      
      # dataframe for stats -----------------------------------------------------
      
      ## define critical period or season for juvenile as all year is critical
      winter <- c(1,2,3,4,11,12) ## winter months
      summer <- c(5:10) ## summer months
      
      new_datax <- new_datax %>%
        mutate(season = ifelse(month %in% winter, "winter", "summer") )
      
      ###### calculate amount of time
      
      if(dim(limits)[1] == 12) {
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
      
        
      } else {
        time_stats <- new_datax %>%
          dplyr::group_by(water_year, season) %>%
          dplyr::mutate(Seasonal = sum(eval(thresh))/length(DateTime)*100) %>%
          distinct(water_year,  Seasonal) %>%
          mutate(position= paste(PositionName), Node = NodeName)
         
      }
      
      time_statsx <- rbind(time_statsx, time_stats)
      
      ### count days per month
      
      if(dim(limits)[1] == 12) {
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
      
      } else {
        new_datax <- new_datax %>% 
          group_by(month, day, water_year, ID = data.table::rleid(eval(thresh))) %>%
          mutate(threshold = if_else(eval(thresh),  row_number(), 0L)) %>%
          mutate(position= paste(PositionName)) 
      }
      
      days_data <- rbind(days_data, new_datax)
      
    } ## end 3rd loop - positions
    
     ## in scenario loop
   
    ## percentage time
    melt_time<-reshape2::melt(time_statsx, id=c("season", "position", "water_year", "Node"))
    melt_time <- melt_time %>% 
      rename( Probability_Threshold = variable) %>%
      mutate(Species = SpeciesName, Life_Stage = LifeStageName, Hydraulic = HydraulicName, Node = NodeName, Scenario =scenarios[s])
   
    melt_timex <- rbind(melt_timex, melt_time)
    
    ### days per month
    if(dim(limits)[1] == 12) {
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
      mutate(Species = SpeciesName, Life_Stage = LifeStageName, Hydraulic = HydraulicName, Scenario =scenarios[s])
    
    
    } else {
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
      head(total_days)
      # # create year_month column       
      total_days <- ungroup(total_days) %>%
        # unite(month_year, water_year:month, sep="-", remove=F) %>%
        mutate(Node= paste(NodeName)) #%>%
      
      ## convert month year to date format
      
      total_days$month_year <-  zoo::as.yearmon(total_days$month_year)
      total_days$month_year <- as.Date(total_days$month_year)
      
      ## define seasons
      winter <- c(1,2,3,4,11,12) ## winter months
      summer <- c(5:10) ## summer months
      
      total_days <- total_days %>%
        mutate(season = ifelse(month %in% winter, "winter", "summer") )
      
      # ## melt data
      
      melt_days<-reshape2::melt(total_days, id=c("month_year", "water_year", "month", "season", "position", "Node"))
      melt_days <- melt_days %>%
        rename( n_days = value) %>%
        select(-variable) %>%
        mutate(Species = SpeciesName, Life_Stage = LifeStageName, Hydraulic = HydraulicName, Scenario =scenarios[s])
      
    }
    
    
    melt_daysx <- rbind(melt_daysx, melt_days)
    head(melt_daysx)
  } ## end 2nd loop scenarios
  
  ## in species hydraulic loop
  
  ## save df
  write.csv(melt_timex, paste("output_data/scenarios/X1", NodeName, SpeciesName, LifeStageName, HydraulicName,  "time_stats_updated_hyd_scenarios.csv", sep="_"))
  
  write.csv(melt_days, paste("output_data/scenarios/X1", NodeName, SpeciesName, LifeStageName, HydraulicName,  "total_days_long_updated_hyd_scenarios.csv", sep="_") )
  
  } ## end species hydraulic loop
  
  ## back to species loop
  
} ## end 1st loop - node
