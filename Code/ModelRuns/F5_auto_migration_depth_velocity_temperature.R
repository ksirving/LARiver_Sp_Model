## migration

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
setwd("/Users/katieirving/Documents/git/flow_eco_mech")
## upload hydraulic data
setwd("input_data/HecRas")

h <- list.files(pattern="hydraulic")
length(h) ## 18
h
n=10

## set wd back to main
setwd("/Users/katieirving/Documents/git/flow_eco_mech")

for(n in 1: length(h)) {
  
  NodeData <- read.csv(file=paste("input_data/HecRas/", h[n], sep=""))
  F34D <- read.csv("input_data/HecRas/hydraulic_ts_F34D.csv") ## for dates
  head(NodeData)
  ## format hydraulic data
  
  NodeData <- NodeData %>%
    mutate(Q_ts.datetime = F34D$Q_ts.datetime)
  
  hydraul <-NodeData[,-1]
  
  ## change some names
  hydraul <- hydraul %>%
    rename(DateTime = Q_ts.datetime, node = Gage, Q = Flow)
  
  ## define node name
  NodeName <- unique(hydraul$node)
  
  ## convert units and change names
  
  hyd_dep <- hydraul %>%
    mutate(depth_cm_LOB = (Hydr..Depth..ft..LOB*0.3048)*100,
           depth_cm_MC = (Hydr..Depth..ft..MC*0.3048)*100,
           depth_cm_ROB = (Hydr..Depth..ft..ROB*0.3048)*100) %>%
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
    mutate(season = ifelse(month == 12 | month == 1 | month == 2 | month == 3 | month == 4| month == 5 | month == 6, 
                           paste("critical"), paste("non_critical") )) %>%    
    mutate(water_year = ifelse(month == 10 | month == 11 | month == 12, year, year-1))

  ## save out
  save(all_data, file=paste("output_data/F5_", NodeName, "_Steelhead_depth_Migration_discharge_probs_2010_2017_TS.RData", sep=""))
  
  # format probability time series ------------------------------------------
  
  ### define dataframes for 2nd loop
  
  ## Q Limits
  limits <- as.data.frame(matrix(ncol=3, nrow=1)) %>%
    rename(LOB = V1, MC = V2, ROB = V3) 
  rownames(limits)<-c("Lower_limit")
  
  time_statsx <- NULL
  days_data <- NULL
  
  ## define positions
  positions <- unique(all_data$variable)
  p=2
  # probability as a function of discharge -----------------------------------
  
  for(p in 1:length(positions)) {
    
    new_data <- all_data %>% 
      filter(variable  == positions[p])
    
    ## define position
    PositionName <- str_split(positions[p], "_", 3)[[1]]
    PositionName <- PositionName[3]
    PositionName
    
    ## get roots
    curve <- spline(new_data$Q, new_data$value,
                    xmin = min(new_data$Q), xmax = max(new_data$Q), ties = mean)
    
    if(min(curve$y) ==0 && max(curve$y) ==0) {
      newx1a <- 0
    } else if(min(curve$y)>18) {
      newx1a <- min(curve$x)
    } else {
      newx1a <- approx(x = curve$y, y = curve$x, xout = 18)$y
    }
    newx1a

    range(new_data$value)
    range(new_data$Q)

    
    ## MAKE DF OF Q LIMITS
    limits[,p] <- c(newx1a)
    
    # create year_month column       
    new_datax <- new_data %>% unite(month_year, c(water_year,month), sep="-", remove=F) 
    
    # dataframe for stats -----------------------------------------------------
    
    ## define critical period or season for juvenile as all year is critical
    
    
    
    ###### calculate amount of time
    time_stats <- new_data %>%
      dplyr::group_by(water_year, season) %>%
      dplyr::mutate(Seasonal = sum(Q <= newx1a)/length(DateTime)*100) %>%
      distinct(water_year,  Seasonal) %>%
      mutate(position= paste(PositionName), Node = NodeName)
    time_stats
    
    time_statsx <- rbind(time_statsx, time_stats)
    
    ### count days per month
    new_datax <- new_datax %>% 
      group_by(month, day, water_year, ID = data.table::rleid(Q >= newx1a)) %>%
      mutate(threshold = if_else(Q >= newx1a,  row_number(), 0L)) %>%
      mutate(position= paste(PositionName)) 
    
    
    days_data <- rbind(days_data, new_datax)
    
    
  } ## end 2nd loop
  
  ## limits
  ## note that 0.1 upper/lower limit is max/min Q to adhere to 0.1 bound
  limits <- limits %>%
    mutate(Species ="Steelhead", Life_Stage = "Migration", Hydraulic = "Depth", Node = NodeName)
  
  write.csv(limits, paste("output_data/F5_",NodeName,"_Steelhead_Migration_depth_Q_limits.csv", sep=""))
  
  
  file_name = paste("figures/Application_curves/Depth/", NodeName, "_Steelhead_Migration_depth_prob_Q_thresholds.png", sep ="")
  
  png(file_name, width = 500, height = 600)
  
  ggplot(all_data, aes(x = Q, y=value)) +
    geom_line(aes(group = variable, lty = variable)) +
    scale_linetype_manual(values= c("dotted", "solid", "dashed"))+
    #                       name="Cross\nSection\nPosition",
    #                       breaks=c("depth_cm_LOB", "depth_cm_MC", "depth_cm_ROB"),
    #                         labels = c("LOB", "MC", "ROB")) +
    
    facet_wrap(~variable, scales="free_x", nrow=3, labeller=labeller(variable = labels)) +
    # geom_point(data = subset(all_data, variable =="depth_cm_MC"), aes(y=0.1, x=limits[1,2]), color="green") +
    geom_point(data = subset(hyd_dep, variable =="depth_cm_MC"), aes(y=18, x=limits[1,2]), color="green") +
    # geom_point(data = subset(hyd_dep, variable =="depth_cm_MC"), aes(y=10, x=limits[1,2]), color="green") +
    
    geom_point(data = subset(hyd_dep, variable =="depth_cm_LOB"), aes(y=18, x=limits[1,1]), color="green") +
    # geom_point(data = subset(hyd_dep, variable =="depth_cm_LOB"), aes(y=10, x=limits[1,1]), color="green") +
    
    geom_point(data = subset(hyd_dep, variable =="depth_cm_ROB"), aes(y=18, x=limits[1,3]), color="green") +
    # geom_point(data = subset(hyd_dep, variable =="depth_cm_ROB"), aes(y=10, x=limits[1,3]), color="green") +
    
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
    labs(title = paste(NodeName, ": Migration/Depth: Depth ~ Q", sep=""),
         y = "Probability",
         x = "Q (cfs)") #+ theme_bw(base_size = 15)
  
  dev.off()
  
  ## percentage time
  melt_time<-reshape2::melt(time_statsx, id=c("season", "position", "water_year", "Node"))
  melt_time <- melt_time %>% 
    rename( Season = variable) %>%
    mutate(Species ="Steelhead", Life_Stage = "Migration", Hydraulic = "Depth", Node = NodeName)
  
  write.csv(melt_time, paste("output_data/F5_", NodeName, "_Steelhead_Migration_depth_time_stats.csv", sep=""))
  
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
    mutate(Species ="Steelhead", Life_Stage = "Migration", Hydraulic = "Depth")
  
  
  ## save df
  write.csv(melt_days, paste("output_data/F5_", NodeName, "_Steelhead_Migration_depth_total_days_long.csv", sep="") )
  
} ## end 1st loop

n=1
# Velocity ----------------------------------------------------------------

setwd("/Users/katieirving/Documents/git/flow_eco_mech")

for(n in 1: length(h)) {
  
  NodeData <- read.csv(file=paste("input_data/HecRas/", h[n], sep=""))
  F34D <- read.csv("input_data/HecRas/hydraulic_ts_F34D.csv") ## for dates
  
  ## format hydraulic data
  
  NodeData <- NodeData %>%
    mutate(Q_ts.datetime = F34D$Q_ts.datetime)
  
  hydraul <-NodeData[,-1]
  
  ## change some names
  hydraul <- hydraul %>%
    rename(DateTime = Q_ts.datetime, node = Gage, Q = Flow)
  
  ## define node name
  NodeName <- unique(hydraul$node)
  
  ## convert units and change names
  
  hyd_vel <- hydraul %>%
    mutate(depth_cm_LOB = (Hydr..Depth..ft..LOB*0.3048)*100,
           depth_cm_MC = (Hydr..Depth..ft..MC*0.3048)*100,
           depth_cm_ROB = (Hydr..Depth..ft..ROB*0.3048)*100) %>%
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
  
  
  ## take only depth variable
  hyd_vel <- hyd_vel %>% select(DateTime, node, Q, contains("vel"), date_num)
  
  # ## melt channel position data
  hyd_vel<-reshape2::melt(hyd_vel, id=c("DateTime","Q", "node", "date_num"))
  ## change NAs to 0 in concrete overbanks
  hyd_vel[is.na(hyd_vel)] <- 0
  
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
    mutate(season = ifelse(month == 12 | month == 1 | month == 2 | month == 3 | month == 4| month == 5 | month == 6, 
                           paste("critical"), paste("non_critical") )) %>%
    mutate(water_year = ifelse(month == 10 | month == 11 | month == 12, year, year-1))
  
  
  ## save out
  save(all_data, file=paste("output_data/F5_", NodeName, "_Steelhead_velocity_Migration_discharge_probs_2010_2017_TS.RData", sep=""))
  
  # format probability time series ------------------------------------------
  
  ### define dataframes for 2nd loop
  
  ## Q Limits
  limits <- as.data.frame(matrix(ncol=3, nrow=2)) %>%
    rename(LOB = V1, MC = V2, ROB = V3) 
  rownames(limits)<-c("Lower_limit",
                      "Upper_Limit")
  
  time_statsx <- NULL
  days_data <- NULL
  
  ## define positions
  positions <- unique(all_data$variable)

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
    } else if(min(curve$y)>0) {
      newx1a <- min(curve$x)
    } else {
      newx1a <- approx(x = curve$y, y = curve$x, xout = 0.0001)$y
    }


    if(min(curve$y) ==0 && max(curve$y) ==0) {
      newx2a <- 0
    } else  if(max(curve$y)<3.1) {
      newx2a <- max(curve$x)
    } else {
      newx2a <- approx(x = curve$y, y = curve$x, xout = 3.1)$y
    }

    
    ## MAKE DF OF Q LIMITS
    limits[,p] <- c(newx1a, newx2a)
    
    # create year_month column       
    new_datax <- new_data %>% unite(month_year, c(water_year,month), sep="-", remove=F) 
    
    # dataframe for stats -----------------------------------------------------
    
    ## define critical period or season for juvenile as all year is critical
    # Q >= newx1a & 
    ###### calculate amount of time
    time_stats <- new_data %>%
      dplyr::group_by(water_year, season) %>%
      dplyr::mutate(Seasonal = sum(Q <= newx2a)/length(DateTime)*100) %>%
      distinct(water_year,  Seasonal) %>%
      mutate(position= paste(PositionName), Node = NodeName)

    
    time_statsx <- rbind(time_statsx, time_stats)
    
    ### count days per month
    new_datax <- new_datax %>% 
      group_by(month, day, water_year, ID = data.table::rleid(Q >= newx1a & Q <= newx2a)) %>%
      mutate(threshold = if_else(Q >= newx1a & Q <= newx2a,  row_number(), 0L)) %>%
      mutate(position= paste(PositionName)) 
    
    
    days_data <- rbind(days_data, new_datax)
    
    
  } ## end 2nd loop
  
  ## limits
  ## note that 0.1 upper/lower limit is max/min Q to adhere to 0.1 bound
  limits <- limits %>%
    mutate(Species ="Steelhead", Life_Stage = "Migration", Hydraulic = "Velocity", Node = NodeName)
  
  write.csv(limits, paste("output_data/F5_",NodeName,"_Steelhead_Migration_velocity_Q_limits.csv", sep=""))
  
  
  file_name = paste("figures/Application_curves/Depth/", NodeName, "_Steelhead_Migration_velocity_prob_Q_thresholds.png", sep ="")
  
  png(file_name, width = 500, height = 600)
  
  ggplot(all_data, aes(x = Q, y=value)) +
    geom_line(aes(group = variable, lty = variable)) +
    scale_linetype_manual(values= c("dotted", "solid", "dashed"))+
    #                       name="Cross\nSection\nPosition",
    #                       breaks=c("depth_cm_LOB", "depth_cm_MC", "depth_cm_ROB"),
    #                         labels = c("LOB", "MC", "ROB")) +
    
    facet_wrap(~variable, scales="free_x", nrow=3, labeller=labeller(variable = labels)) +
    
    geom_point(data = subset(hyd_vel, variable =="vel_m_MC"), aes(y=0, x=limits[1,2]), color="green") +
    geom_point(data = subset(hyd_vel, variable =="vel_m_MC"), aes(y=3.1, x=limits[2,2]), color="green") +
    
    geom_point(data = subset(hyd_vel, variable =="vel_m_LOB"), aes(y=0, x=limits[1,1]), color="green") +
    geom_point(data = subset(hyd_vel, variable =="vel_m_LOB"), aes(y=3.1, x=limits[2,1]), color="green") +
    
    geom_point(data = subset(hyd_vel, variable =="vel_m_ROB"), aes(y=0, x=limits[1,3]), color="green") +
    geom_point(data = subset(hyd_vel, variable =="vel_m_ROB"), aes(y=3.1, x=limits[2,3]), color="green") +
    
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
    labs(title = paste(NodeName, ": Migration: Velocity ~ Q", sep=""),
         y = "Velocity (m/s)",
         x = "Q (cfs)") #+ theme_bw(base_size = 15)
  
  dev.off()
  
  ## percentage time
  melt_time<-reshape2::melt(time_statsx, id=c("season", "position", "water_year", "Node"))
  melt_time <- melt_time %>% 
    rename( Season = variable) %>%
    mutate(Species ="Steelhead", Life_Stage = "Migration", Hydraulic = "Velocity", Node = NodeName)
  
  write.csv(melt_time, paste("output_data/F5_", NodeName, "_Steelhead_Migration_velocity_time_stats.csv", sep=""))
  
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
    mutate(Species ="Steelhead", Life_Stage = "Migration", Hydraulic = "Velocity")
  
  
  ## save df
  write.csv(melt_days, paste("output_data/F5_", NodeName, "_Steelhead_Migration_velocity_total_days_long.csv", sep="") )
  
} ## end 1st loop



#  smolts -----------------------------------------------------------------

for(n in 1: length(h)) {
  
  NodeData <- read.csv(file=paste("input_data/HecRas/", h[n], sep=""))
  F34D <- read.csv("input_data/HecRas/hydraulic_ts_F34D.csv") ## for dates
  
  ## format hydraulic data
  
  NodeData <- NodeData %>%
    mutate(Q_ts.datetime = F34D$Q_ts.datetime)
  
  hydraul <-NodeData[,-1]
  
  ## change some names
  hydraul <- hydraul %>%
    rename(DateTime = Q_ts.datetime, node = Gage, Q = Flow)
  
  ## define node name
  NodeName <- unique(hydraul$node)
  
  ## convert units and change names
  
  hyd_dep <- hydraul %>%
    mutate(depth_cm_LOB = (Hydr..Depth..ft..LOB*0.3048)*100,
           depth_cm_MC = (Hydr..Depth..ft..MC*0.3048)*100,
           depth_cm_ROB = (Hydr..Depth..ft..ROB*0.3048)*100) %>%
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
    mutate(season = ifelse(month == 12 | month == 1 | month == 2 | month == 3 | month == 4| month == 5 | month == 6 | month == 7, 
                           paste("critical"), paste("non_critical") )) %>%   
    mutate(water_year = ifelse(month == 10 | month == 11 | month == 12, year, year-1))
  
  
  ## save out
  save(all_data, file=paste("output_data/F5_", NodeName, "_Steelhead_depth_Smolts_discharge_probs_2010_2017_TS.RData", sep=""))
  
  # format probability time series ------------------------------------------
  
  ### define dataframes for 2nd loop
  
  ## Q Limits
  limits <- as.data.frame(matrix(ncol=3, nrow=1)) %>%
    rename(LOB = V1, MC = V2, ROB = V3) 
  rownames(limits)<-c("Lower_limit")
  
  time_statsx <- NULL
  days_data <- NULL
  
  ## define positions
  positions <- unique(all_data$variable)
  
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
    } else if(min(curve$y)>12) {
      newx1a <- min(curve$x)
    } else {
      newx1a <- approx(x = curve$y, y = curve$x, xout = 12)$y
    }
    
    
    
    ## MAKE DF OF Q LIMITS
    limits[,p] <- c(newx1a)
    
    # create year_month column       
    new_datax <- new_data %>% unite(month_year, c(water_year,month), sep="-", remove=F) 
    
    # dataframe for stats ----------------------------------------------------
    
    
    
    ###### calculate amount of time
    time_stats <- new_data %>%
      dplyr::group_by(water_year, season) %>%
      dplyr::mutate(Seasonal = sum(Q >= newx1a)/length(DateTime)*100) %>%
      distinct(water_year,  Seasonal) %>%
      mutate(position= paste(PositionName), Node = NodeName)
    
    
    time_statsx <- rbind(time_statsx, time_stats)
    
    ### count days per month
    new_datax <- new_datax %>% 
      group_by(month, day, water_year, ID = data.table::rleid(Q >= newx1a)) %>%
      mutate(threshold = if_else(Q >= newx1a,  row_number(), 0L)) %>%
      mutate(position= paste(PositionName)) 
    
    
    days_data <- rbind(days_data, new_datax)
    
    
  } ## end 2nd loop
  
  ## limits
  ## note that 0.1 upper/lower limit is max/min Q to adhere to 0.1 bound
  limits <- limits %>%
    mutate(Species ="Steelhead", Life_Stage = "Smolts", Hydraulic = "Depth", Node = NodeName)
  
  write.csv(limits, paste("output_data/F5_",NodeName,"_Steelhead_Smolts_depth_Q_limits.csv", sep=""))
  
  
  file_name = paste("figures/Application_curves/Depth/", NodeName, "_Steelhead_Smolts_depth_prob_Q_thresholds.png", sep ="")
  
  png(file_name, width = 500, height = 600)
  
  ggplot(all_data, aes(x = Q, y=value)) +
    geom_line(aes(group = variable, lty = variable)) +
    scale_linetype_manual(values= c("dotted", "solid", "dashed"))+
    #                       name="Cross\nSection\nPosition",
    #                       breaks=c("depth_cm_LOB", "depth_cm_MC", "depth_cm_ROB"),
    #                         labels = c("LOB", "MC", "ROB")) +
    
    facet_wrap(~variable, scales="free_x", nrow=3, labeller=labeller(variable = labels)) +
    # geom_point(data = subset(all_data, variable =="depth_cm_MC"), aes(y=0.1, x=limits[1,2]), color="green") +
    geom_point(data = subset(hyd_dep, variable =="depth_cm_MC"), aes(y=3, x=limits[1,2]), color="green") +
    # geom_point(data = subset(hyd_dep, variable =="depth_cm_MC"), aes(y=10, x=limits[1,2]), color="green") +
    
    geom_point(data = subset(hyd_dep, variable =="depth_cm_LOB"), aes(y=3, x=limits[1,1]), color="green") +
    # geom_point(data = subset(hyd_dep, variable =="depth_cm_LOB"), aes(y=10, x=limits[1,1]), color="green") +
    
    geom_point(data = subset(hyd_dep, variable =="depth_cm_ROB"), aes(y=3, x=limits[1,3]), color="green") +
    # geom_point(data = subset(hyd_dep, variable =="depth_cm_ROB"), aes(y=10, x=limits[1,3]), color="green") +
    
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
    labs(title = paste(NodeName, ": Smolts/Depth: Depth ~ Q", sep=""),
         y = "Probability",
         x = "Q (cfs)") #+ theme_bw(base_size = 15)
  
  dev.off()
  
  ## percentage time
  melt_time<-reshape2::melt(time_statsx, id=c("season", "position", "water_year", "Node"))
  melt_time <- melt_time %>% 
    rename( Season = variable) %>%
    mutate(Species ="Steelhead", Life_Stage = "Smolts", Hydraulic = "Depth", Node = NodeName)
  
  write.csv(melt_time, paste("output_data/F5_", NodeName, "_Steelhead_Smolts_depth_time_stats.csv", sep=""))
  
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
    mutate(Species ="Steelhead", Life_Stage = "Smolts", Hydraulic = "Depth")
  
  
  ## save df
  write.csv(melt_days, paste("output_data/F5_", NodeName, "_Steelhead_Smolts_depth_total_days_long.csv", sep="") )
  
} ## end 1st loop


# Velocity ----------------------------------------------------------------

setwd("/Users/katieirving/Documents/git/flow_eco_mech")

for(n in 1: length(h)) {
  
  NodeData <- read.csv(file=paste("input_data/HecRas/", h[n], sep=""))
  F34D <- read.csv("input_data/HecRas/hydraulic_ts_F34D.csv") ## for dates
  
  ## format hydraulic data
  
  NodeData <- NodeData %>%
    mutate(Q_ts.datetime = F34D$Q_ts.datetime)
  
  hydraul <-NodeData[,-1]
  
  ## change some names
  hydraul <- hydraul %>%
    rename(DateTime = Q_ts.datetime, node = Gage, Q = Flow)
  
  ## define node name
  NodeName <- unique(hydraul$node)
  
  ## convert units and change names
  
  hyd_vel <- hydraul %>%
    mutate(depth_cm_LOB = (Hydr..Depth..ft..LOB*0.3048)*100,
           depth_cm_MC = (Hydr..Depth..ft..MC*0.3048)*100,
           depth_cm_ROB = (Hydr..Depth..ft..ROB*0.3048)*100) %>%
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
  
  
  ## take only depth variable
  hyd_vel <- hyd_vel %>% select(DateTime, node, Q, contains("vel"), date_num)
  
  # ## melt channel position data
  hyd_vel<-reshape2::melt(hyd_vel, id=c("DateTime","Q", "node", "date_num"))
  ## change NAs to 0 in concrete overbanks
  hyd_vel[is.na(hyd_vel)] <- 0
  
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
    mutate(season = ifelse(month == 12 | month == 1 | month == 2 | month == 3 | month == 4| month == 5 | month == 6 | month == 7, 
                           paste("critical"), paste("non_critical") )) %>%    
    mutate(water_year = ifelse(month == 10 | month == 11 | month == 12, year, year-1))
  
  
  ## save out
  save(all_data, file=paste("output_data/F5_", NodeName, "_Steelhead_velocity_Smolts_discharge_probs_2010_2017_TS.RData", sep=""))
  
  # format probability time series ------------------------------------------
  
  ### define dataframes for 2nd loop
  
  ## Q Limits
  limits <- as.data.frame(matrix(ncol=3, nrow=2)) %>%
    rename(LOB = V1, MC = V2, ROB = V3) 
  rownames(limits)<-c("Lower_limit",
                      "Upper_Limit")
  
  time_statsx <- NULL
  days_data <- NULL
  
  ## define positions
  positions <- unique(all_data$variable)
  
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
    } else if(min(curve$y)>0) {
      newx1a <- min(curve$x)
    } else {
      newx1a <- approx(x = curve$y, y = curve$x, xout = 0.0001)$y
    }
    
    
    
    if(min(curve$y) ==0 && max(curve$y) ==0) {
      newx2a <- 0
    } else  if(max(curve$y) < 0.49) {
      newx2a <- max(curve$x)
    } else if(min(curve$y) > 0.49) {
      newx2a <- min(curve$x)
    } else { 
      newx2a <- approx(x = curve$y, y = curve$x, xout = 0.49)$y
    }
    
    
    
    ## MAKE DF OF Q LIMITS
    limits[,p] <- c(newx1a, newx2a)
    
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
    
    
    time_statsx <- rbind(time_statsx, time_stats)
    
    ### count days per month
    new_datax <- new_datax %>% 
      group_by(month, day, water_year, ID = data.table::rleid(Q >= newx1a & Q <= newx2a)) %>%
      mutate(threshold = if_else(Q >= newx1a & Q <= newx2a,  row_number(), 0L)) %>%
      mutate(position= paste(PositionName)) 
    
    
    days_data <- rbind(days_data, new_datax)
    
    
  } ## end 2nd loop
  
  ## limits
  ## note that 0.1 upper/lower limit is max/min Q to adhere to 0.1 bound
  limits <- limits %>%
    mutate(Species ="Steelhead", Life_Stage = "Smolts", Hydraulic = "Velocity", Node = NodeName)
  
  write.csv(limits, paste("output_data/F5_",NodeName,"_Steelhead_Smolts_velocity_Q_limits.csv", sep=""))
  
  
  file_name = paste("figures/Application_curves/Depth/", NodeName, "_Steelhead_Smolts_velocity_prob_Q_thresholds.png", sep ="")
  
  png(file_name, width = 500, height = 600)
  
  ggplot(all_data, aes(x = Q, y=value)) +
    geom_line(aes(group = variable, lty = variable)) +
    scale_linetype_manual(values= c("dotted", "solid", "dashed"))+
    #                       name="Cross\nSection\nPosition",
    #                       breaks=c("depth_cm_LOB", "depth_cm_MC", "depth_cm_ROB"),
    #                         labels = c("LOB", "MC", "ROB")) +
    
    facet_wrap(~variable, scales="free_x", nrow=3, labeller=labeller(variable = labels)) +
    
    geom_point(data = subset(hyd_vel, variable =="vel_m_MC"), aes(y=0, x=limits[1,2]), color="green") +
    geom_point(data = subset(hyd_vel, variable =="vel_m_MC"), aes(y=3.1, x=limits[2,2]), color="green") +
    
    geom_point(data = subset(hyd_vel, variable =="vel_m_LOB"), aes(y=0, x=limits[1,1]), color="green") +
    geom_point(data = subset(hyd_vel, variable =="vel_m_LOB"), aes(y=3.1, x=limits[2,1]), color="green") +
    
    geom_point(data = subset(hyd_vel, variable =="vel_m_ROB"), aes(y=0, x=limits[1,3]), color="green") +
    geom_point(data = subset(hyd_vel, variable =="vel_m_ROB"), aes(y=3.1, x=limits[2,3]), color="green") +
    
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
    labs(title = paste(NodeName, ": Smolts: Velocity ~ Q", sep=""),
         y = "Velocity (m/s)",
         x = "Q (cfs)") #+ theme_bw(base_size = 15)
  
  dev.off()
  
  ## percentage time
  melt_time<-reshape2::melt(time_statsx, id=c("season", "position", "water_year", "Node"))
  melt_time <- melt_time %>% 
    rename( Season = variable) %>%
    mutate(Species ="Steelhead", Life_Stage = "Smolts", Hydraulic = "Velocity", Node = NodeName)
  
  write.csv(melt_time, paste("output_data/F5_", NodeName, "_Steelhead_Smolts_velocity_time_stats.csv", sep=""))
  
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
    mutate(Species ="Steelhead", Life_Stage = "Smolts", Hydraulic = "Velocity")
  
  
  ## save df
  write.csv(melt_days, paste("output_data/F5_", NodeName, "_Steelhead_Smolts_velocity_total_days_long.csv", sep="") )
  
} ## end 1st loop


