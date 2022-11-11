## Depth curves - model and application
## Cladophora

## produces probability curves for depth, and application to sample node data (time series) for adult and Juvenile
## also data distributions 

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
load(file="root_interpolation_function.Rdata")

## define root equation
load(file="expression_Q_limit_function.RData")

# Combine with hydraulic data -------------------------------------------

## depth model
load(file="clad_depth_mod.RData")
clad_depth_mod <- depth_lmq

## upload hydraulic data
setwd("input_data/HecRas")

h <- list.files(pattern="predictions")
length(h) ## 20

## set wd back to main
setwd("/Users/katieirving/Documents/git/flow_eco_mech")
n=1
for(n in 1: length(h)) {
  
  NodeData <- read.csv(file=paste("input_data/HecRas/", h[n], sep=""))
  F34D <- read.csv("input_data/HecRas/hydraulic_ts_F34D.csv") ## for dates
  
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
  
  names(hydraul)
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
  hyd_dep <- hyd_dep %>% rename(depth_cm = value)

  ## change NAs to 0 in concrete overbanks
  hyd_dep[is.na(hyd_dep)] <- 0
  
  all_data <- hyd_dep %>%
    mutate(prob_fit = predict(clad_depth_mod, newdata = hyd_dep, type="response")) %>%
    mutate(prob_fit = ifelse(prob_fit<=0, 0, prob_fit)) ## predicts negative percentages - cut off at 0 for quick fix
  
  
  
  
  ## save out
  save(all_data, file=paste("output_data/C1_", NodeName, "_Cladophora_adult_depth_discharge_probability_updated_hyd.RData", sep=""))
  
  
  # format probability time series ------------------------------------------
  
  ## look at data using lubridate etc
  
  ## format date time
  all_data$DateTime<-as.POSIXct(all_data$DateTime,
                                format = "%Y-%m-%d %H:%M",
                                tz = "GMT")
  
  ## create year, month, day and hour columns and add water year
  
  all_data <- all_data %>%
    mutate(month = month(DateTime)) %>%
    mutate(year = year(DateTime)) %>%
    mutate(day = day(DateTime)) %>%
    mutate(hour = hour(DateTime)) %>%
    mutate(water_year = ifelse(month == 10 | month == 11 | month == 12, year, year-1))
  
  save(all_data, file=paste("output_data/C1_", NodeName, "_Cladophora_depth_adult_discharge_probs_2010_2017_TS_updated_hyd.RData", sep=""))
  
  ### define dataframes for 2nd loop
  
  # define positions
  positions <- unique(all_data$variable)
  
  ## Q Limits
  limits <- as.data.frame(matrix(ncol=length(positions), nrow=12)) 
  limits$Type<-c("Q_limit")
  
  H_limits <- as.data.frame(matrix(ncol=length(positions), nrow=12)) 
  H_limits$Type<-c("Hydraulic_limit")
  
  
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
    
    peak <- new_data %>%
      filter(prob_fit == max(prob_fit)) #%>%
    
    peakQ  <- max(peak$Q)
    min_limit <- filter(new_data, depth_cm >= 0.03)
    min_limit <- min(min_limit$Q)

    ## Main channel curves
    
    ## find roots for each probability
    
  
    if(min(new_data$prob_fit)>25) {
      newx1a <- min(new_data$Q)
      hy_lim1 <- min(new_data$depth_cm)
    } else {
      newx1a <- RootLinearInterpolant(new_data$Q, new_data$prob_fit, 25)
      hy_lim1 <- RootLinearInterpolant(new_data$depth_cm, new_data$prob_fit, 25)
    }
    
    newx2a  <- RootLinearInterpolant(new_data$Q, new_data$prob_fit, 50)
    hy_lim2 <- RootLinearInterpolant(new_data$depth_cm, new_data$prob_fit, 50)
    
    if(length(newx2a) > 4) {
      newx2a <- c(newx2a[1], newx2a[length(newx2a)])
      hy_lim2<- c(hy_lim2[1], hy_lim2[length(hy_lim2)])
    } else {
      newx2a <- newx2a
      hy_lim2 <- hy_lim2
    }
    
    newx3a <- RootLinearInterpolant(new_data$Q, new_data$prob_fit, 75)
    hy_lim3 <- RootLinearInterpolant(new_data$depth_cm, new_data$prob_fit, 75)
    
    if(min(new_data$prob_fit)>75) {
      newx3a <- min(new_data$Q)
      hy_lim3 <- min(new_data$depth_cm)
    } else {
      newx3a <- newx3a
      hy_lim3 <- hy_lim3
    }
    
    if(length(newx3a) > 4) {
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
    non_critical <- c(1,2,8:12) 
    critical <- c(3:7) 
    
    new_datax <- new_datax %>%
      mutate(season = ifelse(month %in% non_critical, "non_critical", "critical") )
    
    ## define equation for roots
    ## produces percentage of time for each year and season within year for each threshold
    
    ## Main channel curves
    
    
    low_thresh <- expression_Q(newx1a, peakQ) 
    low_thresh <-as.expression(do.call("substitute", list(low_thresh[[1]], list(limit = as.name("newx1a")))))
    
    med_thresh <- expression_Q(newx2a, peakQ)
    med_thresh <-as.expression(do.call("substitute", list(med_thresh[[1]], list(limit = as.name("newx2a")))))
    
    high_thresh <- expression_Q(newx3a, peakQ)
    high_thresh <-as.expression(do.call("substitute", list(high_thresh[[1]], list(limit = as.name("newx3a")))))
    

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
      group_by(month, day, water_year, ID01 = data.table::rleid(eval(low_thresh))) %>%
      mutate(Low = if_else(eval(low_thresh), row_number(), 0L)) %>%
      ungroup() %>%
      group_by(month, day, water_year, ID02 = data.table::rleid(eval(med_thresh))) %>%
      mutate(Medium = if_else(eval(med_thresh), row_number(), 0L)) %>%
      ungroup() %>%
      group_by(month, day, water_year, ID03 = data.table::rleid(eval(high_thresh))) %>%
      mutate(High = if_else(eval(high_thresh), row_number(), 0L)) %>%
      mutate(position= paste(PositionName)) #%>%
    # select(Q, month, water_year, day, ID01, Low, ID02, Medium, ID03, High, position, DateTime, node) 
    
    days_data <- rbind(days_data, new_datax)
    
  } ## end 2nd loop
  
  ## limits
  limits <- rbind(limits, H_limits)
  ## note that 0.1 upper/lower limit is max/min Q to adhere to 0.1 bound
  limits <- limits %>%
    mutate(Species ="Cladophora", Life_Stage = "Adult", Hydraulic = "Depth", Node = NodeName)
  
  write.csv(limits, paste("output_data/C1_",NodeName,"_Cladophora_adult_depth_Q_limits_updated_hyd.csv", sep=""))
  

  ## percentage time
  melt_time<-reshape2::melt(time_statsx, id=c("season", "position", "water_year", "Node"))
  melt_time <- melt_time %>% 
    rename( Probability_Threshold = variable) %>%
    mutate(Species ="Cladophora", Life_Stage = "Adult", Hydraulic = "Depth", Node = NodeName)
  
  write.csv(melt_time, paste("output_data/C1_", NodeName, "_Cladophora_adult_depth_time_stats_updated_hyd.csv", sep=""))
  
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
  non_critical <- c(1,2,8:12) 
  critical <- c(3:7) 
  
  total_days <- total_days %>%
    mutate(season = ifelse(month %in% non_critical, "non_critical", "critical") )
  
  # ## melt data
  
  melt_days<-reshape2::melt(total_days, id=c("month_year", "water_year", "month", "season", "position", "Node"))
  melt_days <- melt_days %>%
    rename(Probability_Threshold = variable, n_days = value) %>%
    mutate(Species ="Cladophora", Life_Stage = "Adult", Hydraulic = "Depth")
  
  
  ## save df
  write.csv(melt_days, paste("output_data/C1_", NodeName, "_Cladophora_adult_depth_total_days_long_updated_hyd.csv", sep="") )
  
} ## end 1st loop




# Velocity ----------------------------------------------------------------


## upload habitat curve data
load(file="clad_vel_mod.RData")

## upload hydraulic data
setwd("input_data/HecRas")

h <- list.files(pattern="predictions")
length(h) ## 18

## set wd back to main
setwd("/Users/katieirving/Documents/git/flow_eco_mech")
n=1
n
for(n in 1: length(h)) {
  
  NodeData <- read.csv(file=paste("input_data/HecRas/", h[n], sep=""))
  F34D <- read.csv("input_data/HecRas/hydraulic_ts_F34D.csv") ## for dates
  
  NodeName <- str_split(h[n], "_", 3)[[1]]
  NodeName <- NodeName[1]
  ## format hydraulic data
  cat(paste("Running Node", NodeName))
  
  NodeData <- NodeData %>%
    mutate(DateTime = F34D$Q_ts.datetime)
  
  hydraul <-NodeData[,-1]
  names(hydraul)
  
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
  
  
  ## take only depth variable for min limit
  hyd_dep <- hyd_vel %>% select(DateTime, node, Q, contains("depth"), date_num)
  
  
  hyd_dep<-reshape2::melt(hyd_dep, id=c("DateTime","Q", "node", "date_num"))
  hyd_dep <- hyd_dep %>%
    mutate(depth_cm = value) %>%
    select(date_num, depth_cm)
  
  ## take only depth variable
  hyd_vel <- hyd_vel %>% select(DateTime, node, Q, contains("vel"), date_num)
  
  # ## melt channel position data
  hyd_vel<-reshape2::melt(hyd_vel, id=c("DateTime","Q", "node", "date_num"))
  hyd_vel <- rename(hyd_vel, Velocity = value)
  ## change NAs to 0 in concrete overbanks
  hyd_vel[is.na(hyd_vel)] <- 0
  
  ## join depth data to vel df
  hyd_vel <- left_join(hyd_vel, hyd_dep, by="date_num")
  
  ## change NAs to 0 in concrete overbanks
  hyd_vel[is.na(hyd_vel)] <- 0
  
  all_data <- hyd_vel %>%
    mutate(prob_fit = predict(clad_vel_mod, newdata = hyd_vel, type="response")) %>%
    mutate(prob_fit = ifelse(prob_fit<=0, 0, prob_fit)) ## predicts negative percentages - cut off at 0 for quick fix
  
  ## save out
  save(all_data, file=paste("output_data/C1_", NodeName, "_Cladophora_adult_velocity_discharge_probability_updated_hyd.RData", sep=""))
  
  
  # format probability time series ------------------------------------------
  
  ## look at data using lubridate etc
  
  ## format date time
  all_data$DateTime<-as.POSIXct(all_data$DateTime,
                                format = "%Y-%m-%d %H:%M",
                                tz = "GMT")
  
  ## create year, month, day and hour columns and add water year
  
  all_data <- all_data %>%
    mutate(month = month(DateTime)) %>%
    mutate(year = year(DateTime)) %>%
    mutate(day = day(DateTime)) %>%
    mutate(hour = hour(DateTime)) %>%
    mutate(water_year = ifelse(month == 10 | month == 11 | month == 12, year, year-1))
  
  save(all_data, file=paste("output_data/C1_", NodeName, "_Cladophora_velocity_adult_discharge_probs_2010_2017_TS_updated_hyd.RData", sep=""))
  
  ### define dataframes for 2nd loop
  
  ## define positions
  positions <- unique(all_data$variable)
  ## Q Limits
  limits <- as.data.frame(matrix(ncol=length(positions), nrow=12)) 
  limits$Type<-c("Q_limit")
  
  H_limits <- as.data.frame(matrix(ncol=length(positions), nrow=12)) 
  H_limits$Type<-c("Hydraulic_limit")
  
  time_statsx <- NULL
  days_data <- NULL

  
  # probability as a function of discharge -----------------------------------
  
  for(p in 1:length(positions)) {
    
    new_data <- all_data %>% 
      filter(variable  == positions[p])
    
    ## define position
    PositionName <- str_split(positions[p], "_", 3)[[1]]
    PositionName <- PositionName[3]

    
    peak <- new_data %>%
      filter(prob_fit == max(prob_fit)) #%>%
    
    peakQ  <- max(peak$Q)
    min_limit <- filter(new_data, depth_cm >0.03)
    min_limit <- min(min_limit$Q)
  
    ## Main channel curves
    
    ## find roots for each probability
    newx1a <- RootLinearInterpolant(new_data$Q, new_data$prob_fit, 0.25)
    hy_lim1 <- RootLinearInterpolant(new_data$Velocity, new_data$prob_fit, 0.25)
    
    
    if(length(newx1a) > 4) {
      newx1a <- c(newx1a[1], newx1a[length(newx1a)])
      hy_lim1 <- c(hy_lim1[1], hy_lim1[length(hy_lim1)])
    } else {
      newx1a <- newx1a
      hy_lim1 <- hy_lim1
    }
    
    newx2a  <- RootLinearInterpolant(new_data$Q, new_data$prob_fit, 0.50)
    hy_lim2 <- RootLinearInterpolant(new_data$Velocity, new_data$prob_fit, 0.5)

    if(length(newx2a) > 4) {
      newx2a <- c(newx2a[1], newx2a[length(newx2a)])
      hy_lim2 <- c(hy_lim2[1], hy_lim2[length(hy_lim2)])
    } else {
      newx2a <- newx2a
      hy_lim2 <- hy_lim2
    }

    if(min(new_data$prob_fit)>0.75) {
      newx3a <- min(new_data$Q)
      hy_lim3 <- min(new_data$Velocity)
    } else {
      newx3a <- RootLinearInterpolant(new_data$Q, new_data$prob_fit, 0.75)
      hy_lim3 <- RootLinearInterpolant(new_data$Velocity, new_data$prob_fit, 0.75)
    }

    if(length(newx3a) > 4) {
      newx3a <- c(newx3a[1], newx3a[length(newx3a)])
      hy_lim3 <- c(hy_lim3[1], hy_lim3[length(hy_lim3)])
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
    non_critical <- c(1,2,8:12) 
    critical <- c(3:7) 
    
    new_datax <- new_datax %>%
      mutate(season = ifelse(month %in% non_critical, "non_critical", "critical") )
    
    ## define equation for roots
    ## produces percentage of time for each year and season within year for each threshold
    
    ## Main channel curves
    
    
    low_thresh <- expression_Q(newx1a, peakQ) 
    low_thresh <-as.expression(do.call("substitute", list(low_thresh[[1]], list(limit = as.name("newx1a")))))
    
    med_thresh <- expression_Q(newx2a, peakQ)
    med_thresh <-as.expression(do.call("substitute", list(med_thresh[[1]], list(limit = as.name("newx2a")))))
    
    high_thresh <- expression_Q(newx3a, peakQ)
    high_thresh <-as.expression(do.call("substitute", list(high_thresh[[1]], list(limit = as.name("newx3a")))))
    

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
      group_by(month, day, water_year, ID01 = data.table::rleid(eval(low_thresh))) %>%
      mutate(Low = if_else(eval(low_thresh), row_number(), 0L)) %>%
      ungroup() %>%
      group_by(month, day, water_year, ID02 = data.table::rleid(eval(med_thresh))) %>%
      mutate(Medium = if_else(eval(med_thresh), row_number(), 0L)) %>%
      ungroup() %>%
      group_by(month, day, water_year, ID03 = data.table::rleid(eval(high_thresh))) %>%
      mutate(High = if_else(eval(high_thresh), row_number(), 0L)) %>%
      mutate(position= paste(PositionName)) #%>%
    # select(Q, month, water_year, day, ID01, Low, ID02, Medium, ID03, High, position, DateTime, node) 
    
    days_data <- rbind(days_data, new_datax)
    
  } ## end 2nd loop
  
  ## limits
  limits <- rbind(limits, H_limits)
  
  limits <- limits %>%
    mutate(Species ="Cladophora", Life_Stage = "Adult", Hydraulic = "Velocity", Node = NodeName)
  
  write.csv(limits, paste("output_data/C1_",NodeName,"_Cladophora_adult_velocity_Q_limits_updated_hyd.csv", sep=""))

  
  
  ## percentage time
  melt_time<-reshape2::melt(time_statsx, id=c("season", "position", "water_year", "Node"))
  melt_time <- melt_time %>% 
    rename( Probability_Threshold = variable) %>%
    mutate(Species ="Cladophora", Life_Stage = "Adult", Hydraulic = "Velocity", Node = NodeName)
  
  write.csv(melt_time, paste("output_data/C1_", NodeName, "_Cladophora_adult_velocity_time_stats_updated_hyd.csv", sep=""))
  
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
  non_critical <- c(1,2,8:12) 
  critical <- c(3:7) 
  
  total_days <- total_days %>%
    mutate(season = ifelse(month %in% non_critical, "non_critical", "critical") )
  
  # ## melt data
  
  melt_days<-reshape2::melt(total_days, id=c("month_year", "water_year", "month", "season", "position", "Node"))
  melt_days <- melt_days %>%
    rename(Probability_Threshold = variable, n_days = value) %>%
    mutate(Species ="Cladophora", Life_Stage = "Adult", Hydraulic = "Velocity")
  
  
  ## save df
  write.csv(melt_days, paste("output_data/C1_", NodeName, "_Cladophora_adult_velocity_total_days_long_updated_hyd.csv", sep="") )
  
} ## end 1st loop


