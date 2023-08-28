## Depth curves - model and application
## Willow

## produces probability curves for depth, and application to sample node data (time series) for Seedling and Juvenile
## also data distributions 
# getwd()
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
load(file="output_data/functions/root_interpolation_function.Rdata")

## define root equation
load(file="output_data/functions/expression_Q_limit_function.RData")

# Combine with hydraulic data -------------------------------------------

## models
load(file="output_data/Models/depth_seedling_mod.rda")
## shear
load(file="output_data/Models/shear_seedling.rda")

## upload hydraulic data
setwd("ignore/HecRas")

h <- list.files(pattern="predictions")
length(h) ## 18
h
n=17


min_limit_df <- as.data.frame(matrix(ncol=4, nrow=length(h)))
colnames(min_limit_df) <- c("Node", "LOB", "MC", "ROB")

## set wd back to main
setwd("/Users/katieirving/Library/CloudStorage/OneDrive-SCCWRP/Documents - Katie’s MacBook Pro/git/LARiver_Sp_Model/")

n=5
for(n in 1: length(h)) {
  
  NodeData <- read.csv(file=paste("ignore/HecRas/", h[n], sep=""))
  F34D <- read.csv("ignore/HecRas/hydraulic_ts_F34D.csv") ## for dates
  NodeName <- str_split(h[n], "_", 3)[[1]]
  NodeName <- NodeName[1]
  ## format hydraulic data
  
  
  cat(paste("Running Node", NodeName))
  
  NodeData <- NodeData %>%
    mutate(DateTime = F34D$Q_ts.datetime)
  
  hydraul <-NodeData[,-1]
  # head(hydraul)
  ## change some names
  hydraul <- hydraul %>%
    rename(Q = Flow) %>%
    mutate(node = NodeName)
  
  
  ## convert units and change names - depending on concrete/soft bottom. if/else to determine changes to data
  
  if(length(hydraul) == 8) {
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
             sp_w_ROB = (Stream.Power..lb.ft.s..ROB*4.44822)/0.3048) %>%
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
    mutate(prob_fit = predict(depth_seedling_mod, newdata = hyd_dep, type="response")) %>%
    mutate(prob_fit = ifelse(prob_fit<=0, 0, prob_fit)) %>%## predicts negative percentages - cut off at 0 for quick fix
    mutate(prob_fit = ifelse(prob_fit>100, 100, prob_fit)) 
  
  ## save out
  save(all_data, file=paste("ignore/Probs/W1_", NodeName, "_Willow_Seedling_depth_discharge_probability_updated_hyd.RData", sep=""))
  
  
  # format probability time series ------------------------------------------
  
  ## look at data using lubridate etc
  
  ## format date time
  all_data$DateTime<-as.POSIXct(all_data$DateTime,
                                format = "%m/%d/%Y %H:%M",
                                tz = "GMT")
  
  ## create year, month, day and hour columns and add water year
  
  all_data <- all_data %>%
    mutate(month = month(DateTime)) %>%
    mutate(year = year(DateTime)) %>%
    mutate(day = day(DateTime)) %>%
    mutate(hour = hour(DateTime)) %>%
    mutate(water_year = ifelse(month == 10 | month == 11 | month == 12, year, year-1))
  
  save(all_data, file=paste("ignore/Probs/W1_", NodeName, "_Willow_depth_Seedling_discharge_probs_2010_2017_TS_updated_hyd.RData", sep=""))
  
  ### define dataframes for 2nd loop
  
  # define positions
  positions <- unique(all_data$variable)
  
  ## Q Limits
  limits <- as.data.frame(matrix(ncol=length(positions), nrow=12)) 
  limits$Type<-c("Q_limit")
  
  H_limits <- as.data.frame(matrix(ncol=length(positions), nrow=12)) 
  H_limits$Type<-c("Hydraulic_limit")
  
  ## calculation
  Q_Calc <- as.data.frame(matrix(ncol=3, nrow=3 ))
  names(Q_Calc) <- c("Low", "Medium", "High")
  
  min_limit_df[n,"Node"] <- NodeName

  time_statsx <- NULL
  days_data <- NULL

  # probability as a function of discharge -----------------------------------
  p=1
  for(p in 1:length(positions)) {
    
    new_data <- all_data %>% 
      filter(variable  == positions[p])
    
    ## define position
    PositionName <- str_split(positions[p], "_", 3)[[1]]
    PositionName <- PositionName[3]
    
    
    peak <- new_data %>%
      filter(prob_fit == max(prob_fit)) #%>%
    
    peakQ  <- max(peak$Q)
    min_limit <- filter(new_data, depth_cm >= 3)
    min_limit <- min(min_limit$Q)
    min_limit
    min_limit_df[n, PositionName] <- min_limit

    ## find roots for each probability
    
    ## high mortality = low prob of occurrence
    
    if(min(new_data$prob_fit)>75) {
      newx1a <- min(new_data$Q)
      hy_lim1 <- min(new_data$depth_cm)
    } else {
      newx1a <- RootLinearInterpolant(new_data$Q, new_data$prob_fit, 75)
      hy_lim1 <- RootLinearInterpolant(new_data$depth_cm, new_data$prob_fit, 75)
    }
    
    if(max(new_data$prob_fit)<75) {
      newx1a <- NA
      hy_lim1 <- NA
    } else {
      newx1a <- newx1a
      hy_lim1 <- hy_lim1
    }
    
    if(length(newx1a) > 1) {
      newx1a <- c(newx1a[length(newx1a)])
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
      newx2a <- min(new_data$Q)
      hy_lim2 <- min(new_data$depth_cm)
    } else {
      newx2a <- newx2a
      hy_lim2 <- hy_lim2
    }
    
    if(length(newx2a) > 1) {
      newx2a <- c(newx2a[length(newx2a)])
      hy_lim2<- c(hy_lim2[1], hy_lim2[length(hy_lim2)])
    } else {
      newx2a <- newx2a
      hy_lim2 <- hy_lim2
    }
    
    
    ## low mortality = high prob of occurrence
    if(min(new_data$prob_fit)>25) {
      newx3a <- min(new_data$Q)
      hy_lim3 <- min(new_data$depth_cm)
    } else {
      newx3a <- RootLinearInterpolant(new_data$Q, new_data$prob_fit, 25)
      hy_lim3 <- RootLinearInterpolant(new_data$depth_cm, new_data$prob_fit, 25)
    }
    
    if(max(new_data$prob_fit)<25) {
      newx3a <- NA
      hy_lim2 <- NA
    } else {
      newx3a <- newx3a
      hy_lim3 <- hy_lim3
    }
    
    if(length(newx3a) > 1) {
      newx3a <- c(newx3a[length(newx3a)])
      hy_lim3<- c(hy_lim3[1], hy_lim3[length(hy_lim3)])
    } else {
      newx3a <- newx3a
      hy_lim3 <- hy_lim3
    }
    
    newx1a <- sort(newx1a, decreasing = T)
    newx2a <- sort(newx2a, decreasing = T)
    newx3a <- sort(newx3a, decreasing = T)
    newx3a
    
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
    
    non_critical <- c(1:3,10:12) ## winter months
    critical <- c(4:9) ## summer months
    
    new_datax <- new_datax %>%
      mutate(season = ifelse(month %in% non_critical, "non_critical", "critical") )
    
    ## define equation for roots
    ## produces percentage of time for each year and season within year for each threshold
    
    ## Main channel curves
 

    low_thresh <- expression_Q(newx1a, peakQ) 
    low_thresh <-as.expression(do.call("substitute", list(low_thresh[[1]], list(limit = as.name("newx1a")))))
    ## change symbol if only 1 value
    if(length(newx1a)==1) {
      low_thresh <-as.expression(do.call("substitute", list(low_thresh[[1]], list(">=" = as.symbol("<=")))))
    }
    # low_thresh <-as.expression(do.call("substitute", list(low_thresh[[1]], list(">=" = as.symbol("<=")))))


    med_thresh <- expression_Q(newx2a, peakQ)
    med_thresh <-as.expression(do.call("substitute", list(med_thresh[[1]], list(limit = as.name("newx2a")))))
    ## change symbol if only 1 value
    if(length(newx2a)==1) {
      med_thresh <-as.expression(do.call("substitute", list(med_thresh[[1]], list(">=" = as.symbol("<=")))))
    }
   
    
    high_thresh <- expression_Q(newx3a, peakQ)
    high_thresh <-as.expression(do.call("substitute", list(high_thresh[[1]], list(limit = as.name("newx3a")))))
    ## change symbol if only 1 value
    if(length(newx3a)==1) {
      high_thresh <-as.expression(do.call("substitute", list(high_thresh[[1]], list(">=" = as.symbol("<=")))))
    }
    
    
    Q_Calc[p,] <- c(paste(low_thresh), paste(med_thresh), paste(high_thresh))
    
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
  
  Q_Calc$Position <- positions
  
  Q_Calc <- Q_Calc %>%
    mutate(Species ="Willow", Life_Stage = "Seedling", Hydraulic = "Depth", Node = NodeName)
  
  write.csv(Q_Calc, paste("ignore/Probs/W1_",NodeName,"_Willow_Seedling_depth_Q_calculation_updated_hyd.csv", sep=""))
  
  ## limits
  limits <- rbind(limits, H_limits)
  
  limits <- limits %>%
    mutate(Species ="Willow", Life_Stage = "Seedling", Hydraulic = "Depth", Node = NodeName)

  write.csv(limits, paste("ignore/Probs/W1_",NodeName,"_Willow_Seedling_depth_Q_limits_updated_hyd.csv", sep=""))
  
  
  ## percentage time
  melt_time<-reshape2::melt(time_statsx, id=c("season", "position", "water_year", "Node"))
  melt_time <- melt_time %>% 
    rename( Probability_Threshold = variable) %>%
    mutate(Species ="Willow", Life_Stage = "Seedling", Hydraulic = "Depth", Node = NodeName)
  
  write.csv(melt_time, paste("ignore/Probs/W1_", NodeName, "_Willow_Seedling_depth_time_stats_updated_hyd.csv", sep=""))
  
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
  non_critical <- c(1:3,10:12) ## winter months
  critical <- c(4:9) ## summer months
  
  total_days <- total_days %>%
    mutate(season = ifelse(month %in% non_critical, "non_critical", "critical") )
  
  # ## melt data
  
  melt_days<-reshape2::melt(total_days, id=c("month_year", "water_year", "month", "season", "position", "Node"))
  melt_days <- melt_days %>%
    rename(Probability_Threshold = variable, n_days = value) %>%
    mutate(Species ="Willow", Life_Stage = "Seedling", Hydraulic = "Depth")
  
  
  ## save df
  write.csv(melt_days, paste("ignore/Probs/W1_", NodeName, "_Willow_Seedling_depth_total_days_long_updated_hyd.csv", sep="") )
  
} ## end 1st loop

write.csv(min_limit_df, "ignore/Probs/min_flow_limit_node_position.csv")

# Shear ----------------------------------------------------------------


for(n in 1: length(h)) {
  
  NodeData <- read.csv(file=paste("ignore/HecRas/", h[n], sep=""))
  F34D <- read.csv("ignore/HecRas/hydraulic_ts_F34D.csv") ## for dates
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
  
  if(length(hydraul) == 8) {
    hyd_shear <- hydraul %>%
      mutate(depth_cm_MC = (Max..Depth..ft..MC*0.3048)*100) %>%
      mutate(shear_pa_MC = (Shear..lb.sq.ft..MC/0.020885)) %>%
      mutate(sp_w_MC = (Stream.Power..lb.ft.s..MC*4.44822)/0.3048) %>%
      mutate(vel_m_MC = (Avg..Vel...ft.s..MC*0.3048)) %>%
      select(-contains("ft")) %>%
      mutate(date_num = seq(1,length(DateTime), 1))
  } else {
    hyd_shear <- hydraul %>%
      mutate(depth_cm_LOB = (Max..Depth..ft..LOB*0.3048)*100,
             depth_cm_MC = (Max..Depth..ft..MC*0.3048)*100,
             depth_cm_ROB = (Max..Depth..ft..ROB*0.3048)*100) %>%
      mutate(shear_pa_LOB = (Shear..lb.sq.ft..LOB/0.020885),
             shear_pa_MC = (Shear..lb.sq.ft..MC/0.020885),
             shear_pa_ROB = (Shear..lb.sq.ft..ROB/0.020885)) %>%
      mutate(sp_w_LOB = (Stream.Power..lb.ft.s..LOB*4.44822)/0.3048,
             sp_w_MC = (Stream.Power..lb.ft.s..MC*4.44822)/0.3048,
             sp_w_ROB = (Stream.Power..lb.ft.s..ROB*4.44822)/0.3048) %>%
      mutate(vel_m_LOB = (Avg..Vel...ft.s..LOB*0.3048),
             vel_m_MC = (Avg..Vel...ft.s..MC*0.3048),
             vel_m_ROB = (Avg..Vel...ft.s..ROB*0.3048)) %>%
      select(-contains("ft")) %>%
      mutate(date_num = seq(1,length(DateTime), 1))
    
  }
  
  
  
  

  
  # take only depth variable for min limit
  hyd_dep <- hyd_shear %>% select(DateTime, node, Q, contains("depth"), date_num)

  
  ## take only shear & depth (for min_limit) variable
  hyd_shear <- hyd_shear %>% select(DateTime, node, Q, contains( "shear"), date_num)
  head(hyd_shear)
  
  # ## melt channel position data
  hyd_shear<-reshape2::melt(hyd_shear, id=c("DateTime","Q", "node", "date_num"))

  
  hyd_shear <- hyd_shear %>%
    # filter(variable %in% c("shear_pa_LOB", "shear_pa_MC", "shear_pa_ROB")) %>%
    rename(shear = value)

  
  all_data <- hyd_shear %>%
    mutate(prob_fit = predict(shear_seedling, newdata = hyd_shear, type="response")) %>%
    mutate(prob_fit = ifelse(prob_fit<=0, 0, prob_fit)) %>% ## predicts negative percentages - cut off at 0 for quick fix
    mutate(prob_fit = ifelse(prob_fit>100, 100, prob_fit)) ## predicts negative percentages - cut off at 0 for quick fix
  
  ## save out
  save(all_data, file=paste("ignore/Probs/W1_", NodeName, "_Willow_Seedling_Shear_discharge_probability_updated_hyd.RData", sep=""))
  
  
  # format probability time series ------------------------------------------
  
  ## look at data using lubridate etc
  
  ## format date time
  all_data$DateTime<-as.POSIXct(all_data$DateTime,
                                format = "%m/%d/%Y %H:%M",
                                tz = "GMT")
  
  ## create year, month, day and hour columns and add water year
  
  all_data <- all_data %>%
    mutate(month = month(DateTime)) %>%
    mutate(year = year(DateTime)) %>%
    mutate(day = day(DateTime)) %>%
    mutate(hour = hour(DateTime)) %>%
    mutate(water_year = ifelse(month == 10 | month == 11 | month == 12, year, year-1))
  
  save(all_data, file=paste("ignore/Probs/W1_", NodeName, "_Willow_Shear_Seedling_discharge_probs_2010_2017_TS_updated_hyd.RData", sep=""))
  
  ### define dataframes for 2nd loop
  
  ## define positions
  positions <- unique(all_data$variable)
  ## Q Limits
  limits <- as.data.frame(matrix(ncol=length(positions), nrow=12)) 
  limits$Type<-c("Q_limit")
  
  H_limits <- as.data.frame(matrix(ncol=length(positions), nrow=12)) 
  H_limits$Type<-c("Hydraulic_limit")
  
  ## calculation
  Q_Calc <- as.data.frame(matrix(ncol=3, nrow=3 ))
  names(Q_Calc) <- c("Low", "Medium", "High")
  
  time_statsx <- NULL
  days_data <- NULL
  # positions
p=1
  # head(new_data)
  # probability as a function of discharge -----------------------------------
  
  for(p in 1:length(positions)) {
    
    new_data <- all_data %>% 
      filter(variable  == positions[p])
    PositionName
    ## define position
    PositionName <- str_split(positions[p], "_", 3)[[1]]
    PositionName <- PositionName[3]

    new_dataD <- hyd_dep %>% 
      select(DateTime, node, Q, contains(PositionName)) 
    
    colnames(new_dataD)[4] <- "depth_cm"
      
    # plot(new_dataD$depth_cm, new_data$shear)

    
    peak <- new_data %>%
      filter(prob_fit == max(prob_fit)) #%>%
    # head(new_data)
    peakQ  <- max(peak$Q)
    min_limit <- filter(new_dataD, depth_cm >=3)
    # min_limit2 <- filter(new_data, shear > 0)
    min_limit <- min(min_limit$Q)
   
    # min_limit <- filter(new_data, shear >0.0)
    # test <- subset(new_dataD, depth_cm >= 5)
    ## find roots for each probability
    
    ## high mortality = low prob of occurrence
    
    if(min(new_data$prob_fit)>75) {
      newx1a <- min(new_data$Q)
      hy_lim1 <- min(new_data$shear)
    } else {
      newx1a <- RootLinearInterpolant(new_data$Q, new_data$prob_fit, 75)
      hy_lim1 <- RootLinearInterpolant(new_data$shear, new_data$prob_fit, 75)
    }
    
    if(max(new_data$prob_fit)<75) {
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
      newx2a <- max(new_data$Q)
      hy_lim2 <- max(new_data$shear)
    } else {
      newx2a <- RootLinearInterpolant(new_data$Q, new_data$prob_fit, 50)
      hy_lim2 <- RootLinearInterpolant(new_data$shear, new_data$prob_fit, 50)
    }
    
    if(min(new_data$prob_fit)>50) {
      newx2a <- min(new_data$Q)
      hy_lim2 <- min(new_data$shear)
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
    
 
    ## low mortality = high prob of occurrence
    if(min(new_data$prob_fit)>25) {
      newx3a <- min(new_data$Q)
      hy_lim3 <- min(new_data$shear)
    } else {
      newx3a <- RootLinearInterpolant(new_data$Q, new_data$prob_fit, 25)
      hy_lim3 <- RootLinearInterpolant(new_data$shear, new_data$prob_fit, 25)
    }
    
    if(max(new_data$prob_fit)<25) {
      newx3a <-NA
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
    
    newx1a <- sort(newx1a, decreasing = T)
    newx2a <- sort(newx2a, decreasing = T)
    newx3a <- sort(newx3a, decreasing = T)
    newx3a
  
    hy_lim2
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
    
    non_critical <- c(1:3,10:12) ## winter months
    critical <- c(4:9) ## summer months
    
    new_datax <- new_datax %>%
      mutate(season = ifelse(month %in% non_critical, "non_critical", "critical") )
    
    ## define equation for roots
    ## produces percentage of time for each year and season within year for each threshold
    
    ## Main channel curves
   
    
    low_thresh <- expression_Q(newx1a, peakQ) 
    low_thresh <-as.expression(do.call("substitute", list(low_thresh[[1]], list(limit = as.name("newx1a")))))
    ## change symbol if only 1 value
    if(length(newx1a)==1) {
      low_thresh <-as.expression(do.call("substitute", list(low_thresh[[1]], list(">=" = as.symbol("<=")))))
    }
    # low_thresh <-as.expression(do.call("substitute", list(low_thresh[[1]], list(">=" = as.symbol("<=")))))
    
    
    med_thresh <- expression_Q(newx2a, peakQ)
    med_thresh <-as.expression(do.call("substitute", list(med_thresh[[1]], list(limit = as.name("newx2a")))))
    ## change symbol if only 1 value
    if(length(newx2a)==1) {
      med_thresh <-as.expression(do.call("substitute", list(med_thresh[[1]], list(">=" = as.symbol("<=")))))
    }
    
    
    high_thresh <- expression_Q(newx3a, peakQ)
    high_thresh <-as.expression(do.call("substitute", list(high_thresh[[1]], list(limit = as.name("newx3a")))))
    ## change symbol if only 1 value
    if(length(newx3a)==1) {
      high_thresh <-as.expression(do.call("substitute", list(high_thresh[[1]], list(">=" = as.symbol("<=")))))
    }
    
    Q_Calc[p,] <- c(paste(low_thresh), paste(med_thresh), paste(high_thresh))
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
  
  Q_Calc$Position <- positions
  
  Q_Calc <- Q_Calc %>%
    mutate(Species ="Willow", Life_Stage = "Seedling", Hydraulic = "Shear", Node = NodeName)
  
  write.csv(Q_Calc, paste("ignore/Probs/W1_",NodeName,"_Willow_Seedling_Shear_Q_calculation_updated_hyd.csv", sep=""))
  
  ## limits
  limits <- rbind(limits, H_limits)

  limits <- limits %>%
    mutate(Species ="Willow", Life_Stage = "Seedling", Hydraulic = "Shear", Node = NodeName)
  write.csv(limits, paste("ignore/Probs/W1_",NodeName,"_Willow_Seedling_Shear_Q_limits_updated_hyd.csv", sep=""))
  
  
  ## percentage time
  melt_time<-reshape2::melt(time_statsx, id=c("season", "position", "water_year", "Node"))
  melt_time <- melt_time %>% 
    rename( Probability_Threshold = variable) %>%
    mutate(Species ="Willow", Life_Stage = "Seedling", Hydraulic = "Shear", Node = NodeName)
  
  write.csv(melt_time, paste("ignore/Probs/W1_", NodeName, "_Willow_Seedling_Shear_time_stats_updated_hyd.csv", sep=""))
  
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
  non_critical <- c(1:3, 10:12) 
  critical <- c(4:9) 
  
  total_days <- total_days %>%
    mutate(season = ifelse(month %in% non_critical, "non_critical", "critical") )
  
  # ## melt data
  
  melt_days<-reshape2::melt(total_days, id=c("month_year", "water_year", "month", "season", "position", "Node"))
  melt_days <- melt_days %>%
    rename(Probability_Threshold = variable, n_days = value) %>%
    mutate(Species ="Willow", Life_Stage = "Seedling", Hydraulic = "Shear")
  
  
  ## save df
  write.csv(melt_days, paste("ignore/Probs/W1_", NodeName, "_Willow_Seedling_Shear_total_days_long_updated_hyd.csv", sep="") )
  
} ## end 1st loop
