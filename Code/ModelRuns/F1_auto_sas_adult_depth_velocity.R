## Depth curves - model and application
## adult 

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

## upload habitat curve data
fitdata <- read.csv("output_data/old_data/adult_depth_prob_curve_data.csv")

## upload hydraulic data
setwd("input_data/HecRas")

h <- list.files(pattern="hydraulic")
length(h) ## 20

## set wd back to main
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
labels <- c(depth_cm_LOB = "Left Over Bank", depth_cm_MC = "Main Channel", depth_cm_ROB = "Right Over Bank")

### node figure for depth ~ Q
file_name <- paste("figures/Application_curves/nodes/", NodeName, "_Depth_Q.png", sep="")
png(file_name, width = 500, height = 600)

ggplot(hyd_dep, aes(x = Q, y=value)) +
  geom_line(aes( group = variable, lty = variable)) +
  scale_linetype_manual(values= c("dotted", "solid", "dashed"),
                        breaks=c("depth_cm_LOB", "depth_cm_MC", "depth_cm_ROB"))+
  facet_wrap(~variable, scales="free_x", nrow=3, labeller=labeller(variable = labels)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
  labs(title = paste(NodeName, ": Depth ~ Q"),
       y = "Depth (cm)",
       x = "Q (cfs)") #+ theme_bw(base_size = 15)

dev.off()

## change NAs to 0 in concrete overbanks
hyd_dep[is.na(hyd_dep)] <- 0

  ## use smooth spline to predict on new data set
new_values <-smooth.spline(fitdata$depth_fit, fitdata$prob_fit)

all_data <- hyd_dep %>%
    group_by(variable) %>%
    mutate(prob_fit = predict(new_values, value)$y) %>%
    rename(depth_cm = value)



## save out
save(all_data, file=paste("output_data/F1_", NodeName, "_SAS_adult_depth_discharge_probability.RData", sep=""))


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

save(all_data, file=paste("output_data/F1_", NodeName, "_SAS_depth_adult_discharge_probs_2010_2017_TS.RData", sep=""))

### define dataframes for 2nd loop

## Q Limits
limits <- as.data.frame(matrix(ncol=3, nrow=12)) %>%
  rename(LOB = V1, MC = V2, ROB = V3) 
rownames(limits)<-c("Low_Prob_1", "Low_Prob_2", "Low_Prob_3", "Low_Prob_4",
                    "Med_Prob_1", "Med_Prob_2", "Med_Prob_3", "Med_Prob_4",
                    "High_Prob_1", "High_Prob_2", "High_Prob_3", "High_Prob_4")

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

## bind shallow and deeper depths by 0.1 - 10cm & 120cm
## change all prob_fit lower than 0.1 to 0.1
new_data[which(new_data$prob_fit <  0.1),"prob_fit"] <- 0.1


peak <- new_data %>%
  filter(prob_fit == max(prob_fit)) #%>%

peakQ  <- max(peak$Q)
min_limit <- filter(new_data, depth_cm >= 3)
min_limit <- min(min_limit$Q)


## Main channel curves

## find roots for each probability
newx1a <- RootLinearInterpolant(new_data$Q, new_data$prob_fit, 0.1)
newx1a <- c(min(new_data$Q), max(new_data$Q))

newx2a  <- RootLinearInterpolant(new_data$Q, new_data$prob_fit, 0.2)


if(length(newx2a) > 4) {
  newx2a <- c(newx2a[1], newx2a[length(newx2a)])
} else {
  newx2a <- newx2a
}

newx3a <- RootLinearInterpolant(new_data$Q, new_data$prob_fit, 0.3)

if(min(new_data$prob_fit)>0.3) {
  newx3a <- min(new_data$Q)
} else {
  newx3a <- newx3a
}

if(length(newx3a) > 4) {
  newx3a <- c(newx3a[1], newx3a[length(newx3a)])
} else {
  newx3a <- newx3a
}


## MAKE DF OF Q LIMITS
limits[,p] <- c(newx1a[1], newx1a[2],newx1a[3], newx1a[4],
               newx2a[1], newx2a[2],newx2a[3], newx2a[4], 
               newx3a[1], newx3a[2],newx3a[3],newx3a[4])

# create year_month column       
new_datax <- new_data %>% unite(month_year, c(water_year,month), sep="-", remove=F) 

# dataframe for stats -----------------------------------------------------

## define critical period or season for adult as all year is critical
winter <- c(1,2,3,4,11,12) ## winter months
summer <- c(5:10) ## summer months

new_datax <- new_datax %>%
  mutate(season = ifelse(month %in% winter, "winter", "summer") )

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
## note that 0.1 upper/lower limit is max/min Q to adhere to 0.1 bound
limits <- limits %>%
  mutate(Species ="SAS", Life_Stage = "Adult", Hydraulic = "Depth", Node = NodeName)

write.csv(limits, paste("output_data/F1_",NodeName,"_SAS_adult_depth_Q_limits.csv", sep=""))

all_data[which(all_data$prob_fit <  0.1),"prob_fit"] <- 0.1

file_name = paste("figures/Application_curves/Depth/", NodeName, "_SAS_adult_depth_prob_Q_thresholds.png", sep ="")

png(file_name, width = 500, height = 600)

ggplot(all_data, aes(x = Q, y=prob_fit)) +
  geom_line(aes(group = variable, lty = variable)) +
  scale_linetype_manual(values= c("dotted", "solid", "dashed"))+
  #                       name="Cross\nSection\nPosition",
  #                       breaks=c("depth_cm_LOB", "depth_cm_MC", "depth_cm_ROB"),
  #                         labels = c("LOB", "MC", "ROB")) +
  
  facet_wrap(~variable, scales="free_x", nrow=3, labeller=labeller(variable = labels)) +
  geom_point(data = subset(all_data, variable =="depth_cm_MC"), aes(y=0.1, x=limits[1,2]), color="green") +
  geom_point(data = subset(all_data, variable =="depth_cm_MC"), aes(y=0.1, x=limits[2,2]), color="green") +
  geom_point(data = subset(all_data, variable =="depth_cm_MC"), aes(y=0.1, x=limits[3,2]), color="green") +
  geom_point(data = subset(all_data, variable =="depth_cm_MC"), aes(y=0.1, x=limits[4,2]), color="green") +
  geom_point(data = subset(all_data, variable =="depth_cm_MC"), aes(y=0.2, x=limits[5,2]), color="red") +
  geom_point(data = subset(all_data, variable =="depth_cm_MC"), aes(y=0.2, x=limits[6,2]), color="red") +
  geom_point(data = subset(all_data, variable =="depth_cm_MC"), aes(y=0.2, x=limits[7,2]), color="red") +
  geom_point(data = subset(all_data, variable =="depth_cm_MC"), aes(y=0.2, x=limits[8,2]), color="red") +
  geom_point(data = subset(all_data, variable =="depth_cm_MC"), aes(y=0.3, x=limits[9,2]), color="blue") +
  geom_point(data = subset(all_data, variable =="depth_cm_MC"), aes(y=0.3, x=limits[10,2]), color="blue") +
  geom_point(data = subset(all_data, variable =="depth_cm_MC"), aes(y=0.3, x=limits[11,2]), color="blue") +
  geom_point(data = subset(all_data, variable =="depth_cm_MC"), aes(y=0.3, x=limits[12,2]), color="blue") +
  
  geom_point(data = subset(all_data, variable =="depth_cm_LOB"), aes(y=0.1, x=limits[1,1]), color="green") +
  geom_point(data = subset(all_data, variable =="depth_cm_LOB"), aes(y=0.1, x=limits[2,1]), color="green") +
  geom_point(data = subset(all_data, variable =="depth_cm_LOB"), aes(y=0.1, x=limits[3,1]), color="green") +
  geom_point(data = subset(all_data, variable =="depth_cm_LOB"), aes(y=0.1, x=limits[4,1]), color="green") +
  geom_point(data = subset(all_data, variable =="depth_cm_LOB"), aes(y=0.2, x=limits[5,1]), color="red") +
  geom_point(data = subset(all_data, variable =="depth_cm_LOB"), aes(y=0.2, x=limits[6,1]), color="red") +
  geom_point(data = subset(all_data, variable =="depth_cm_LOB"), aes(y=0.2, x=limits[7,1]), color="red") +
  geom_point(data = subset(all_data, variable =="depth_cm_LOB"), aes(y=0.2, x=limits[8,1]), color="red") +
  geom_point(data = subset(all_data, variable =="depth_cm_LOB"), aes(y=0.3, x=limits[9,1]), color="blue") +
  geom_point(data = subset(all_data, variable =="depth_cm_LOB"), aes(y=0.3, x=limits[10,1]), color="blue") +
  geom_point(data = subset(all_data, variable =="depth_cm_LOB"), aes(y=0.3, x=limits[11,1]), color="blue") +
  geom_point(data = subset(all_data, variable =="depth_cm_LOB"), aes(y=0.3, x=limits[12,1]), color="blue") +
  
  geom_point(data = subset(all_data, variable =="depth_cm_ROB"), aes(y=0.1, x=limits[1,3]), color="green") +
  geom_point(data = subset(all_data, variable =="depth_cm_ROB"), aes(y=0.1, x=limits[2,3]), color="green") +
  geom_point(data = subset(all_data, variable =="depth_cm_ROB"), aes(y=0.1, x=limits[3,3]), color="green") +
  geom_point(data = subset(all_data, variable =="depth_cm_ROB"), aes(y=0.1, x=limits[4,3]), color="green") +
  geom_point(data = subset(all_data, variable =="depth_cm_ROB"), aes(y=0.2, x=limits[5,3]), color="red") +
  geom_point(data = subset(all_data, variable =="depth_cm_ROB"), aes(y=0.2, x=limits[6,3]), color="red") +
  geom_point(data = subset(all_data, variable =="depth_cm_ROB"), aes(y=0.2, x=limits[7,3]), color="red") +
  geom_point(data = subset(all_data, variable =="depth_cm_ROB"), aes(y=0.2, x=limits[8,3]), color="red") +
  geom_point(data = subset(all_data, variable =="depth_cm_ROB"), aes(y=0.3, x=limits[9,3]), color="blue") +
  geom_point(data = subset(all_data, variable =="depth_cm_ROB"), aes(y=0.3, x=limits[10,3]), color="blue") +
  geom_point(data = subset(all_data, variable =="depth_cm_ROB"), aes(y=0.3, x=limits[11,3]), color="blue") +
  geom_point(data = subset(all_data, variable =="depth_cm_ROB"), aes(y=0.3, x=limits[12,3]), color="blue") +
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
  labs(title = paste(NodeName, ": Adult/Depth: Probability ~ Q", sep=""),
       y = "Probability",
       x = "Q (cfs)") #+ theme_bw(base_size = 15)

dev.off()

## percentage time
melt_time<-reshape2::melt(time_statsx, id=c("season", "position", "water_year", "Node"))
melt_time <- melt_time %>% 
  rename( Probability_Threshold = variable) %>%
  mutate(Species ="SAS", Life_Stage = "Adult", Hydraulic = "Depth", Node = NodeName)

write.csv(melt_time, paste("output_data/F1_", NodeName, "_SAS_adult_depth_time_stats.csv", sep=""))

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
  mutate(Species ="SAS", Life_Stage = "Adult", Hydraulic = "Depth")


## save df
write.csv(melt_days, paste("output_data/F1_", NodeName, "_SAS_adult_depth_total_days_long.csv", sep="") )

} ## end 1st loop




# Velocity ----------------------------------------------------------------


## upload habitat curve data
fitdata <- read.csv("output_data/adult_velocity_prob_curve_data.csv")

## upload hydraulic data
setwd("input_data/HecRas")

h <- list.files(pattern="hydraulic")
length(h) ## 18

## set wd back to main
setwd("/Users/katieirving/Documents/git/flow_eco_mech")
n=1

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
  labels <- c(vel_m_LOB = "Left Over Bank", vel_m_MC = "Main Channel", vel_m_ROB = "Right Over Bank")
  
  ### node figure for depth ~ Q
  file_name <- paste("figures/Application_curves/nodes/", NodeName, "_Velocity_Q.png", sep="")
  png(file_name, width = 500, height = 600)
  
  ggplot(hyd_vel, aes(x = Q, y=value)) +
    geom_line(aes( group = variable, lty = variable)) +
    scale_linetype_manual(values= c("dotted", "solid", "dashed"),
                          breaks=c("vel_m_LOB", "vel_m_MC", "vel_m_ROB"))+
    facet_wrap(~variable, scales="free_x", nrow=3, labeller=labeller(variable = labels)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
    labs(title = paste(NodeName, ": Velocity ~ Q"),
         y = "Velocity (m/s)",
         x = "Q (cfs)") #+ theme_bw(base_size = 15)
  
  dev.off()
  
  ## change NAs to 0 in concrete overbanks
  hyd_vel[is.na(hyd_vel)] <- 0

  ## use smooth spline to predict on new data set
  new_values <-smooth.spline(fitdata$velocity_fit, fitdata$prob_fit)
  
  all_data <- hyd_vel %>%
    group_by(variable) %>%
    mutate(prob_fit = predict(new_values, value)$y) %>%
    rename(vel_m = value)
  
  
  
  ## save out
  save(all_data, file=paste("output_data/F1_", NodeName, "_SAS_adult_velocity_discharge_probability.RData", sep=""))
  
  
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
  
  save(all_data, file=paste("output_data/F1_", NodeName, "_SAS_velocity_adult_discharge_probs_2010_2017_TS.RData", sep=""))
  
  ### define dataframes for 2nd loop
  
  ## Q Limits
  limits <- as.data.frame(matrix(ncol=3, nrow=12)) %>%
    rename(LOB = V1, MC = V2, ROB = V3) 
  rownames(limits)<-c("Low_Prob_1", "Low_Prob_2", "Low_Prob_3", "Low_Prob_4",
                      "Med_Prob_1", "Med_Prob_2", "Med_Prob_3", "Med_Prob_4",
                      "High_Prob_1", "High_Prob_2", "High_Prob_3", "High_Prob_4")
  
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
  
    ## bind shallow and deeper depths by 0.1 - 10cm & 120cm
    ## change all prob_fit lower than 0.1 to 0.1

    peak <- new_data %>%
      filter(prob_fit == max(prob_fit)) #%>%
    
    peakQ  <- max(peak$Q)
    min_limit <- filter(new_data, vel_m >0)
    min_limit <- min(min_limit$Q)
  
    ## Main channel curves
    
    ## find roots for each probability
    newx1a <- RootLinearInterpolant(new_data$Q, new_data$prob_fit, 0.1)
   
    
    if(length(newx1a) > 4) {
      newx1a <- c(newx1a[1], newx1aR[length(newx1a)])
    } else {
      newx1a <- newx1a
    }
    
    newx2a  <- RootLinearInterpolant(new_data$Q, new_data$prob_fit, 0.2)
    
    if(length(newx2a) > 4) {
      newx2a <- c(newx2a[1], newx2a[length(newx2a)])
    } else {
      newx2a <- newx2a
    }
    
    newx3a <- RootLinearInterpolant(new_data$Q, new_data$prob_fit, 0.3)

    
    if(min(new_data$prob_fit)>0.3) {
      newx3a <- min(new_data$Q)
    } else {
      newx3a <- newx3a
    }
    
    if(length(newx3a) > 4) {
      newx3a <- c(newx3a[1], newx3a[length(newx3a)])
    } else {
      newx3a <- newx3a
    }
    
    
    ## MAKE DF OF Q LIMITS
    limits[,p] <- c(newx1a[1], newx1a[2],newx1a[3], newx1a[4],
                    newx2a[1], newx2a[2],newx2a[3], newx2a[4], 
                    newx3a[1], newx3a[2],newx3a[3],newx3a[4])
    
    # create year_month column       
    new_datax <- new_data %>% unite(month_year, c(water_year,month), sep="-", remove=F) 
    
    # dataframe for stats -----------------------------------------------------
    
    ## define critical period or season for adult as all year is critical
    winter <- c(1,2,3,4,11,12) ## winter months
    summer <- c(5:10) ## summer months
    
    new_datax <- new_datax %>%
      mutate(season = ifelse(month %in% winter, "winter", "summer") )
    
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
  ## note that 0.1 upper/lower limit is max/min Q to adhere to 0.1 bound
  limits <- limits %>%
    mutate(Species ="SAS", Life_Stage = "Adult", Hydraulic = "Velocity", Node = NodeName)
  write.csv(limits, paste("output_data/F1_",NodeName,"_SAS_adult_velocity_Q_limits.csv", sep=""))

  ## plot thresholds
  file_name = paste("figures/Application_curves/Velocity/", NodeName, "_adult_depth_prob_Q_thresholds.png", sep ="")
  
  png(file_name, width = 500, height = 600)
  
  ggplot(all_data, aes(x = Q, y=prob_fit)) +
    geom_line(aes(group = variable, lty = variable)) +
    scale_linetype_manual(values= c("dotted", "solid", "dashed"))+
    #                       name="Cross\nSection\nPosition",
    #                       breaks=c("depth_cm_LOB", "depth_cm_MC", "depth_cm_ROB"),
    #                         labels = c("LOB", "MC", "ROB")) +
    
    facet_wrap(~variable, scales="free_x", nrow=3, labeller=labeller(variable = labels)) +
    geom_point(data = subset(all_data, variable =="vel_m_MC"), aes(y=0.1, x=limits[1,2]), color="green") +
    geom_point(data = subset(all_data, variable =="vel_m_MC"), aes(y=0.1, x=limits[2,2]), color="green") +
    geom_point(data = subset(all_data, variable =="vel_m_MC"), aes(y=0.1, x=limits[3,2]), color="green") +
    geom_point(data = subset(all_data, variable =="vel_m_MC"), aes(y=0.1, x=limits[4,2]), color="green") +
    geom_point(data = subset(all_data, variable =="vel_m_MC"), aes(y=0.2, x=limits[5,2]), color="red") +
    geom_point(data = subset(all_data, variable =="vel_m_MC"), aes(y=0.2, x=limits[6,2]), color="red") +
    geom_point(data = subset(all_data, variable =="vel_m_MC"), aes(y=0.2, x=limits[7,2]), color="red") +
    geom_point(data = subset(all_data, variable =="vel_m_MC"), aes(y=0.2, x=limits[8,2]), color="red") +
    geom_point(data = subset(all_data, variable =="vel_m_MC"), aes(y=0.3, x=limits[9,2]), color="blue") +
    geom_point(data = subset(all_data, variable =="vel_m_MC"), aes(y=0.3, x=limits[10,2]), color="blue") +
    geom_point(data = subset(all_data, variable =="vel_m_MC"), aes(y=0.3, x=limits[11,2]), color="blue") +
    geom_point(data = subset(all_data, variable =="vel_m_MC"), aes(y=0.3, x=limits[12,2]), color="blue") +
    
    geom_point(data = subset(all_data, variable =="vel_m_LOB"), aes(y=0.1, x=limits[1,1]), color="green") +
    geom_point(data = subset(all_data, variable =="vel_m_LOB"), aes(y=0.1, x=limits[2,1]), color="green") +
    geom_point(data = subset(all_data, variable =="vel_m_LOB"), aes(y=0.1, x=limits[3,1]), color="green") +
    geom_point(data = subset(all_data, variable =="vel_m_LOB"), aes(y=0.1, x=limits[4,1]), color="green") +
    geom_point(data = subset(all_data, variable =="vel_m_LOB"), aes(y=0.2, x=limits[5,1]), color="red") +
    geom_point(data = subset(all_data, variable =="vel_m_LOB"), aes(y=0.2, x=limits[6,1]), color="red") +
    geom_point(data = subset(all_data, variable =="vel_m_LOB"), aes(y=0.2, x=limits[7,1]), color="red") +
    geom_point(data = subset(all_data, variable =="vel_m_LOB"), aes(y=0.2, x=limits[8,1]), color="red") +
    geom_point(data = subset(all_data, variable =="vel_m_LOB"), aes(y=0.3, x=limits[9,1]), color="blue") +
    geom_point(data = subset(all_data, variable =="vel_m_LOB"), aes(y=0.3, x=limits[10,1]), color="blue") +
    geom_point(data = subset(all_data, variable =="vel_m_LOB"), aes(y=0.3, x=limits[11,1]), color="blue") +
    geom_point(data = subset(all_data, variable =="vel_m_LOB"), aes(y=0.3, x=limits[12,1]), color="blue") +
    
    geom_point(data = subset(all_data, variable =="vel_m_ROB"), aes(y=0.1, x=limits[1,3]), color="green") +
    geom_point(data = subset(all_data, variable =="vel_m_ROB"), aes(y=0.1, x=limits[2,3]), color="green") +
    geom_point(data = subset(all_data, variable =="vel_m_ROB"), aes(y=0.1, x=limits[3,3]), color="green") +
    geom_point(data = subset(all_data, variable =="vel_m_ROB"), aes(y=0.1, x=limits[4,3]), color="green") +
    geom_point(data = subset(all_data, variable =="vel_m_ROB"), aes(y=0.2, x=limits[5,3]), color="red") +
    geom_point(data = subset(all_data, variable =="vel_m_ROB"), aes(y=0.2, x=limits[6,3]), color="red") +
    geom_point(data = subset(all_data, variable =="vel_m_ROB"), aes(y=0.2, x=limits[7,3]), color="red") +
    geom_point(data = subset(all_data, variable =="vel_m_ROB"), aes(y=0.2, x=limits[8,3]), color="red") +
    geom_point(data = subset(all_data, variable =="vel_m_ROB"), aes(y=0.3, x=limits[9,3]), color="blue") +
    geom_point(data = subset(all_data, variable =="vel_m_ROB"), aes(y=0.3, x=limits[10,3]), color="blue") +
    geom_point(data = subset(all_data, variable =="vel_m_ROB"), aes(y=0.3, x=limits[11,3]), color="blue") +
    geom_point(data = subset(all_data, variable =="vel_m_ROB"), aes(y=0.3, x=limits[12,3]), color="blue") +
    
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
    labs(title = paste(NodeName, ": Adult/Velocity: Probability ~ Q", sep=""),
         y = "Probability",
         x = "Q (cfs)") #+ theme_bw(base_size = 15)
  
  dev.off()
  
  ## percentage time
  melt_time<-reshape2::melt(time_statsx, id=c("season", "position", "water_year", "Node"))
  melt_time <- melt_time %>% 
    rename( Probability_Threshold = variable) %>%
    mutate(Species ="SAS", Life_Stage = "Adult", Hydraulic = "Velocity", Node = NodeName)
  
  write.csv(melt_time, paste("output_data/F1_", NodeName, "_SAS_adult_velocity_time_stats.csv", sep=""))
  
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
    mutate(Species ="SAS", Life_Stage = "Adult", Hydraulic = "Velocity")
  
  
  ## save df
  write.csv(melt_days, paste("output_data/F1_", NodeName, "_SAS_adult_velocity_total_days_long.csv", sep="") )
  
} ## end 1st loop


