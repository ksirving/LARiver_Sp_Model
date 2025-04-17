## willow germination and depth

library(tidyverse)
library(lubridate)

## upload hydraulic data


# F57C <- read.csv("/Users/katieirving/Documents/git/flow_eco_mech/input_data/HecRas/hydraulic_ts_F57C.csv")
# LA8 <- read.csv("/Users/katieirving/Documents/git/flow_eco_mech/input_data/HecRas/hydraulic_ts_LA8.csv")
LA11 <- read.csv("/Users/katieirving/Documents/git/flow_eco_mech/input_data/HecRas/hydraulic_ts_LA11.csv")
# LA20 <- read.csv("/Users/katieirving/Documents/git/flow_eco_mech/input_data/HecRas/hydraulic_ts_LA20_2.csv")

## go through script one at a time
## go through script one at a time

hydraul <- LA11[,-1]
names(hydraul)
head(hydraul)
## select columns

hyd_dep <- hydraul[,c(1:3,5,9,13)]
colnames(hyd_dep) <-c("DateTime", "node", "Q", "depth_ft_LOB", "depth_ft_MC", "depth_ft_ROB")

## convert unit from feet to meters

hyd_dep <- hyd_dep %>%
  mutate(depth_cm_LOB = (depth_ft_LOB*0.3048)*100,
         depth_cm_MC = (depth_ft_MC*0.3048)*100,
         depth_cm_ROB = (depth_ft_ROB*0.3048)*100) %>%
  select(-contains("ft")) %>%
  mutate(date_num = seq(1,length(DateTime), 1))

# ## melt channel position data
hyd_dep<-reshape2::melt(hyd_dep, id=c("DateTime","Q", "node", "date_num"))


labels <- c(depth_cm_LOB = "Left Over Bank", depth_cm_MC = "Main Channel", depth_cm_ROB = "Right Over Bank")

## workflow
## get probabilities for depth at each hourly time step
## get thresholds i.e. 25, 50, 75%

hyd_dep$date_num <- seq(1,length(hyd_dep$DateTime), 1)

head(hyd_dep)
summary(depth_seedling_mod)

new_data <- hyd_dep %>%
  select(c(DateTime, Q, depth_cm, date_num)) %>%
  mutate(prob_fit = predict(depth_seedling_mod, newdata = hyd_dep)) %>%
  mutate(prob_fit = ifelse(prob_fit >100, 100, prob_fit)) ## percentage goes up to 200 so cut off at 100

head(new_data)

# format probability time series ------------------------------------------

## look at data using lubridate etc

## format date time
new_data$DateTime<-as.POSIXct(new_data$DateTime,
                              format = "%Y-%m-%d %H:%M",
                              tz = "GMT")

## create year, month, day and hour columns

new_data <- new_data %>%
  mutate(month = month(DateTime)) %>%
  mutate(year = year(DateTime)) %>%
  mutate(day = day(DateTime)) %>%
  mutate(hour = hour(DateTime))

head(new_data)


## define thresholds for germination

depth_germ_cm <- 5
depth_germ_lo_dur_days <-  85
depth_germ_hi_dur_days <- 280

## depth v discharge

plot(hyd_dep$Q, hyd_dep$depth_cm)

## plot
range(hyd_dep$Q) ## 0.00 998.845 

## smooth spline the curve to get exact value of discharge at a given probability
spl <- smooth.spline(hyd_dep$depth_cm ~ hyd_dep$Q)

## find discharge point at 5cm 

newy5 <- 5
newx5 <- try(uniroot(function(x) predict(spl, x, deriv = 0)$y - newy5,
                      interval = c(min(hyd_dep$Q), max(hyd_dep$Q)))$root, silent=T)
newx5 ## 1.521938

### define suitable years through if else statement
###	IF depth exceeds 5cm for >85 and <280 days, Suitable - in Q

head(hyd_dep)

plot(hyd_dep$Q, hyd_dep$depth_cm, type="n", main = "Willow/Germination: Depth according to Q", xlab="Q (cfs)", ylab="Depth (cm)")
lines(spl, col="black")
points(newx5, newy5, col="red", pch=19) # 5cm

### percentage of time above threshold

time_stats <- hyd_dep %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(Threshold = sum(Q >= newx5)/length(DateTime)*100) %>%
  distinct(year, Threshold)

time_stats

# Number of days above discharge ------------------------------------------
# need number of days discharge is above the limits outlined above - counted per month
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

## change year to water year

hyd_dep <- hyd_dep %>% 
  mutate(water_year = ifelse(month == 10 | month == 11 | month == 12, year, year-1))

new_data <- hyd_dep %>%
  group_by(month, day, water_year, ID = data.table::rleid(Q >= newx5)) %>%
  mutate(threshold5 = if_else(Q >= newx5,  row_number(), 0L))


head(new_data)

## melt data frame so that each probability column are all in one row 
## select only columns needed - Q, month, year, day all IDs and probs
# names(new_data)

new_datax <- select(new_data, c(Q, month, water_year, year, day, ID, threshold5) ) # all probs
names(new_datax)




## melt
melt_data<-reshape2::melt(new_datax, id=c("ID", "day", "month", "year", "Q", "water_year"))
melt_data <- rename(melt_data, Probability_Threshold = variable, 
                    consec_hours = value)

names(melt_data)
head(melt_data)
## groups data by year, month and ID & threshold
## counts the number of days in each month probability is within the depth of each threshold - days are not necessarily conseq
## each threshold separately

## count how many full days i.e. 24 hours
total_days01 <- melt_data %>% 
  group_by(ID, day, month, water_year) %>%
  summarise(n_hours = max(consec_hours))  %>%
  mutate(n_days = ifelse(n_hours >= 24, 1, 0)) # %>%
# total_days01
## count the number of days in each month
total_days_per_year01 <- total_days01 %>%
  group_by(water_year) %>%
  summarise(days_per_water_year = sum(n_days)) %>%
  mutate(suitablility = ifelse(days_per_water_year >= 85 & days_per_water_year <= 280, "Yes", "No"))

write.csv(total_days_per_year01, "output_data/M3_willow_germ_days_per_year_suiability.csv")


