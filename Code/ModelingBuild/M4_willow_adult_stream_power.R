## willow germination and depth

library(tidyverse)
library(lubridate)


## upload hydraulic data

hydraul <- read.csv("input_data/demo_ts_F57C.csv")
head(hydraul)

hyd_dep <- hydraul[,c(1:3,11)]
colnames(hyd_dep)[4] <-"stream_power_lb_ft"

## convert unit from lb ft s to w/m2
hyd_dep$stream_power_wm2 <- (hyd_dep$stream_power_lb_ft*4.44822)/0.3048

## format date time
hyd_dep$DateTime<-as.POSIXct(hyd_dep$DateTime,
                             format = "%Y-%m-%d %H:%M",
                             tz = "America/Los_Angeles")

## create year, month, day and hour columns

hyd_dep <- hyd_dep %>%
  mutate(month = month(DateTime))%>%
  mutate(year = year(DateTime))%>%
  mutate(day = day(DateTime))%>%
  mutate(hour = hour(DateTime))


## depth v discharge

plot(hyd_dep$Q, hyd_dep$stream_power_wm2)

## plot
range(hyd_dep$Q) ## 0.00 998.845
range(hyd_dep$stream_power_lb_ft)

## smooth spline the curve to get exact value of discharge at a given probability
spl <- smooth.spline(hyd_dep$stream_power_wm2 ~ hyd_dep$Q)

## find discharge point at 20 w/m2 & 4000 w/m2

newy20 <- 20
newx20 <- try(uniroot(function(x) predict(spl, x, deriv = 0)$y - newy20,
                     interval = c(min(hyd_dep$Q), max(hyd_dep$Q)))$root, silent=T)

## if no value, return an NA
newx20 <- ifelse(class(newx20) == "try-error",  NA, newx20)


newy4000 <- 4000
newx4000 <- try(uniroot(function(x) predict(spl, x, deriv = 0)$y - newy4000,
                      interval = c(min(hyd_dep$Q), max(hyd_dep$Q)))$root, silent=T)
## if no value, return an NA
newx4000 <- ifelse(class(newx4000) == "try-error",  NA, newx4000)

newx4000 
### define suitable years through if else statement
###	IF stream power less than 20 or more than 4000 unsuitable

head(hyd_dep)

plot(hyd_dep$Q, hyd_dep$stream_power_wm2, type="n", main = "Willow/Adult: Stream Power according to Q", xlab="Q (cfs)", ylab="Stream Power (W/m2)")
lines(spl, col="black")
points(newx20, newy20, col="red", pch=19) # 20
points(newx4000, newy4000, col="red", pch=19) # 20


### percentage of time above threshold

time_stats <- hyd_dep %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(Threshold2 = sum(Q >= newx20)/length(DateTime)*100) %>%
  dplyr::mutate(Threshold4 = sum(Q <= newx4000)/length(DateTime)*100) %>%
  distinct(year, Threshold2, Threshold4)

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
  group_by(month, day, water_year, ID = data.table::rleid(Q >= newx20)) %>%
  mutate(threshold20 = if_else(Q >= newx20,  row_number(), 0L)) %>%
  ungroup() %>%
  group_by(month, day, water_year, ID = data.table::rleid(Q <= newx4000)) %>%
  mutate(threshold4000 = if_else(Q >= newx4000,  row_number(), 0L))

head(new_data)

## melt data frame so that each probability column are all in one row 
## select only columns needed - Q, month, year, day all IDs and probs
# names(new_data)

new_datax <- select(new_data, c(Q, month, water_year, year, day, ID, threshold20, threshold4000) ) # all probs
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

write.csv(total_days_per_year01, "output_data/M3_willow_adult_stream_power_days_per_year_suiability.csv")


