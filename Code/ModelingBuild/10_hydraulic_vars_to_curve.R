## apply curve to hydraulic variables

library(tidyverse)
library(dplyr)
# library(sm)
library(tidyr)

setwd("/Users/katieirving/Documents/git/flow_eco_mech")

## upload hydraulic data

hydraul <- read.csv("input_data/demo_ts_F57C.csv")
## select columns
hyd_dep <- hydraul[,c(1:3,9)]
colnames(hyd_dep)[4] <-"depth_ft"

## convert unit from feet to meters
hyd_dep$depth_cm <- (hyd_dep$depth_ft*0.3048)*100

## plot depth data

# plot(hyd_dep$DateTime, hyd_dep$depth_cm)
# dim(hyd_dep) ## 60273

## get 3 month time series

## start date 2010-10-01 01:00:00
## end date 2010-12-31 23:00:00
## subset data to one year

# hyd_dep <- hyd_dep[1:8760,]
tail(hyd_dep)
head(hyd_dep)
dim(hyd_dep)
# plot(hyd_dep$DateTime, hyd_dep$depth_cm)

## plot time series - use numbers for now, add dates in plot

str(hyd_dep)

hyd_dep$date_num <- seq(1,length(hyd_dep$DateTime), 1)
plot(hyd_dep$date_num, hyd_dep$depth_cm, type="n")
lines(hyd_dep$date_num, hyd_dep$depth_cm)
# hyd_dep$depth_cm[1500:2038]

### upload curve data
## depth
ad_depth_con <- read.csv("output_data/05a_adult_depth_continuous.csv")
ad_depth_cat <- read.csv("output_data/05a_adult_depth_categorical.csv")

## combine data adult
all_depth <- rbind(ad_depth_con, ad_depth_cat)

## uncount data
depth_freq <- all_depth %>% 
  uncount(Abundance)
unique(depth_freq$Dataset)

depth_freq <- subset(depth_freq, Dataset!="Thompson")
## curve data in dataframe - no plot

## scaled data
depth_freq$Scaled_Depth <-scale(depth_freq$Depth, scale=T, center=T)
scaled_x <- depth_freq$Scaled_Depth
h <- hist(scaled_x, plot=F)
xfit<-seq(min(scaled_x),max(scaled_x),length=10000)
yfit<-dnorm(xfit,mean=mean(scaled_x),sd=sd(scaled_x))

##  raw depth values
xfit_r <- seq(min(depth_freq$Depth), max(depth_freq$Depth), length=10000)
max(depth_freq$Depth)
tail(xfit_r)
## data frame with probabilities and depth

fitdata <- data.frame(matrix(ncol=2, nrow=length(yfit)))
fitdata[,1] <- xfit_r
fitdata[,2] <- yfit
colnames(fitdata) <- c("depth_fit", "prob_fit")
xfit_r

## round the depths - don't need the high resolution
# ?round
hyd_dep$depth_cm_round <- round(hyd_dep$depth_cm, digits=1 )
fitdata$depth_fit_round <- round(fitdata$depth_fit, digits=1)
# head(fitdata)
# head(hyd_dep)
# match(hyd_dep$depth_cm_round,fitdata$depth_fit_round)

plot(hyd_dep$date_num, hyd_dep$depth_cm_round, type="n")
lines(hyd_dep$date_num, hyd_dep$depth_cm_round)

all_data <- merge(hyd_dep, fitdata, by.x="depth_cm_round", by.y="depth_fit_round")
head(all_data)
head(hyd_dep)

all_data <- all_data[, c(1,2,7,9)]
sum(is.na(all_data))

## remove duplicate date_num (date time) and order

all_data <- all_data[!duplicated(all_data$date_num),]
new_data <- all_data[order(all_data$date_num),]

## plot modelled depth with probability

par(mar=c(4,6,2,4))

plot(new_data$date_num, new_data$depth_cm_round, type="n", axes=FALSE, xlab="Date", ylab="Depth (cm)", main="Adult/Depth: 3 month time series")
lines(new_data$date_num, new_data$depth_cm_round)
axis(1, at=pretty(new_data$date_num), labels=c("2010-10-1","2010-10-22","2010-11-12","2010-12-03","2010-12-30", "2010-01-20"), las=1)
axis(2, at=pretty(new_data$depth_cm_round))
par(new=TRUE)
plot(new_data$date_num, new_data$prob_fit, axes=F, type="n", xlab="", ylab="")
lines(new_data$date_num, new_data$prob_fit, col="red")
axis(4, at=pretty(all_data$prob_fit))
mtext("Probability", side=4, line=2.5)

# dim(new_data)
# new_data[100,]
# 
# ## zoom in on low prob scores
# new_data2 <- new_data[1:500,]
# new_data2[pretty(new_data2$date_num),]
# 
# plot(new_data2$date_num, new_data2$depth_cm_round, type="n", axes=FALSE, xlab="Date", ylab="Depth (cm)", main="Adult/Depth: 500 day time series")
# lines(new_data2$date_num, new_data2$depth_cm_round)
# axis(1, at=pretty(new_data2$date_num), 
#      labels=c("2010-10-01","2010-10-05","2010-10-09","2010-10-13","2010-10-17", "2010-10-22"), las=1)
# axis(2, at=pretty(new_data2$depth_cm_round))
# par(new=TRUE)
# plot(new_data2$date_num, new_data2$prob_fit, axes=F, type="n", xlab="", ylab="")
# lines(new_data2$date_num, new_data2$prob_fit, col="red")
# axis(4, at=pretty(all_data$prob_fit))
# mtext("Probability", side=4, line=2.5)
# 
# ## separate dat in new data
# ?separate
# new_data$DateTime
# 
# new_datax <- separate(new_data, DateTime, into=c("Date", "Time"), sep=" ", remove=F)
# head(new_datax)
# unique(new_datax$Date)[17:22] ## december weirdness
# 
# new_data3 <- subset(new_datax, Date %in% unique(new_datax$Date)[17:22])
# head(new_data3)
# dim(new_data3)
# new_data3$DateTime
# seq(1, 49,1)
# ## zoom in more
# 
# par(mar=c(8,6,2,4))
# ## depth time series
# plot(new_data3$date_num, new_data3$depth_cm_round, type="n", axes=FALSE, xlab="", ylab="Depth (cm)", main="Adult/Depth: 7 day time series")
# lines(new_data3$date_num, new_data3$depth_cm_round)
# axis(1, at=new_data3$date_num, labels=new_data3$Date, las=3)
# mtext("Date", side=1, line=5.5)
# axis(2, at=pretty(new_data3$depth_cm_round))
# ## probability 
# par(new=TRUE)
# plot(new_data3$date_num, new_data3$prob_fit, axes=F, type="n", xlab="", ylab="")
# 
# lines(new_data3$date_num, new_data3$prob_fit, col="red")
# axis(4, at=pretty(all_data$prob_fit), col="red")
# mtext("Probability", side=4, line=2.5, col="red")
# 
# ## overlay velocity on figure
# ## drop of certain percentage of probability over the time series - number of events and duration of events??
# ## what's happening in the other parts of the x-section? 
# ## not suitable for that amount of time - overall % (or other measurement) of suitability over the time series?
# ## how do we measure success?
# ## smooth the probability curve? loess??
# 
# write.csv(new_data3, "output_data/10_2010_oct_dip_test.csv")

## smoothing splines - tried and failed!!
# par(mar=c(8,6,2,4))
# ## depth time series
# plot(new_data3$date_num, new_data3$depth_cm_round, type="n", axes=FALSE, xlab="", ylab="Depth (cm)", main="Adult/Depth: 7 day time series")
# lines(new_data3$date_num, new_data3$depth_cm_round)
# axis(1, at=new_data3$date_num, labels=new_data3$Date, las=3)
# mtext("Date", side=1, line=5.5)
# axis(2, at=pretty(new_data3$depth_cm_round))
# ## probability 
# par(new=TRUE)
# plot(new_data3$date_num, new_data3$prob_fit, axes=F, type="n", xlab="", ylab="")
# 
# # smoothingSpline1 = smooth.spline(new_data3$date_num, new_data3$prob_fit, spar=1)
# # smoothingSpline = smooth.spline(new_data3$date_num, new_data3$prob_fit, spar=0.35)
# # smoothingSpline0.1 = smooth.spline(new_data3$date_num, new_data3$prob_fit, spar=0.1)
# loessFit <- loess(prob_fit~date_num, new_data3, span = 0.2)
# approxFit <- approx(new_data3$date_num, new_data3$prob_fit)
# 
# # lines(smoothingSpline, col="red")
# # lines(smoothingSpline1, col="purple")
# lines(loessFit, col="purple")
# # lines(approxFit, col="blue")
# # lines(smoothingSpline, col="orange")
# lines(new_data3$date_num, new_data3$prob_fit, col="red")
# 
# axis(4, at=pretty(all_data$prob_fit), col="red")
# mtext("Probability", side=4, line=2.5, col="red")

## moving average to smooth
install.packages("TTR")
library("TTR")

## analysse the data as time series

head(new_data)
tail(new_data)
firstHour <- 24*(as.Date("2010-10-17 00:00:00")-as.Date("2010-1-1 00:00:00"))
new_dataTS <- ts(new_data$prob_fit,start=c(2010,firstHour),frequency=24*365)
depth_ts <- ts(new_data$depth_cm_round, start=c(2010,firstHour),frequency=24*365)

plot.ts(new_dataTS, col="red")
par(new=T)
plot.ts(depth_ts)

par(mar=c(4,6,2,4))

plot.ts(depth_ts, type="n", xlab="Date", ylab="Depth (cm)", main="Adult/Depth: Time series")
lines(depth_ts, col="darkgrey")
# axis(1, at=pretty(depth_ts))
# axis(2, at=pretty(depth_ts))
par(new=TRUE)
plot.ts(new_dataTS, axes=F, type="n", xlab="", ylab="")
lines(new_dataTS, col="red")
axis(4, at=pretty(new_dataTS), col="red")
mtext("Probability", side=4, line=2.5, col="red")

## SMA for trend - we want seasonal 
new_dataTS3 <- TTR::SMA(new_dataTS, n=3)
plot.ts(new_dataTS3)

new_dataTS8 <- TTR::SMA(new_dataTS, n=8)
plot.ts(new_dataTS8)

new_dataTS5 <- TTR::SMA(new_dataTS, n=5)
plot.ts(new_dataTS5)

new_dataTS1 <- TTR::SMA(new_dataTS, n=1)
plot.ts(new_dataTS1)


## decompose data

new_dataTS_comp <- decompose(new_dataTS)
plot(new_dataTS_comp)

##  do the same over 1 water year - where are the seasonal dips/unsuitable habitat?
## subset to one year

new_dataTS2010 <- window(new_dataTS, start = 2010, end = 2013)
length(new_dataTS2010) ## 10586
length(new_dataTS)

plot.ts(new_dataTS2010)

new_dataTS2010_comp <- decompose(new_dataTS2010)
plot(new_dataTS2010_comp)

library(dplyr)  

sum(new_dataTS2010<=0.2) ## 225 hours where probability is less than 0.2
?filter
head(new_data)
low_probs <- filter(new_data, prob_fit <=0.2 )

dim(low_probs) # 5516



## same with velocity

# ## remove datenum
# 
# new_datats <- new_datats[, -5]
# length()
# install.packages("zoo")
# library(zoo)
# ?zoo
# 
# ## ts with zoo
# hourly_ts <- zoo(
#   x         = new_data3[["prob_fit"]],
#   order.by  = new_data3[["DateTime"]],
#   frequency = unique(length(new_data3$DateTime))
# )
# 
# head(hourly_ts)
# length(hourly_ts)
# 
# plot(hourly_ts)
# 
# start(hourly_ts)
# end(hourly_ts)
