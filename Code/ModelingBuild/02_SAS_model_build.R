## SAS curves - model build
## adult 

## produces probability curves for depth & velocity, for adult and juvenile
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

getwd()

## depth
ad_depth_con <- read.csv("output_data/bio/01_adult_depth_continuous_updated.csv") ## all wulff incl and thompson removed - remove SAWA?
ad_depth_cat <- read.csv("output_data/bio/01_adult_depth_categorical.csv")

# ad_depth_red <- subset(ad_depth_con, !Dataset=="Thompson")
all_depth <- rbind(ad_depth_con, ad_depth_cat)

unique(all_depth$Dataset) # 4 datasets, 

depth_freq <- all_depth %>% 
  uncount(Abundance)
# hist(depth_freq$Depth)

depth_freq <- subset(depth_freq, !Dataset=="SAWA")
dim(depth_freq) ## 1376


# Adult data distribution -------------------------------------------------------


# ## compare different data sets
unique(depth_freq$Dataset)

sx <- depth_freq$Dataset == "Saiki"
wx <- depth_freq$Dataset == "Wulff"
smx <- depth_freq$Dataset == "SMEA"

depth_freq$Dataset_num[sx] <- 1
depth_freq$Dataset_num[wx] <- 2
depth_freq$Dataset_num[smx] <- 3


attach(depth_freq)

# create value labels
data.f <- factor(Dataset_num, levels= 1:3,
                 labels = c( "Saiki", "Wulff", "SMEA"))

# plot densities
sm.density.compare(as.vector(Depth), Dataset_num, xlab="Depth (cm)")
title(main="Adult/Depth Distribution by Dataset")

# add legend via mouse click
colfill<-c(2:(2+length(levels(data.f))))
legend(locator(1), levels(data.f), fill=colfill)


# Adult model build -------------------------------------------------------

# ## validation
# library(boot)
# 
# set.seed(231241)
# mean_df <- function(dataset, i) median(dataset[i, "Depth"])
# res <- boot(depth_freq, mean_df, 1000)
# res

## check data
unique(depth_freq$Dataset) ## 3 datasets, 1376
mean(depth_freq$Depth) ## 44.4

## histogram with normal curve
x <-depth_freq$Depth
h<-hist(x, breaks=10, col="red", xlab="Depth (cm)",
        main="Adult/Depth Histogram with Normal Curve")
xfit<-seq(min(x),max(x),length=130)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)
# plot(density(depth_freq$Depth))


## probability curve - histogram scaled and centered depth, then transformed back to raw depth
png("figures/bio/02_SAS_Adult_velocity_Prob_curve.png", width = 700, height = 700)

depth_freq$Scaled_Depth <-scale(depth_freq$Depth, scale=T, center=T)
scaled_x <- depth_freq$Scaled_Depth
h <- hist(scaled_x, plot=F)
xfit<-seq(min(scaled_x),max(scaled_x),length=120)
yfit<-dnorm(xfit,mean=mean(scaled_x),sd=sd(scaled_x))
## x axis with raw depth values
xfit_r <- seq(min(depth_freq$Depth), max(depth_freq$Depth), length=120)
## plot curve with raw depth axis
plot(xfit_r, yfit, axes=FALSE, xlab='', ylab='', type='l', col='', main = "" )
axis(1, at=pretty(xfit_r))
par(new=TRUE)
#plot the line with no axes or labels
plot(xfit, yfit, axes=FALSE, xlab='Depth (cm)', ylab='Probability', type='l', col='red', main = "Adult/Depth: Probability curve" )
#add these now with axis
par(new=TRUE)
axis(2, at=pretty(range(yfit)))

dev.off()

## data frame with probabilities and depth - to combine with hydraulic data

fitdata <- data.frame(matrix(ncol=2, nrow=length(yfit)))
fitdata[,1] <- xfit_r
fitdata[,2] <- yfit
colnames(fitdata) <- c("depth_fit", "prob_fit")
head(fitdata)

write.csv(fitdata, "output_data/Models/02_adult_depth_prob_curve_data.csv")


# Velocity ----------------------------------------------------------------


## adult
ad_vel_con <- read.csv("output_data/bio/01_adult_velocity_continuous_updated.csv")
# dim(ad_vel_con) ## 343
# unique(ad_vel_con$Dataset) ## 2 datasets

all_vel <- ad_vel_con

vel_freq <- all_vel %>% 
  uncount(Abundance)
vel_freq <- na.omit(vel_freq)

# Adult Data distribution -------------------------------------------------------

## data distribution by dataset

all_vel <- ad_vel_con

vel_freq <- all_vel %>% 
  uncount(Abundance)
vel_freq <- na.omit(vel_freq)

tx <- vel_freq$Dataset == "Saiki"
wx <- vel_freq$Dataset == "Wulff"

vel_freq$Dataset_num[tx] <- 1
vel_freq$Dataset_num[wx] <- 2

attach(vel_freq)

# create value labels
data.f <- factor(Dataset_num, levels= 1:2,
                 labels = c("Saiki", "Wulff"))
data.f
# plot densities
sm.density.compare(as.vector(Velocity), Dataset_num, xlab="Velocity m/s")
title(main="Adult/Velocity")

# add legend via mouse click
colfill<-c(2:(2+length(levels(data.f))))
legend(locator(1), levels(data.f), fill=colfill)


# Adult model build -------------------------------------------------------

## need to predict outside of the velocity range from the curve. 

# check data
# unique(vel_freq$Dataset)
# mean(vel_freq$Velocity) ## 0.6121954
# dim(vel_freq) ## 1167
# range(vel_freq$Velocity)

## probability curve
vel_freq$Scaled_Vel <-scale(vel_freq$Velocity, scale=T, center=T)
scaled_x <- vel_freq$Scaled_Vel
h <- hist(scaled_x, plot=F)
xfit<-seq(min(scaled_x),max(scaled_x),length=1000)
yfit<-dnorm(xfit,mean=mean(scaled_x),sd=sd(scaled_x))

## x axis with raw velocity values
xfit_r <- seq(min(vel_freq$Velocity), max(vel_freq$Velocity), length=1000)

## plot curve with raw depth axis
png("figures/bio/02_SAS_Adult_velocity_Prob_curve.png", width = 700, height = 700)

plot(xfit_r, yfit, axes=FALSE, xlab='', ylab='', type='l', col='', main = "" )
axis(1, at=pretty(xfit_r), cex.axis=2)
par(new=TRUE)
#plot the line with no axes or labels
plot(xfit, yfit, axes=FALSE, xlab='Velocity (m/s)', ylab='Probability', type='l', col='red', main = "Adult/Velocity",
     cex.main = 2, cex.axis=2, cex.lab=2)
## add 1sd shift
par(new=TRUE)

#add these now with axis

axis(2, at=pretty(range(yfit)), cex.axis=2)
dev.off()
## data frame with probabilities and depth

fitdata <- data.frame(matrix(ncol=2, nrow=length(yfit)))
fitdata[,1] <- xfit_r
fitdata[,2] <- yfit
colnames(fitdata) <- c("velocity_fit", "prob_fit")

write.csv(fitdata, "output_data/Models/02_adult_velocity_prob_curve_data.csv")

