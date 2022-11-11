### creating intuitive figures

library(tidyverse)
library(dplyr)
library(sm)

setwd("/Users/katieirving/Documents/git/flow_eco_mech")

# work flow
# depth is skewing to the shallower depths
# remove thompson
# standardise observations per dataset/site - investigate how
# need a relative measure of observations with depth

ad_depth_con <- read.csv("output_data/05a_adult_depth_continuous.csv")
ad_depth_cat <- read.csv("output_data/05a_adult_depth_categorical.csv")
juv_depth_con <- read.csv("output_data/05a_juvenile_depth_continuous.csv")
juv_depth_cat <- read.csv("output_data/05a_juvenile_depth_categorical.csv")
# sp_depth_cat <- read.csv("output_data/08a_spawning_depth_abundance.csv")
# sp_depth_cat
# 
head(ad_depth_con)

## change abundance to presence only to see difference in histogram

sum(ad_depth_con$Abundance==0)
which(ad_depth_con$Abundance==0)

ad_depth_con[c(145:148),] ## thompson data

## add presence column

ad_depth_con$presence <- ifelse(ad_depth_con$Abundance == 0,0,1)
hist(ad_depth_con$Depth)
## define index for density histogram


tx <- ad_depth_con$Dataset == "Thompson"
sx <- ad_depth_con$Dataset == "Saiki"
wx <- ad_depth_con$Dataset == "Wulff"
swx <- ad_depth_con$Dataset == "SAWA"
# smx <- ad_depth_con$Dataset == "SMEA"

ad_depth_con$Dataset_num[tx] <- 1
ad_depth_con$Dataset_num[sx] <- 2
ad_depth_con$Dataset_num[wx] <- 3
ad_depth_con$Dataset_num[swx] <- 4
# ad_depth_con$Dataset_num[smx] <- 5

attach(ad_depth_con)

# create value labels
data.f <- factor(Dataset_num, levels= 1:4,
                 labels = c("Thompson", "Saiki", "Wulff", "SAWA"))
data.f
str(Depth)
as.vector(Depth)
# plot densities
sm.density.compare(as.vector(Depth), Dataset_num, xlab="Depth (cm)")
title(main="Depth Distribution by Dataset: Presence only")

# add legend via mouse click
colfill<-c(2:(2+length(levels(data.f))))
legend(locator(1), levels(data.f), fill=colfill)


## uncount data into frequency

depth_freq <- ad_depth_con %>% 
  uncount(Abundance)
hist(depth_freq$Depth)

##look at different data sets

## compare different data sets
### get numbers for datasets
unique(depth_freq$Dataset)
tx <- depth_freq$Dataset == "Thompson"
sx <- depth_freq$Dataset == "Saiki"
wx <- depth_freq$Dataset == "Wulff"
swx <- depth_freq$Dataset == "SAWA"
# smx <- depth_freq$Dataset == "SMEA"

depth_freq$Dataset_num[tx] <- 1
depth_freq$Dataset_num[sx] <- 2
depth_freq$Dataset_num[wx] <- 3
depth_freq$Dataset_num[swx] <- 4
# depth_freq$Dataset_num[smx] <- 5

attach(depth_freq)

# create value labels
data.f <- factor(Dataset_num, levels= 1:4,
                 labels = c("Thompson", "Saiki", "Wulff", "SAWA"))
data.f
str(Depth)
as.vector(Depth)
# plot densities
sm.density.compare(as.vector(Depth), Dataset_num, xlab="Depth (cm)")
title(main="Depth Distribution by Dataset: Abundance")

# add legend via mouse click
colfill<-c(2:(2+length(levels(data.f))))
legend(locator(1), levels(data.f), fill=colfill)

## presence only shifts the peak to 40, and curve declines less steeply into deeper depths

## see which dataset deeper depths are

deep50 <- subset(ad_depth_con, Depth >=50)
deep50 ## 3 datasets Saiki, Wulff & SAWA have depths above 50cm
dim(deep50) ## 63 instances of deeper depths
dim(ad_depth_con) ## 227

deep41 <- subset(ad_depth_con, Depth >=41)
deep41 ## 3 datasets Saiki, Wulff & SAWA have depths above 50cm
dim(deep41) ## 108 instances of deeper depths
dim(ad_depth_con) ## 227

## deeper depths are limited in availability,  but still utilsed

mean(ad_depth_con$Depth)

## look at histogram without thompson

ad_depth_red <- subset(ad_depth_con, !Dataset=="Thompson")
tail(ad_depth_red)
dim(ad_depth_red) #212
## histogram of presence only
hist(ad_depth_red$Depth)

depth_freq <- ad_depth_red %>% 
  uncount(Abundance)
hist(depth_freq$Depth)
mean(depth_freq$Depth) ## 43.09675

## probaility curve without thompson
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
plot(xfit, yfit, axes=FALSE, xlab='Depth (cm)', ylab='Probability', type='l', col='red', main = "Adult/Depth: Probability curve (reduced data)" )
## add 1sd shift
par(new=TRUE)

#add these now with axis

axis(2, at=pretty(range(yfit)))

## add smea back in and redo curve
ad_depth_con <- read.csv("output_data/05a_adult_depth_continuous.csv")
ad_depth_cat <- read.csv("output_data/05a_adult_depth_categorical.csv")

ad_depth_red <- subset(ad_depth_con, !Dataset=="Thompson")
all_depth <- rbind(ad_depth_red, ad_depth_cat)

depth_freq <- all_depth %>% 
  uncount(Abundance)
hist(depth_freq$Depth)
mean(depth_freq$Depth) ## 44.79041

## probaility curve without thompson
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
plot(xfit, yfit, axes=FALSE, xlab='Depth (cm)', ylab='Probability', type='l', col='red', main = "Adult/Depth: Probability curve (reduced data)" )
## add 1sd shift
par(new=TRUE)

#add these now with axis

axis(2, at=pretty(range(yfit)))
