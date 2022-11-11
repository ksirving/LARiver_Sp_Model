#### standardise abundance data
library(tidyverse)
library(dplyr)
install.packages("lattice")
library(lattice)
library(sm)

setwd("/Users/katieirving/Documents/git/flow_eco_mech")

## upload data
ad_depth_con <- read.csv("output_data/05a_adult_depth_continuous.csv")
ad_depth_cat <- read.csv("output_data/05a_adult_depth_categorical.csv")
## normalise abundance data

## The abundance of fish from one study, divided by the total abundance of all studies combined
ad_depth_con
## total abundance
sum(ad_depth_con$Abundance) #2266

## combine data
all_depth <- rbind(ad_depth_con, ad_depth_cat)
hist(all_depth$Depth)
## uncount data into frequency

depth_freq <- all_depth %>% 
  uncount(Abundance)
hist(depth_freq$Depth)

### relative frequency histogram - percentage

x <-depth_freq$Depth
h<-hist(x, breaks=10, col="red", xlab="Depth (cm)",
        main="Adult Depth", freq=F)
xfit<-seq(min(x),max(x),length=130)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)

xfit ## depths
yfit ## frequencies
plot(density(depth_freq$Depth))

## compare different data sets
### get numbers for datasets
unique(depth_freq$Dataset)
tx <- depth_freq$Dataset == "Thompson"
sx <- depth_freq$Dataset == "Saiki"
wx <- depth_freq$Dataset == "Wulff"
swx <- depth_freq$Dataset == "SAWA"
smx <- depth_freq$Dataset == "SMEA"

depth_freq$Dataset_num[tx] <- 1
depth_freq$Dataset_num[sx] <- 2
depth_freq$Dataset_num[wx] <- 3
depth_freq$Dataset_num[swx] <- 4
depth_freq$Dataset_num[smx] <- 5

attach(depth_freq)

# create value labels
data.f <- factor(Dataset_num, levels= 1:5,
                 labels = c("Thompson", "Saiki", "Wulff", "SAWA", "SMEA"))
data.f
str(Depth)
as.vector(Depth)
# plot densities
sm.density.compare(as.vector(Depth), Dataset_num, xlab="Depth (cm)")
title(main="Depth Distribution by Dataset")

# add legend via mouse click
colfill<-c(2:(2+length(levels(data.f))))
legend(locator(1), levels(data.f), fill=colfill)

## top of peak is 0.030 - what can we relate that to? is it the same for other variables?

hist(x)
