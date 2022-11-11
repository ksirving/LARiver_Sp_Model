### histogram probability scale and center

library(tidyverse)
library(dplyr)
library(sm)

setwd("/Users/katieirving/Documents/git/flow_eco_mech")

## work flow
## histograms as probability
## density or probability?
## scale and center using mean depth

## upload data 
## Depth
## Adult

ad_depth_con <- read.csv("output_data/05a_adult_depth_continuous.csv")
ad_depth_cat <- read.csv("output_data/05a_adult_depth_categorical.csv")
juv_depth_con <- read.csv("output_data/05a_juvenile_depth_continuous.csv")
juv_depth_cat <- read.csv("output_data/05a_juvenile_depth_categorical.csv")
sp_depth_cat <- read.csv("output_data/05a_spawning_depth_continuous.csv")

## combine data
all_depth <- rbind(ad_depth_con, ad_depth_cat)

## uncount data into frequency

depth_freq <- all_depth %>% 
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


### histogram

x <-depth_freq$Depth
h<-hist(x, breaks=10, col="red", xlab="Depth (cm)",
        main="Histogram with Normal Curve")
xfit<-seq(min(x),max(x),length=130)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)
plot(density(depth_freq$Depth))


## histogram with probability and prob dist curve
x <-depth_freq$Depth
h <- hist(x)
h$counts=(h$counts/sum(h$counts))
plot(h, xlab="Depth (cm)", ylab = "Probability")
par(new=TRUE)
xfit<-seq(min(x),max(x),length=120)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) ## gives density
# yfit <- yfit*diff(h$mids[1:2])*length(x) ## relates density back to frequency
#plot the line with no axes or labels
plot(xfit, yfit, axes=FALSE, xlab='', ylab='', type='l', col='red' )
#add these now with axis
axis(4, at=pretty(range(yfit)))

# ## histogram with probability and density - don't know how to get prob distribution of density
# x <-depth_freq$Depth
# d <- density(depth_freq$Depth)
# h <- hist(x, plot=FALSE)
# h$counts=(h$counts/sum(h$counts))
# plot(h, xlab="Depth (cm)", ylab = "Probability", main="Adult/Depth: Histogram & Density Probability")
# par(new=TRUE)
# plot(d,axes=FALSE, xlab='', ylab='', type='l', col='red', main='')
# axis(4, at=pretty(range(d$y)))

### probability histogram with prob curve on density

# x <-depth_freq$Depth
# h <- hist(x)
# h$counts=(h$counts/sum(h$counts))
# plot(h, xlab="Depth (cm)", ylab = "Probability")
# par(new=TRUE)
# xfit<-seq(min(x),max(x),length=120)
# # yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
# yfitd<-density(dnorm(xfit, mean=mean(x), sd=sd(x)))
# plot(yfitd,axes=FALSE, xlab='', ylab='', type='l', col='red', main='')
# axis(4, at=pretty(range(yfitd$y))) ## doesn't work!!!!

### scale and center
## histogram with percentage of observations and prob dist curve -THIS IS THE ONE WE WANT!!!!!!
mean ## 33.5929
scaled_x <-scale(depth_freq$Depth, scale=T, center=T)
scaled_x
h <- hist(scaled_x)
h$counts=(h$counts/sum(h$counts))*100
plot(h, xlab="Distance from mean Depth", ylab = "Percentage of observations",main = "Adult/Depth: Probability curve")
par(new=TRUE)
xfit<-seq(min(scaled_x),max(scaled_x),length=120)
yfit<-dnorm(xfit,mean=mean(scaled_x),sd=sd(scaled_x))
#plot the line with no axes or labels
plot(xfit, yfit, axes=FALSE, xlab='', ylab='', type='l', col='red' )
#add these now with axis
axis(4, at=pretty(range(yfit)))
## don't think we need the probability of histogram, as probability of scaled/centered makes more sense

## again with frequency - - 

# # mean ## 33.5929
# depth_freq$Scaled_Depth <-scale(depth_freq$Depth, scale=T, center=T)
# scaled_x <- depth_freq$Scaled_Depth
# h <- hist(scaled_x)
# # h$counts=(h$counts/sum(h$counts))
# plot(h,  xlab="Distance from mean Depth", ylab = "Freq. of observations", main = "Adult/Depth: Probability curve")
# par(new=TRUE)
# xfit<-seq(min(scaled_x),max(scaled_x),length=120)
# yfit<-dnorm(xfit,mean=mean(scaled_x),sd=sd(scaled_x))
# #plot the line with no axes or labels
# plot(xfit, yfit, axes=FALSE, xlab='', ylab='', type='l', col='red' )
# #add these now with axis
# axis(4, at=pretty(range(yfit)))
# # axis(1, at=pretty(seq(0,120,1)))
# ?axis ## need a label
# 
# ## get probabilities of a certain depth
# dnorm(-1, mean=mean(scaled_x),sd=sd(scaled_x))
# 0.2419707

### juvenile

juv_depth_con <- read.csv("output_data/05a_juvenile_depth_continuous.csv")
juv_depth_cat <- read.csv("output_data/05a_juvenile_depth_categorical.csv")
# sp_depth_cat <- read.csv("output_data/05a_spawning_depth_continuous.csv")

## combine data
all_depth <- rbind(juv_depth_con, juv_depth_cat)


## uncount data into frequency

depth_freq <- all_depth %>% 
  uncount(Abundance)
hist(depth_freq$Depth)

##look at different data sets

## compare different data sets
### get numbers for datasets
unique(depth_freq$Dataset)
sx <- depth_freq$Dataset == "Saiki"
smx <- depth_freq$Dataset == "SMEA"

depth_freq$Dataset_num[sx] <- 1
depth_freq$Dataset_num[smx] <- 2


attach(depth_freq)

# create value labels
data.f <- factor(Dataset_num, levels= 1:2,
                 labels = c("Saiki", "SMEA"))
data.f
str(Depth)
as.vector(Depth)
# plot densities
sm.density.compare(as.vector(Depth), Dataset_num, xlab="Depth (cm)")
title(main="Depth Distribution by Dataset (Juvenile)")

# add legend via mouse click
colfill<-c(2:(2+length(levels(data.f))))
legend(locator(1), levels(data.f), fill=colfill)

## again with frequency - - THIS IS THE ONE WE WANT!!!!!!

# mean ## 36.55253
depth_freq$Scaled_Depth <-scale(depth_freq$Depth, scale=T, center=T)
scaled_x <- depth_freq$Scaled_Depth
h <- hist(scaled_x)
h$counts=(h$counts/sum(h$counts))*100
plot(h,  xlab="Distance from mean Depth", ylab = "Percentage of observations", main = "Juvenile/Depth: Probability curve")
par(new=TRUE)
xfit<-seq(min(scaled_x),max(scaled_x),length=120)
yfit<-dnorm(xfit,mean=mean(scaled_x),sd=sd(scaled_x))
#plot the line with no axes or labels
plot(xfit, yfit, axes=FALSE, xlab='', ylab='', type='l', col='red' )
#add these now with axis
axis(4, at=pretty(range(yfit)))
# axis(1, at=pretty(seq(0,120,1)))
?axis ## need a label
h$breaks
## get probabilities of a certain depth
dnorm(-1, mean=mean(scaled_x),sd=sd(scaled_x))
# 0.2419707

max(depth_freq$Depth)
min(depth_freq$Depth)
