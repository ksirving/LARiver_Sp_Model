### creating intuitive figures

library(tidyverse)
library(dplyr)
library(sm)

setwd("/Users/katieirving/Documents/git/flow_eco_mech")

## work flow
## are the pnorm values the same as dnorm with scaled data?
## high/mid/low probability - quantiles - prob values to 1-0
## try model with removed lower occupied depths
## transform scaled data back to raw values

## upload data 
## Depth
## Adult

ad_depth_con <- read.csv("output_data/05a_adult_depth_continuous.csv")
ad_depth_cat <- read.csv("output_data/05a_adult_depth_categorical.csv")
juv_depth_con <- read.csv("output_data/05a_juvenile_depth_continuous.csv")
juv_depth_cat <- read.csv("output_data/05a_juvenile_depth_categorical.csv")
sp_depth_cat <- read.csv("output_data/08a_spawning_depth_abundance.csv")
sp_depth_cat

## combine data adult
all_depth <- rbind(ad_depth_con, ad_depth_cat)

depth_freq <- all_depth %>% 
  uncount(Abundance)
## histogram with curve for slide

# h<- hist(depth_freq$Depth, xlab="Depth (cm)", main="Adult/Depth - Histogram")
# xfit<-seq(min(x),max(x),length=130)
# yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
# yfit[1]
# yfit <- yfit*diff(h$mids[1:2])*length(x)
# lines(xfit, yfit, col="red", lwd=2)


### centered and scaled histogram probability
mean ## 33.5929

# subset(depth_freq, Scaled_Depth >=2)

depth_freq$Scaled_Depth <-scale(depth_freq$Depth, scale=T, center=T)
scaled_x <- depth_freq$Scaled_Depth
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

## are the values the same? no!
dnorm(-1, mean=mean(scaled_x),sd=sd(scaled_x)) ## 0.2419707
pnorm(-1, mean=mean(scaled_x),sd=sd(scaled_x)) ## 0.1586553
dnorm(0, mean=mean(scaled_x),sd=sd(scaled_x)) ## 0.3989423
pnorm(0, mean=mean(scaled_x),sd=sd(scaled_x)) ## 0.5

### transform back to raw values
# dataframe of sclaed and depth data

head(depth_freq)

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
plot(xfit, yfit, axes=FALSE, xlab='Depth (cm)', ylab='Probability Quantile', type='l', col='red', main = "Adult/Depth: Probability curve" )

#add these now with axis
# ## get quantiles of yfit (probability)
quants <- as.vector(quantile(yfit))

axis(2, at=quants[c(1,3:5)], labels=c(0,0.50,0.75,1), las=2)
#### y axis needs to be 0-1

## plot with 0-0.4 to show the tag

# depth_freq$Scaled_Depth <-scale(depth_freq$Depth, scale=T, center=T)
# scaled_x <- depth_freq$Scaled_Depth
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
## add 1sd shift
par(new=TRUE)


plot(xfit1, yfit, axes=FALSE, xlab='Depth (cm)', ylab='Probability', type='l', col='red', main = "Adult/Depth: Probability curve" )

#add these now with axis
# ## get quantiles of yfit (probability)
# quants <- as.vector(quantile(yfit))
axis(2, at=pretty(range(yfit)))

## add 1sd shift
## plot with 0-0.4 to show the tag

# depth_freq$Scaled_Depth <-scale(depth_freq$Depth, scale=T, center=T)
# scaled_x <- depth_freq$Scaled_Depth
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

axis(2, at=pretty(range(yfit)))

## add 1sd shift
xfit1 <- xfit+1
# points(xfit1, yfit, col="red", pch=2)
lines(xfit1, yfit, col="red",lty=2)

### histogram with freq and only curve

x <-depth_freq$Depth
h<-hist(x, breaks=10, lty="white", xlab="Depth (cm)",
        main="Adult/Depth: Curve")
xfit<-seq(min(x),max(x),length=130)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))

yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="red", lwd=2)

### spawning curve
sp_depth_cat

## select columns
all_depth <- select(sp_depth_cat, Depth, Abundance)
all_depth <- na.omit(all_depth)

depth_freq <- all_depth %>% 
  uncount(Abundance)
## histogram with curve for slide

# h<- hist(depth_freq$Depth, xlab="Depth (cm)", main="Adult/Depth - Histogram")
# xfit<-seq(min(x),max(x),length=130)
# yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
# yfit[1]
# yfit <- yfit*diff(h$mids[1:2])*length(x)
# lines(xfit, yfit, col="red", lwd=2)


### centered and scaled histogram probability


### transform back to raw values
# dataframe of sclaed and depth data

head(depth_freq)

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
plot(xfit, yfit, axes=FALSE, xlab='Depth (cm)', ylab='Probability Quantile', type='l', col='red', main = "Adult/Depth: Probability curve" )

#add these now with axis
# ## get quantiles of yfit (probability)
quants <- as.vector(quantile(yfit))

axis(2, at=quants[c(1,3:5)], labels=c(0,0.50,0.75,1), las=2)
#### y axis needs to be 0-1

## plot with 0-0.4 to show the tag

# depth_freq$Scaled_Depth <-scale(depth_freq$Depth, scale=T, center=T)
# scaled_x <- depth_freq$Scaled_Depth
h <- hist(scaled_x, plot=F)
xfit<-seq(min(scaled_x),max(scaled_x),length=60)
yfit<-dnorm(xfit,mean=mean(scaled_x),sd=sd(scaled_x))

## x axis with raw depth values
xfit_r <- seq(min(depth_freq$Depth), max(depth_freq$Depth), length=60)

## plot curve with raw depth axis
plot(xfit_r, yfit, axes=FALSE, xlab='', ylab='', type='l', col='', main = "" )
axis(1, at=pretty(xfit_r))
par(new=TRUE)
#plot the line with no axes or labels
plot(xfit, yfit, axes=FALSE, xlab='Depth (cm)', ylab='Probability', type='l', col='red', main = "Spawning/Depth: Probability curve" )

#add these now with axis
# ## get quantiles of yfit (probability)
# quants <- as.vector(quantile(yfit))
axis(2, at=pretty(range(yfit)), labels=c("", 0.1, "", 0.2,"", 0.3, "", 0.4), las=2)
#





       