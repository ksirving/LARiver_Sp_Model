### turning density/ frequency into logistical model/curve

library(tidyverse)
library(dplyr)
library(sm)

setwd("/Users/katieirving/Documents/git/flow_eco_mech")


### upload data
## depth
ad_depth_con <- read.csv("output_data/05a_adult_depth_continuous.csv")
ad_depth_cat <- read.csv("output_data/05a_adult_depth_categorical.csv")
juv_depth_con <- read.csv("output_data/05a_juvenile_depth_continuous.csv")
juv_depth_cat <- read.csv("output_data/05a_juvenile_depth_categorical.csv")
sp_depth_cat <- read.csv("output_data/05a_spawning_depth_continuous.csv")

## temperature
ad_temp_con <- read.csv("output_data/05a_adult_temperature_continuous.csv")
juv_temp_con <- read.csv("output_data/05a_juvenile_temperature_continuous.csv")
sp_temp_con <- read.csv("output_data/05a_spawning_temperature_continuous.csv")

## velocity
ad_vel_con <- read.csv("output_data/05a_adult_velocity_continuous.csv")
ad_vel_cat <- read.csv("output_data/05a_adult_velocity_categorical.csv")
juv_vel_con <- read.csv("output_data/05a_juvenile_velocity_continuous.csv")
juv_vel_cat <- read.csv("output_data/05a_juvenile_velocity_categorical.csv")
sp_vel_cat <- read.csv("output_data/05a_spawning_velocity_continuous.csv")


##### get histogram and density data frames

## depth - adult

ad_depth_con <- read.csv("output_data/05a_adult_depth_continuous.csv")
ad_depth_cat <- read.csv("output_data/05a_adult_depth_categorical.csv")

## combine data
all_depth <- rbind(ad_depth_con, ad_depth_cat)

## uncount data into frequency

depth_freq <- all_depth %>% 
  uncount(Abundance)
hist(depth_freq$Depth)



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

### get density data

d_data <- density(depth_freq$Depth)

dens_df <- data.frame(matrix(nrow=length(d_data$y)))
dens_df[1] <- d_data$y
dens_df$Depth <- d_data$x
colnames(dens_df)[1] <- "density"
dens_df ## rows 1:22 are negative????

plot(dens_df$Depth, dens_df$density)   


### logistic curve
## binary column for 0.010 density

dens_df$dens_0.01 <- ifelse(dens_df$density>=0.010, 1,0)
dens_df$dens_0.01

dens_df$dens_0.015 <- ifelse(dens_df$density>=0.015, 1,0)
dens_df$dens_0.015

dens_df$dens_0.02 <- ifelse(dens_df$density>=0.020, 1,0)
dens_df$dens_0.02

dens_df$dens_0.025 <- ifelse(dens_df$density>=0.025, 1,0)
dens_df$dens_0.025

dens_df$dens_0.03 <- ifelse(dens_df$density>=0.030, 1,0)
dens_df$dens_0.03

## model
dens01_glm <- glm(dens_0.01~Depth, data=dens_df,family=binomial(link = "logit"))
summary(dens01_glm)

dens015_glm <- glm(dens_0.015~Depth, data=dens_df,family=binomial(link = "logit"))
summary(dens015_glm)

dens02_glm <- glm(dens_0.02~Depth, data=dens_df,family=binomial(link = "logit"))
summary(dens02_glm)

dens025_glm <- glm(dens_0.025~Depth, data=dens_df,family=binomial(link = "logit"))
summary(dens025_glm)

dens03_glm <- glm(dens_0.03~Depth, data=dens_df,family=binomial(link = "logit"))
summary(dens03_glm) ## best model
mean(dens_df$Depth)
median(dens_df$Depth)
head(dens_df)
# create range of depth values

xdepth  <- seq(0,130, 0.1)
## prediction
ydepth <- predict(dens01_glm, list(Depth = xdepth), type="response")
plot(xdepth, ydepth, pch = 16, xlab = "Depth (m)", ylab = "Presence")
lines(xdepth, ydepth) ## need to force through 0
xdepth
ydepth <- predict(dens015_glm, list(Depth = xdepth), type="response")
plot(dens_df$Depth, dens_df$dens_0.015, pch = 16, xlab = "Depth (m)", ylab = "Presence")
lines(xdepth, ydepth) ## need to force through 0

ydepth <- predict(dens02_glm, list(Depth = xdepth), type="response")
plot(dens_df$Depth, dens_df$dens_0.02, pch = 16, xlab = "Depth (m)", ylab = "Presence")
lines(xdepth, ydepth) ## need to force through 0

ydepth <- predict(dens025_glm, list(Depth = xdepth), type="response")
plot(dens_df$Depth, dens_df$dens_0.025, pch = 16, xlab = "Depth (m)", ylab = "Presence")
lines(xdepth, ydepth) ## need to force through 0

ydepth <- predict(dens03_glm, list(Depth = xdepth), type="response")
plot(dens_df$Depth, dens_df$dens_0.03, pch = 16, xlab = "Depth (m)", ylab = "Presence")
lines(xdepth, ydepth) ## need to force through 0

### quadratic curve on frquencies
head(freq_df)
plot(freq_df$Depth, freq_df$Freq)

fit2<-lm(Freq~poly(Depth,2,raw=TRUE), data=freq_df)

summary(fit2)
range(juv_temp_con$Temp)

pred_depth <- seq(1, 130 , 0.1)
predictedcounts <- predict(fit2,list(Depth=pred_depth, Depth2=pred_depth^2))
plot(freq_df$Depth, freq_df$Freq, pch=16, xlab = "Depth (cm)", 
     ylab = "Abundance", cex.lab = 1.3, col = "blue")
lines(pred_temp, predictedcounts, col = "darkgreen", lwd = 3)

### binary column for 300 freq

## frequency column
#### histogram
x <-depth_freq$Depth
h<-hist(x)
xfit<-seq(min(x),max(x),length=130)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)

xfit ## depths
yfit ## frequencies


freq_df <- cbind(xfit, yfit)

freq_df <- as.data.frame(freq_df)
names(freq_df) <- c("Depth", "Freq")
str(freq_df)
max(freq_df$Freq)


freq_df$f100 <- ifelse(freq_df$Freq >=100, 1,0)
freq_df$f100

freq_df$f200 <- ifelse(freq_df$Freq >= 200, 1,0)
freq_df$f200

freq_df$f300 <- ifelse(freq_df$Freq  >= 300, 1,0)
freq_df$f300

freq_df$f400 <- ifelse(freq_df$Freq >= 400, 1,0)
freq_df$f400

freq_df$f500 <- ifelse(freq_df$Freq >= 500, 1,0)
freq_df$f500


## model glm
freq300_glm <- glm(f300~Depth, data=freq_df,family=binomial(link = "logit"))
summary(freq300_glm)

freq200_glm <- glm(f200~Depth, data=freq_df,family=binomial(link = "logit"))
summary(freq200_glm)

freq100_glm <- glm(f100~Depth, data=freq_df,family=binomial(link = "logit"))
summary(freq100_glm)

freq400_glm <- glm(f400~Depth, data=freq_df,family=binomial(link = "logit"))
summary(freq400_glm)

freq500_glm <- glm(f500~Depth, data=freq_df,family=binomial(link = "logit"))
summary(freq500_glm)

# create range of depth values

xdepth  <- seq(0,130, 0.1)
## prediction


ydepth <- predict(freq100_glm, list(Depth = xdepth), type="response")
plot(freq_df$Depth, freq_df$f100, pch = 16, xlab = "Depth (m)", ylab = "Presence")
lines(xdepth, ydepth) ## need to force through 0

ydepth <- predict(freq200_glm, list(Depth = xdepth), type="response")
plot(freq_df$Depth, freq_df$f200, pch = 16, xlab = "Depth (m)", ylab = "Presence")
lines(xdepth, ydepth) ## need to force through 0

ydepth <- predict(freq300_glm, list(Depth = xdepth), type="response")
plot(freq_df$Depth, freq_df$f300, pch = 16, xlab = "Depth (m)", ylab = "Presence")
lines(xdepth, ydepth) ## need to force through 0

ydepth <- predict(freq400_glm, list(Depth = xdepth), type="response")
plot(freq_df$Depth, freq_df$f400, pch = 16, xlab = "Depth (m)", ylab = "Presence")
lines(xdepth, ydepth) ## need to force through 0

ydepth <- predict(freq500_glm, list(Depth = xdepth), type="response")
plot(freq_df$Depth, freq_df$f500, pch = 16, xlab = "Depth (m)", ylab = "Presence")
lines(xdepth, ydepth) ## need to force through 0

### models gam
install.packages("mgcv")
library(mgcv)

freq100_gam <- gam(f100~s(Depth), data=freq_df,family=binomial(link = "logit"))
summary(freq300_gam)

freq200_gam <- gam(f200~s(Depth), data=freq_df,family=binomial(link = "logit"))
summary(freq200_gam)

freq300_gam <- gam(f300~s(Depth), data=freq_df,family=binomial(link = "logit"))
summary(freq300_gam)

freq400_gam <- gam(f400~s(Depth), data=freq_df,family=binomial(link = "logit"))
summary(freq400_gam)

freq500_gam <- gam(f500~s(Depth), data=freq_df,family=binomial(link = "logit"))
summary(freq500_gam)

# create range of depth values

xdepth  <- seq(0,130, 0.1)
## prediction


ydepth <- predict(freq100_gam, list(Depth = xdepth), type="response")
plot(freq_df$Depth, freq_df$f100, pch = 16, xlab = "Depth (m)", ylab = "Presence")
lines(xdepth, ydepth) ## need to force through 0

ydepth <- predict(freq200_gam, list(Depth = xdepth), type="response")
plot(freq_df$Depth, freq_df$f200, pch = 16, xlab = "Depth (m)", ylab = "Presence")
lines(xdepth, ydepth) ## need to force through 0

ydepth <- predict(freq300_gam, list(Depth = xdepth), type="response")
plot(freq_df$Depth, freq_df$f300, pch = 16, xlab = "Depth (m)", ylab = "Presence")
lines(xdepth, ydepth) ## need to force through 0

ydepth <- predict(freq400_gam, list(Depth = xdepth), type="response")
plot(freq_df$Depth, freq_df$f400, pch = 16, xlab = "Depth (m)", ylab = "Presence")
lines(xdepth, ydepth) ## need to force through 0

ydepth <- predict(freq500_gam, list(Depth = xdepth), type="response")
plot(freq_df$Depth, freq_df$f500, pch = 16, xlab = "Depth (m)", ylab = "Presence")
lines(xdepth, ydepth) ## need to force through 0

library(ggplot2)

ggplot(freq_df, aes(x, y)) + geom_point() + geom_smooth(method = "gam", formula = freq_df$f300 ~s(freq_df$Depth))

## gam on freq
head(freq_df)

test <- gam(Freq~Depth, data=freq_df)
summary(test)

ydepth <- predict(test, list(Depth = xdepth), type="response")
plot(freq_df$Depth, freq_df$freq, pch = 16, xlab = "Depth (m)", ylab = "Presence")
lines(xdepth, ydepth) ## need to force through 0


#### zero intercept

freq300_glm_zero <- glm(f300~Depth-1, data=freq_df, family=binomial)

ydepthz <- predict(freq300_glm_zero, list(Depth = xdepth), type="response")
plot(freq_df$Depth, freq_df$f300, pch = 16, xlab = "Depth (m)", ylab = "Presence")
lines(xdepth, ydepthz) ## doesn't work!!!!!
length(xdepth)
length(ydepthz)


### centre and scale





