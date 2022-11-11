### depth utilization

## use depth utilization data to produce response curve depth vs availability
## show how valuable the deeper depths are

# workflow
# get utilization data
# combine with abundance data
# get proportions
# find model to produce curve

## packages
library(tidyverse)
library(dplyr)
library(sm)

setwd("/Users/katieirving/Documents/git/flow_eco_mech")


## abundance data
ad_depth_con <- read.csv("output_data/05a_wulff_depth_2016_2016.csv")
# ad_depth_cat <- read.csv("output_data/05a_adult_depth_categorical.csv")

## habitat availability data - changed 1 value from 77 to 0.77 (2016 data)
av_2015 <- read.csv("input_data/SAR 2015 Microhabitat Availability.csv")
av_2016 <- read.csv("input_data/SAR 2016 Microhabitat Availability Data.csv")

## select depth and velocity rows
av_2015 <- av_2015 %>% select(Depth..cm., Vel..0.6..m.s) %>% 
  rename(Depth = Depth..cm., 
         Velocity = Vel..0.6..m.s)


av_2016 <- av_2016 %>% select(Depth..cm., Vel..0.6..m.s) %>% 
  rename(Depth = Depth..cm., 
         Velocity = Vel..0.6..m.s)

## combine years
available_habitat <- rbind(av_2015, av_2016)
available_habitat$Type <- "Habitat Availability"
# head(available_habitat)
## separate variables 
available_depth <- available_habitat[, c(1,3)]
## for some reason the habitat availability does not include 120cm, but if some fish were found there it has to be available
## am i missing something?
available_depth[2042,] <- c(as.numeric(as.character(120)), "Habitat Availability")
available_depth$Depth <- as.numeric(as.character(available_depth$Depth))
str(available_depth)

available_velocity <- available_habitat[, c(2,3)]
str(available_velocity)
sum(is.na(available_velocity))
available_velocity <- na.omit(available_velocity)
available_velocity$Velocity <- as.numeric(as.character(available_velocity$Velocity))

range(available_velocity$Velocity) ## some negative values???? change to 0? -0.18  1.72

# depth -------------------------------------------------------------------

## combine fish data with habitat data

head(available_depth)
head(ad_depth_con)

fish <- filter(ad_depth_con, Dataset == "Wulff")

fish_freq <- fish %>% 
  uncount(Abundance)

hist(fish_freq$Depth)
dim(fish_freq) ## 814
dim(available_depth) ## 2041

## histogram both together
names(fish_freq)
names(available_depth)
## combine both datasets
fish_freq$Type <- "Fish Abundance"
fish_freq <- fish_freq[c(2,5)]

dat <- rbind(available_depth, fish_freq)

## plot both histograms 
ggplot(dat, aes(Depth, fill=Type, colour=Type)) +
  geom_histogram(aes(y=..density..), breaks=seq(0,120,5), alpha=0.6, 
                 position="identity", lwd=0.2) +
  ggtitle("Adult Depth Utilization")

## plot y a=xis as percentage
library(scales) # For percent_format()

ggplot(dat, aes(Depth, fill=Type, colour=Type)) +
  geom_histogram(aes(y=2*(..density..)/sum(..density..)), breaks=seq(0,120,5), alpha=0.6, 
                 position="identity", lwd=0.2) +
  scale_y_continuous(labels=percent_format()) +
  ylab("Percentage") +
  ggtitle("Adult Depth Utilization")

## proportions
## use proportional info to make a model

## use habitat avilability as a boundary
## probability density of fish within that boundary

## first - scatter plot - depth fish, depth available
range(fish_freq$Depth)
range(available_depth$Depth)
dim(available_depth)


## get proportions and out into dataframe
f <- hist(fish_freq$Depth, breaks=30)
f$counts=(f$counts/sum(f$counts))*100
df <- as.data.frame(matrix(ncol=2, nrow=length(f$counts)))
df[,1] <- f$counts
df[,2] <- f$mids
fish_stuff <- df %>% 
  rename(proportion = V1,
         habitat_mid = V2)

h <- hist(available_depth$Depth, breaks=30)
h$counts=(h$counts/sum(h$counts))*100
df <- as.data.frame(matrix(ncol=2, nrow=length(h$counts)))
df[,1] <- h$counts
df[,2] <- h$mids
habitat_stuff <- df %>% 
  rename(proportion = V1,
         habitat_mid = V2)

all_stuff <- left_join(habitat_stuff, fish_stuff, by="habitat_mid") 
  
all_stuff <- rename(all_stuff, habitat_prop = proportion.x,
         fish_prob = proportion.y)
all_stuff

## replace NAs with zeros
all_stuff[is.na(all_stuff)]<- 0
all_stuff

plot(all_stuff$fish_prob,all_stuff$habitat_prop)
abline(coef = c(0,0.585))


# Ivlevs eletivity index (Jacobs 1974)

## Ei = (Ui - Ai)/(Ui + Ai)

# electivity value ranging from -1.0 (never used) to +1.0 (exclusively
# used), with 0.0 representing habitat used in proportion to its availability

ui <-all_stuff$fish_prob # proportion of fish 
ai <- all_stuff$habitat_prop # proportion of habitat      

a <- ui-ai
b <- ui + ai

ei <- a/b
ei

ei[is.na(ei)]<- 0

all_stuff$Ivlevs_index <- ei
all_stuff

plot(all_stuff$habitat_mid, all_stuff$Ivlevs_index, ylab="Ivlevs Electivity Index", xlab="Depth (cm)")

## use eletivity values and midpoints in a model


## turn into presence/absence based on ivlev values - 0.0 = habitat used in proportion
## zero ivlevs is where no depth or fish were measured. removed for model


all_stuff$Ivlevs00 <- ifelse(all_stuff$Ivlevs_index >=0.0, 1,0)
all_stuff$Ivlevs00

all_stuff <- subset(all_stuff, !Ivlevs_index == 0)
# which(all_stuff$Ivlevs_index==0)


## model glm
library(mgcv)
Ivlevs00_glm <- glm(Ivlevs00~habitat_mid, data=all_stuff,family=binomial(link = "logit"))
summary(Ivlevs00_glm) # p value 0.4

Ivlevs00_gam <- gam(Ivlevs00~s(habitat_mid), family=binomial, data=all_stuff)
summary(Ivlevs00_gam)
mean(summary(Ivlevs00_gam)$s.table[,4]) ## 0.6296681 , 0.02040539

xdepth  <- seq(0,120, 0.1)

AIC(Ivlevs00_gam,Ivlevs00_glm)

1-(Ivlevs00_glm$deviance/Ivlevs00_glm$null.deviance) ## 21.1%

ydepth <- predict(Ivlevs00_glm, list(habitat_mid = xdepth), type="response")
plot(all_stuff$habitat_mid, all_stuff$Ivlevs00, pch = 16, xlab = "Depth (cm)", ylab = "Probability of occurence")
lines(xdepth, ydepth) ## need to force through 0

1-(Ivlevs00_gam$deviance/Ivlevs00_gam$null.deviance) ## 79.6%

ydepth <- predict(Ivlevs00_gam, list(habitat_mid = xdepth), type="response")
plot(all_stuff$habitat_mid, all_stuff$Ivlevs00, pch = 16, xlab = "Depth (cm)", ylab = "Probability of occurence")
lines(xdepth, ydepth) ## need to force through 0

# testing thresholds ------------------------------------------------------


### - 0.5 almost never used (-1.0 = never used)
all_stuff$Ivlevs05 <- ifelse(all_stuff$Ivlevs_index >=-0.5, 1,0)
all_stuff$Ivlevs05

## model glm
Ivlevs05_glm <- glm(Ivlevs05~habitat_mid, data=all_stuff,family=binomial(link = "logit"))
summary(Ivlevs05_glm)

xdepth  <- seq(0,120, 0.1)

ydepth <- predict(Ivlevs05_glm, list(habitat_mid = xdepth), type="response")
plot(all_stuff$habitat_mid, all_stuff$Ivlevs05, pch = 16, xlab = "Depth (cm)", ylab = "Presence")
lines(xdepth, ydepth) ## need to force through 0

###  0.5 almost always used (1.0 = always used)
all_stuff$Ivlevsp05 <- ifelse(all_stuff$Ivlevs_index >=0.5, 1,0)
all_stuff$Ivlevsp05

## model glm
Ivlevsp05_glm <- glm(Ivlevsp05~habitat_mid, data=all_stuff,family=binomial(link = "logit"))
summary(Ivlevsp05_glm)

xdepth  <- seq(0,120, 0.1)

ydepth <- predict(Ivlevsp05_glm, list(habitat_mid = xdepth), type="response")
plot(all_stuff$habitat_mid, all_stuff$Ivlevsp05, pch = 16, xlab = "Depth (cm)", ylab = "Presence")
lines(xdepth, ydepth) ## need to force through 0



# Velocity ----------------------------------------------------------------

head(available_velocity) ## habitat data
ad_vel_con <- read.csv("output_data/05a_velocity_wulff_2015_2016_both_vels.csv") ## fish abundacne data
head(ad_vel_con)

# fish <- filter(ad_vel_con, Dataset == "Wulff")

fish <- ad_vel_con %>%
  select(Vel_0.6, Abundance, year) %>%
  rename(Velocity = Vel_0.6)

sum(is.na(fish)) ## 107
fish <- na.omit(fish)

fish_freq <- fish %>% 
  uncount(Abundance)

hist(fish_freq$Velocity)
dim(fish_freq) ## 517 - remove 2017
dim(available_velocity) ## 2036

## histogram both together

## combine both datasets
head(fish_freq)
fish_freq$Type <- "Fish Abundance"
fish_freq <- fish_freq[c(1,3)]

dat <- rbind(available_velocity, fish_freq)
range(dat$Velocity)

## plot both histograms 
ggplot(dat, aes(Velocity, fill=Type, colour=Type)) +
  geom_histogram(aes(y=..density..), breaks=seq(0,1.8,0.1), alpha=0.6, 
                 position="identity", lwd=0.2) +
  ggtitle("Adult Velocity Utilization")

## plot y a=xis as percentage
library(scales) # For percent_format()

ggplot(dat, aes(Velocity, fill=Type, colour=Type)) +
  geom_histogram(aes(y=2*(..density..)/sum(..density..)), breaks=seq(0,1.8,0.1), alpha=0.6, 
                 position="identity", lwd=0.2) +
  scale_y_continuous(labels=percent_format()) +
  ylab("Percentage") +
  ggtitle("Adult Velocity Utilization")

## proportions
## use proportional info to make a model

## use habitat avilability as a boundary
## probability density of fish within that boundary

## first - scatter plot - depth fish, depth available

## get proportions and out into dataframe
f <- hist(fish_freq$Velocity, breaks=30)
f$counts=(f$counts/sum(f$counts))*100
df <- as.data.frame(matrix(ncol=2, nrow=length(f$counts)))
df[,1] <- f$counts
df[,2] <- round(f$mids, digits=3)
fish_stuff <- df %>% 
  rename(proportion = V1,
         habitat_mid = V2)


h <- hist(available_velocity$Velocity, breaks=30)
h$counts=(h$counts/sum(h$counts))*100
df <- as.data.frame(matrix(ncol=2, nrow=length(h$counts)))
df[,1] <- h$counts
df[,2] <- round(h$mids, digits = 3)
habitat_stuff <- df %>% 
  rename(proportion = V1,
         habitat_mid = V2)


all_stuff <- full_join(habitat_stuff, fish_stuff, by="habitat_mid") 

all_stuff <- rename(all_stuff, habitat_prop = proportion.x,
                    fish_prob = proportion.y)

all_stuff
## replace NAs with zeros
all_stuff[is.na(all_stuff)]<- 0
all_stuff

plot(all_stuff$fish_prob,all_stuff$habitat_prop)
abline(coef = c(0,0.585))



# Ivlevs electivity index (Jacobs 1974, 1961)

## Ei = (Ui - Ai)/(Ui + Ai)

# electivity value ranging from -1.0 (never used) to +1.0 (exclusively
# used), with 0.0 representing habitat used in proportion to its availability

ui <-all_stuff$fish_prob # proportion of fish 
ai <- all_stuff$habitat_prop # proportion of habitat      

a <- ui-ai
b <- ui + ai

ei <- a/b
ei

ei[is.na(ei)]<- 0

all_stuff$Ivlevs_index <- ei
all_stuff

plot(all_stuff$habitat_mid, all_stuff$Ivlevs_index, ylab="Ivlevs Electivity Index", xlab="Velocity (m/s)")

## use electivity values and midpoints in a model

## turn into presence/absence based on ivlev values - 0.0 = habitat used in proportion

all_stuff$Ivlevs00 <- ifelse(all_stuff$Ivlevs_index >=0.0, 1,0)
all_stuff$Ivlevs00

all_stuff <- subset(all_stuff, !Ivlevs_index == 0)
# which(all_stuff$Ivlevs_index==0)

## model quadratic
library(mgcv)

# fit2<-lm(Freq~poly(Depth,2,raw=TRUE), data=freq_df)
Ivlevs00_lm <- lm(Ivlevs00~habitat_mid, data=all_stuff)
Ivlevs00_glm <- glm(Ivlevs00~habitat_mid, data=all_stuff,family=binomial(link = "logit"))
Ivlevs00_gam <- gam(Ivlevs00~s(habitat_mid), family=binomial, data=all_stuff)
Ivlevs00_quad <- lm(Ivlevs00 ~poly(habitat_mid, 2, raw=T), data=all_stuff)
summary(Ivlevs00_gam) ## most explained variance 44%
mean(summary(Ivlevs00_gam)$s.table[,4]) ## 0.02040539
summary(Ivlevs00_lm)
summary(Ivlevs00_quad)
summary(Ivlevs00_glm)

AIC(Ivlevs00_gam,Ivlevs00_quad)

1-(Ivlevs00_glm$deviance/Ivlevs00_glm$null.deviance) ## 17.1%
xvel  <- seq(0,2, 0.01)

yvel <- predict(Ivlevs00_gam, list(habitat_mid = xvel), type="response")
plot(all_stuff$habitat_mid, all_stuff$Ivlevs00, pch = 16, xlab = "Velocity (m/s)", ylab = "Probability of occurrence")
lines(xvel, yvel) ## need to force through 0

# yvel <- predict(Ivlevs00_lm, list(habitat_mid = xvel), type="response")
# plot(all_stuff$habitat_mid, all_stuff$Ivlevs00, pch = 16, xlab = "Velocity (m/s)", ylab = "Probability of occurrence")
# lines(xvel, yvel) ## need to force through 0
# 
# yvel <- predict(Ivlevs00_quad, list(habitat_mid = xvel), type="response")
# plot(all_stuff$habitat_mid, all_stuff$Ivlevs00, pch = 16, xlab = "Velocity (m/s)", ylab = "Probability of occurence")
# lines(xvel, yvel) ## need to force through 0
# 
# yvel <- predict(Ivlevs00_glm, list(habitat_mid = xvel), type="response")
# plot(all_stuff$habitat_mid, all_stuff$Ivlevs00, pch = 16, xlab = "Velocity (m/s)", ylab = "Probability of occurence")
# lines(xvel, yvel) ## need to force through 0

