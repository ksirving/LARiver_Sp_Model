### probabilistic curves

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
ad_temp_con <- read.csv("output_data/05a_adult_temperature_continous.csv")
juv_temp_con <- read.csv("output_data/05a_juvenile_temperature_continous.csv")
sp_temp_con <- read.csv("output_data/05a_spawning_temperature_continuous.csv")

## velocity
ad_vel_con <- read.csv("output_data/05a_adult_velocity_continuous.csv")
ad_vel_cat <- read.csv("output_data/05a_adult_velocity_categorical.csv")
juv_vel_con <- read.csv("output_data/05a_juvenile_velocity_continuous.csv")
juv_vel_cat <- read.csv("output_data/05a_juvenile_velocity_categorical.csv")
sp_vel_cat <- read.csv("output_data/05a_spawning_velocity_continuous.csv")

## depth - adult

ad_depth_con <- read.csv("output_data/05a_adult_depth_continuous.csv")
ad_depth_cat <- read.csv("output_data/05a_adult_depth_categorical.csv")

all_depth <- rbind(ad_depth_con, ad_depth_cat)

ad_depth_con
plot(ad_depth_con$Depth, ad_depth_con$Abundance, col=ad_depth_con$Dataset)

summary(lm(Abundance~Depth, data=ad_depth_con))
hist(ad_depth_con$Depth)

depth_freq_con <- ad_depth_con %>%
  uncount(Abundance)
hist(depth_freq_con$Depth)

## need to uncount data into frequency

depth_freq <- ad_depth_cat %>% 
  uncount(Abundance)
hist(depth_freq$Depth)
depth_freq

## add data type column

depth_freq_con$Data_Type <- "continuous"
depth_freq$Data_Type <- "categorical"
depth_freq_con$Data_Type_num <- 1
depth_freq$Data_Type_num <- 2

## add together to check histogram
all_data_freq <- rbind(depth_freq, depth_freq_con)
dim(all_data_freq)
all_data_freq$Depth <- as.numeric(as.character(all_data_freq$Depth))
hist(all_data_freq$Depth) ## 71 category 
str(all_data_freq)

############## models

## get probability from histogram
## all data
## includes 71 category and buffers the influence

## test with r tutorial

x <-all_data_freq$Depth
h<-hist(x, breaks=10,lty="white", xlab="Depth (cm)",
        main="Adult/Depth: Curve")
xfit<-seq(min(x),max(x),length=130)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit[1]
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="red", lwd=2)
plot(density(all_data_freq$Depth))
pnorm(1, mean=mean(x),sd=sd(x))
dnorm(50, mean=mean(x),sd=sd(x))

## compare different data sets
attach(all_data_freq)

# create value labels
data.f <- factor(Data_Type_num, levels= 1:2,
                labels = c("Continuous", "Categorical"))
data.f
str(Depth)
as.vector(Depth)
# plot densities
sm.density.compare(as.vector(Depth), Data_Type_num, xlab="Depth (cm)")
title(main="Depth Distribution by Data Type")

# add legend via mouse click
colfill<-c(2:(2+length(levels(data.f))))
legend(locator(1), levels(data.f), fill=colfill)


## categorical

plot(depth_freq$Depth, depth_freq$Abundance) ## many 1's skewing the curve
str(all_df)
## normal dist 
x <-depth_freq$Depth
h<-hist(x, breaks=10, col="red", xlab="Depth (cm)",
        main="Histogram with Normal Curve")
xfit<-seq(min(x),max(x),length=130)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)

plot(density(depth_freq$Depth))
######## continuous

## normal dist 
x <-depth_freq_con$Depth
h<-hist(x, breaks=10, col="red", xlab="Depth (cm)",
        main="Histogram with Normal Curve")
xfit<-seq(min(x),max(x),length=130)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)

plot(density(depth_freq_con$Depth))

### best all together but will need some form of normalisation
x <-all_data_freq$Depth
h<-hist(x, breaks=10, col="red", xlab="Depth (cm)",
        main="Histogram with Normal Curve")
xfit<-seq(min(x),max(x),length=130)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)
xfit

plot(density(all_data_freq$Depth))#;abline(v=50, col = 2)
  
min(all_data_freq$Depth)
### probabilities
d <- seq(1,130,1)
pfit <- dnorm(d, mean=mean(x),sd=sd(x))
str(pfit)
max(pfit)
pfitx <- unlist(pfit)
length(pfitx)
pfit_df <- data.frame(matrix(nrow=130, ncol=2))
pfit_df[,1] <- pfitx
pfit_df[,2] <- seq(1,130,1)
colnames(pfit_df) <- c("density", "Depth")
pfit_df

# density curve
plot(pfit_df$Depth, pfit_df$density, xlab="Depth (cm)", ylab= "Probability Density")


### get cumulative probability

### best all together but will need some form of normalisation
x <-all_data_freq$Depth
h<-hist(x, breaks=10, col="red", xlab="Depth (cm)",
        main="Histogram with Normal Curve")
xfit<-seq(min(x),max(x),length=130)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)
xfit

plot(density(all_data_freq$Depth))#;abline(v=50, col = 2)

min(all_data_freq$Depth)
### probabilities
d <- seq(1,130,1)
pfit <- pnorm(d, mean=mean(x),sd=sd(x))
str(pfit)
max(pfit)
pfitx <- unlist(pfit)
length(pfitx)
pfit_df <- data.frame(matrix(nrow=130, ncol=2))
pfit_df[,1] <- pfitx
pfit_df[,2] <- seq(1,130,1)
colnames(pfit_df) <- c("density", "Depth")
pfit_df

## cumulative
plot(pfit_df$Depth, pfit_df$density, xlab="Depth (cm)", ylab= "Probability")

## get probability density curve

x <-all_data_freq$Depth

plot(pfit_df$Depth, pfit_df$density, xlab="Depth (cm)", ylab= "Probability")

## get actual probability
 
?hist
h<-hist(x, xlab="Depth (cm)",col="white",lty="blank",
        main="Histogram with Normal Curve")
xfit<-seq(min(x),max(x),length=130)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
yfit

ub <- 50
lb <- 20
mean=mean(x)
sd=sd(x)

i <- xfit >= lb & xfit <= ub
lines(xfit, yfit)
polygon(c(lb,xfit[i],ub), c(0,yfit[i],0), col="blue")

area <- pnorm(ub, mean, sd) - pnorm(lb, mean, sd)
result <- paste("P(",lb,"< Depth <",ub,") =",
                signif(area, digits=3))
mtext(result,3) ## 0.585
# axis(1, at=seq(0, 120, 20), pos=0)

mean=mean(x)
sd=sd(x)
h<-hist(x, xlab="Depth (cm)",col="white",lty="blank",
        main="Histogram with Normal Curve")
xfit<-seq(min(x),max(x),length=130)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
yfit


lb <- 5
ub <- 20

i <- xfit >= lb & xfit <= ub
lines(xfit, yfit)
polygon(c(lb,xfit[i],ub), c(0,yfit[i],0), col="red")

area <- pnorm(ub, mean, sd) - pnorm(lb, mean, sd)
result <- paste("P(",lb,"< Depth <",ub,") =",
                signif(area, digits=3))
mtext(result,3) ## 0.17

h<-hist(x, xlab="Depth (cm)",col="white",lty="blank",
        main="Histogram with Normal Curve")
xfit<-seq(min(x),max(x),length=130)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
yfit


lb <- 50
ub <- 80

i <- xfit >= lb & xfit <= ub
lines(xfit, yfit)
polygon(c(lb,xfit[i],ub), c(0,yfit[i],0), col="red")

area <- pnorm(ub, mean, sd) - pnorm(lb, mean, sd)
result <- paste("P(",lb,"< Depth <",ub,") =",
                signif(area, digits=3))
mtext(result,3) # 0.18


## probabilities of abundance at a certain depth
tfit <- pnorm(90, mean=mean(x),sd=sd(x))
tfit ## 0.8142976
## depth at certain probabilities
qfit50 <- qnorm(0.5,mean=mean(x),sd=sd(x))
qfit90 <- qnorm(0.9, mean=mean(x),sd=sd(x))
qfit50

qfit90

## fit prob line at 0.5
# relate probilities to depth and density

## density data
d_data <- density(all_data_freq$Depth)

dens_df <- data.frame(matrix(nrow=length(d_data$y)))
dens_df[1] <- d_data$y
dens_df$Depth <- d_data$x
colnames(dens_df)[1] <- "density"
dens_df

dens_50 <- subset(dens_df, Depth>qfit50)
dens_50[1,1]

line05 <- dens_50[1,1] ## 0.5 line
line05
dens_90 <- subset(dens_df, Depth>qfit90)
line09 <- dens_90[1,1]
line09
plot(density(all_data_freq$Depth))
abline(line05, 0)
abline(line09,0)

## same line on histogram

x <-all_data_freq$Depth
h<-hist(x, breaks=10, col="red", xlab="Depth (cm)",
        main="Histogram with Normal Curve")
xfit<-seq(min(x),max(x),length=130)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)

hist_df <- data.frame(matrix(nrow=length(yfit)))
colnames(hist_df)<- "freq"
hist_df$depth <- xfit
hist_df$freq <- yfit

hist_50 <- subset(hist_df, Depth>qfit50)
hist_50[1,1]

hist05 <- hist_50[1,1] ## 0.5 line
hist05
hist_90 <- subset(hist_df, Depth>qfit90)
hist09 <- hist_90[1,1]
hist09

abline(hist05,0)
abline(hist09,0)

#####################################################

## quadratic curve
## use abundance data

head(all_depth)

plot(all_depth$Depth, all_depth$Abundance)

fit2<-lm(Abundance~poly(Depth,2,raw=TRUE), data=all_depth)

summary(fit2)

pred_depth <- seq(0, 130, 1)
predictedcounts <- predict(fit2,list(Depth=pred_depth, Depth2=pred_depth^2))
plot(all_depth$Depth, all_depth$Abundance, pch=16, xlab = "Depth (cm)", 
     ylab = "Abundance", cex.lab = 1.3, col = "blue")
lines(pred_depth, predictedcounts, col = "darkgreen", lwd = 3)


## skewed due to all the 1s

one_depth <- subset(all_depth, Abundance>1)
dim(one_depth)

## remove categorised and mean data - worse
unique(all_depth$Dataset)[c(2,5)]
data_out <- droplevels(unique(all_depth$Dataset)[c(2,5)])
data_out
some_depth <- subset(all_depth, !Dataset %in% data_out)
some_depth

## try model again
plot(some_depth$Depth, some_depth$Abundance)

Depth2 <- all_depth$Depth^2

fit2<-lm(Abundance~poly(Depth,2,raw=TRUE), data=all_depth)


pred_depth <- seq(0, 130, 1)
predictedcounts <- predict(fit2,list(Depth=pred_depth, Depth2=pred_depth^2))
plot(some_depth$Depth, some_depth$Abundance, pch=16, xlab = "Depth (cm)", 
     ylab = "Abundance", cex.lab = 1.3, col = "blue")
lines(pred_depth, predictedcounts, col = "darkgreen", lwd = 3)
## skewed due to all the 1s

one_depth <- subset(all_depth, Abundance>1)
dim(one_depth)

#### log 

log(all_depth$Abundance)+1

all_depth$log_abundance <- log(all_depth$Abundance)+1

plot(all_depth$Depth, all_depth$Abundance)

fit2<-lm(log(Abundance+1)~poly(Depth,2,raw=TRUE), data=all_depth)

summary(fit2)

pred_depth <- seq(0, 130, 1)
predictedcounts <- predict(fit2,list(Depth=pred_depth, Depth2=pred_depth^2))
plot(all_depth$Depth, log(all_depth$Abundance+1), pch=16, xlab = "Depth (cm)", 
     ylab = "Abundance", cex.lab = 1.3, col = "blue")
lines(pred_depth, predictedcounts, col = "darkgreen", lwd = 3)
## skewed due to all the 1s

one_depth <- subset(all_depth, Abundance>1)
dim(one_depth)

### try with just smea

head(ad_depth_cat)

plot(ad_depth_cat$Depth, ad_depth_cat$Abundance)

fit2<-lm(Abundance~poly(Depth,2,raw=TRUE), data=ad_depth_cat)

summary(fit2)

pred_depth <- seq(0, 70, 1)
predictedcounts <- predict(fit2,list(Depth=pred_depth, Depth2=pred_depth^2))
plot(ad_depth_cat$Depth, ad_depth_cat$Abundance, pch=16, xlab = "Depth (cm)", 
     ylab = "Abundance", cex.lab = 1.3, col = "blue")
lines(pred_depth, predictedcounts, col = "darkgreen", lwd = 3)

## smea minus 70 category

ad_depth_catx <- ad_depth_cat[-15,]

head(ad_depth_catx)

plot(ad_depth_catx$Depth, ad_depth_catx$Abundance)

fit2<-lm(Abundance~poly(Depth,2,raw=TRUE), data=ad_depth_catx)

summary(fit2)

pred_depth <- seq(0, 70, 1)
predictedcounts <- predict(fit2,list(Depth=pred_depth, Depth2=pred_depth^2))
plot(ad_depth_catx$Depth, ad_depth_catx$Abundance, pch=16, xlab = "Depth (cm)", 
     ylab = "Abundance", cex.lab = 1.3, col = "blue")
lines(pred_depth, predictedcounts, col = "darkgreen", lwd = 3)


########################################
######################

## juvenile
juv_depth_con <- read.csv("output_data/05a_juvenile_depth_continuous.csv")
juv_depth_cat <- read.csv("output_data/05a_juvenile_depth_categorical.csv")

juv_depth_con
juv_depth_cat


## need to uncount data into frequency

depth_freq <- juv_depth_cat %>% 
  uncount(Abundance)
hist(depth_freq$Depth)
head(depth_freq)

depth_freq_con <- juv_depth_con %>% 
  uncount(Abundance)
hist(depth_freq_con$Depth)
depth_freq_con

## add data type column

depth_freq_con$Data_Type <- "continuous"
depth_freq$Data_Type <- "categorical"
depth_freq_con$Data_Type_num <- 1
depth_freq$Data_Type_num <- 2

## add together to check histogram
all_data_freq <- rbind(depth_freq, depth_freq_con)
dim(all_data_freq)
all_data_freq$Depth <- as.numeric(as.character(all_data_freq$Depth))
hist(all_data_freq$Depth) 
str(all_data_freq)

############## models

## get probability from histogram
## all data
## includes 71 category and buffers the influence

## test with r tutorial

x <-all_data_freq$Depth
h<-hist(x, breaks=10, col="red", xlab="Depth (cm)",
        main="Histogram with Normal Curve")
xfit<-seq(min(x),max(x),length=130)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)
plot(density(all_data_freq$Depth))

## compare different data sets
attach(all_data_freq)

# create value labels
data.f <- factor(Data_Type_num, levels= 1:2,
                 labels = c("Continuous", "Categorical"))
data.f
str(Depth)
as.vector(Depth)
# plot densities
sm.density.compare(as.vector(Depth), Data_Type_num, xlab="Depth (cm)")
title(main="Depth Distribution by Data Type")

# add legend via mouse click
colfill<-c(2:(2+length(levels(data.f))))
legend(locator(1), levels(data.f), fill=colfill)


## categorical

plot(depth_freq$Depth, depth_freq$Abundance) ## many 1's skewing the curve
str(all_df)
## normal dist 
x <-depth_freq$Depth
h<-hist(x, breaks=10, col="red", xlab="Depth (cm)",
        main="Histogram with Normal Curve")
xfit<-seq(min(x),max(x),length=130)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)

plot(density(depth_freq$Depth))
######## continuous

## normal dist 
x <-depth_freq_con$Depth
h<-hist(x, breaks=10, col="red", xlab="Depth (cm)",
        main="Histogram with Normal Curve")
xfit<-seq(min(x),max(x),length=130)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)

plot(density(depth_freq_con$Depth))

### best all together but will need some form of normalisation
x <-all_data_freq$Depth
x
h<-hist(x, breaks=10, col="red", xlab="Depth (cm)",
        main="Histogram with Normal Curve")
xfit<-seq(min(x),max(x),length=70)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)
xfit

plot(density(all_data_freq$Depth))#;abline(v=50, col = 2)

min(all_data_freq$Depth)
### probabilities
d <- seq(1,70,1)
pfit <- dnorm(d, mean=mean(x),sd=sd(x))
str(pfit)
max(pfit)
pfitx <- unlist(pfit)
length(pfitx)
pfit_df <- data.frame(matrix(nrow=70, ncol=2))
pfit_df[,1] <- pfitx
pfit_df[,2] <- seq(1,70,1)
colnames(pfit_df) <- c("density", "Depth")
pfit_df

# density curve
plot(pfit_df$Depth, pfit_df$density, xlab="Depth (cm)", ylab= "Probability Density")


### get cumulative probability

### best all together but will need some form of normalisation
x <-all_data_freq$Depth
h<-hist(x, breaks=10, col="red", xlab="Depth (cm)",
        main="Histogram with Normal Curve")
xfit<-seq(min(x),max(x),length=70)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)
xfit

plot(density(all_data_freq$Depth))#;abline(v=50, col = 2)

min(all_data_freq$Depth)
### probabilities
d <- seq(1,70,1)
pfit <- pnorm(d, mean=mean(x),sd=sd(x))
str(pfit)
max(pfit)
pfitx <- unlist(pfit)
length(pfitx)
pfit_df <- data.frame(matrix(nrow=70, ncol=2))
pfit_df[,1] <- pfitx
pfit_df[,2] <- seq(1,70,1)
colnames(pfit_df) <- c("density", "Depth")
pfit_df
length(x)
## cumulative
plot(pfit_df$Depth, pfit_df$density, xlab="Depth (cm)", ylab= "Probability")

## get probability density curve

x <-all_data_freq$Depth

plot(pfit_df$Depth, pfit_df$density, xlab="Depth (cm)", ylab= "Probability")

## get actual probability

?hist
h<-hist(x, xlab="Depth (cm)",col="white",lty="blank",
        main="Histogram with Normal Curve")
xfit<-seq(min(x),max(x),length=70)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
yfit

ub <- 50
lb <- 20
mean=mean(x)
sd=sd(x)

i <- xfit >= lb & xfit <= ub
lines(xfit, yfit)
polygon(c(lb,xfit[i],ub), c(0,yfit[i],0), col="blue")

area <- pnorm(ub, mean, sd) - pnorm(lb, mean, sd)
result <- paste("P(",lb,"< Depth <",ub,") =",
                signif(area, digits=3))
mtext(result,3) ## 0.69
# axis(1, at=seq(0, 120, 20), pos=0)

# mean=mean(x)
# sd=sd(x)
# h<-hist(x, xlab="Depth (cm)",col="white",lty="blank",
#         main="Histogram with Normal Curve")
# xfit<-seq(min(x),max(x),length=130)
# yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
# yfit <- yfit*diff(h$mids[1:2])*length(x)
# yfit


lb <- 5
ub <- 20

i <- xfit >= lb & xfit <= ub
lines(xfit, yfit)
polygon(c(lb,xfit[i],ub), c(0,yfit[i],0), col="red")

area <- pnorm(ub, mean, sd) - pnorm(lb, mean, sd)
result <- paste("P(",lb,"< Depth <",ub,") =",
                signif(area, digits=3))
result
mtext(result,3) ## 0.11

# h<-hist(x, xlab="Depth (cm)",col="white",lty="blank",
#         main="Histogram with Normal Curve")
# xfit<-seq(min(x),max(x),length=130)
# yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
# yfit <- yfit*diff(h$mids[1:2])*length(x)
# yfit


lb <- 50
ub <- 80

i <- xfit >= lb & xfit <= ub
lines(xfit, yfit)
polygon(c(lb,xfit[i],ub), c(0,yfit[i],0), col="red")

area <- pnorm(ub, mean, sd) - pnorm(lb, mean, sd)
result <- paste("P(",lb,"< Depth <",ub,") =",
                signif(area, digits=3))
area # 0.18
mtext(result,3) # 0.18


## probabilities of abundance at a certain depth
tfit <- pnorm(90, mean=mean(x),sd=sd(x))
tfit ## 0.8142976
## depth at certain probabilities
qfit50 <- qnorm(0.5,mean=mean(x),sd=sd(x))
qfit90 <- qnorm(0.9, mean=mean(x),sd=sd(x))
qfit50

qfit90

## fit prob line at 0.5
# relate probilities to depth and density

## density data
d_data <- density(all_data_freq$Depth)

dens_df <- data.frame(matrix(nrow=length(d_data$y)))
dens_df[1] <- d_data$y
dens_df$Depth <- d_data$x
colnames(dens_df)[1] <- "density"
dens_df

dens_50 <- subset(dens_df, Depth>qfit50)
dens_50[1,1]

line05 <- dens_50[1,1] ## 0.5 line
line05
dens_90 <- subset(dens_df, Depth>qfit90)
line09 <- dens_90[1,1]
line09
plot(density(all_data_freq$Depth))
abline(line05, 0)
abline(line09,0)

## same line on histogram

x <-all_data_freq$Depth
h<-hist(x, breaks=10, col="red", xlab="Depth (cm)",
        main="Histogram with Normal Curve")
xfit<-seq(min(x),max(x),length=130)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)

hist_df <- data.frame(matrix(nrow=length(yfit)))
colnames(hist_df)<- "freq"
hist_df$depth <- xfit
hist_df$freq <- yfit

hist_50 <- subset(hist_df, Depth>qfit50)
hist_50[1,1]

hist05 <- hist_50[1,1] ## 0.5 line
hist05
hist_90 <- subset(hist_df, Depth>qfit90)
hist09 <- hist_90[1,1]
hist09

abline(hist05,0)
abline(hist09,0)

## try quad model

juv_depth_con <- read.csv("output_data/05a_juvenile_depth_continuous.csv")
juv_depth_cat <- read.csv("output_data/05a_juvenile_depth_categorical.csv")

juv_depth_cat
juv_depth_con
all_depth <- rbind(juv_depth_cat, juv_depth_con)
head(all_depth)

plot(all_depth$Depth, all_depth$Abundance)

fit2<-lm(Abundance~poly(Depth,2,raw=TRUE), data=all_depth)

summary(fit2)

pred_depth <- seq(0, 70, 1)
predictedcounts <- predict(fit2,list(Depth=pred_depth, Depth2=pred_depth^2))
plot(all_depth$Depth, all_depth$Abundance, pch=16, xlab = "Depth (cm)", 
     ylab = "Abundance", cex.lab = 1.3, col = "blue")
lines(pred_depth, predictedcounts, col = "darkgreen", lwd = 3)

####

## temperature

## temperature
ad_temp_con <- read.csv("output_data/05a_adult_temperature_continuous.csv")
juv_temp_con <- read.csv("output_data/05a_juvenile_temperature_continuous.csv")
sp_temp_con <- read.csv("output_data/05a_spawning_temperature_continuous.csv")


## adult
ad_temp_con

plot(ad_temp_con$Temp, ad_temp_con$abundance)

fit2<-lm(abundance~poly(Temp,2,raw=TRUE), data=ad_temp_con)

summary(fit2)
range(ad_temp_con$Temp)

pred_temp <- seq(6, 30 , 0.1)
predictedcounts <- predict(fit2,list(Temp=pred_temp, Temp2=pred_temp^2))
plot(ad_temp_con$Temp, ad_temp_con$abundance, pch=16, xlab = "Depth (cm)", 
     ylab = "Abundance", cex.lab = 1.3, col = "blue")
lines(pred_temp, predictedcounts, col = "darkgreen", lwd = 3)


### prob distributions

temp_freq_con <- ad_temp_con %>% 
  uncount(abundance)
hist(temp_freq_con$Temp)
temp_freq_con


############## models

## get probability from histogram
## all data


x <-temp_freq_con$Temp
h<-hist(x,  col="red", xlab="Temp Degrees c",
        main="Adult Temperature")
xfit<-seq(min(x),max(x),length=130)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)
plot(density(temp_freq_con$Temp))

## compare different data sets
attach(temp_freq_con)
temp_freq_con$Dataset
temp_freq_con$Data_Type_num <- ifelse(temp_freq_con$Dataset == "Saiki", 1,2)
temp_freq_con$Data_Type_num
# create value labels
data.f <- factor(Data_Type_num, levels= 1:2,
                 labels = c("Saiki", "SAWA"))
data.f

# plot densities
sm.density.compare(as.vector(Temp), Data_Type_num, xlab="Temp")
title(main="Temp Distribution by Data Type")

# add legend via mouse click
colfill<-c(2:(2+length(levels(data.f))))
legend(locator(1), levels(data.f), fill=colfill)



### best all together but will need some form of normalisation
x <-temp_freq_con$Temp
x
h<-hist(x, breaks=10, col="red", xlab="Temp (degrees c)",
        main="Adult Temperature")
xfit<-seq(min(x),max(x),length=70)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)
xfit

plot(density(temp_freq_con$Temp))#;abline(v=50, col = 2)

range(temp_freq_con$Temp) #6.9 28.6
dim(temp_freq_con)
### probabilities
d <- seq(6, 30 , 0.1)
length(d)
pfit <- dnorm(d, mean=mean,sd=sd(x))
str(pfit)
max(pfit)
pfitx <- unlist(pfit)
length(pfitx)
pfit_df <- data.frame(matrix(nrow=241, ncol=2))
pfit_df[,1] <- pfitx
pfit_df[,2] <- seq(6, 30 , 0.1)
colnames(pfit_df) <- c("density", "Temp")
pfit_df

# density curve
plot(pfit_df$Temp, pfit_df$density, xlab="Temp (degrees c)", ylab= "Probability Density")


### get cumulative probability

### best all together but will need some form of normalisation
x <-temp_freq_con$Temp
h<-hist(x, breaks=10, col="red", xlab="Temp",
        main="Adult Temp")
xfit<-seq(min(x),max(x),length=70)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)
xfit

### probabilities
d <- seq(6, 30 , 0.1)
length(d)
pfit <- pnorm(d, mean=mean(x),sd=sd(x))
str(pfit)
max(pfit)
pfitx <- unlist(pfit)
length(pfitx)
pfit_df <- data.frame(matrix(nrow=241, ncol=2))
pfit_df[,1] <- pfitx
pfit_df[,2] <- seq(6, 30 , 0.1)
colnames(pfit_df) <- c("density", "Temp")
pfit_df
length(x)
## cumulative
plot(pfit_df$Temp, pfit_df$density, xlab="Temp", ylab= "Probability")

plot(density(temp_freq_con$Temp))

## get actual probability

x <-temp_freq_con$Temp
h<-hist(x, breaks=10, lty="white", xlab="Temp",
        main="Adult Temp")
xfit<-seq(min(x),max(x),length=30)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)
xfit

ub <- 25
lb <- 14
mean=mean(x)
sd=sd(x)

i <- xfit >= lb & xfit <= ub
lines(xfit, yfit)
polygon(c(lb,xfit[i],ub), c(0,yfit[i],0), col="blue")

area <- pnorm(ub, mean, sd) - pnorm(lb, mean, sd)
result <- paste("P(",lb,"< Temp <",ub,") =",
                signif(area, digits=3))
mtext(result,3) ## 0.60

# axis(1, at=seq(0, 120, 20), pos=0)

# mean=mean(x)
# sd=sd(x)
# h<-hist(x, xlab="Depth (cm)",col="white",lty="blank",
#         main="Histogram with Normal Curve")
# xfit<-seq(min(x),max(x),length=130)
# yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
# yfit <- yfit*diff(h$mids[1:2])*length(x)
# yfit


lb <- 10
ub <- 14
xnew <- 10
i <- xfit >= lb & xfit <= ub
lines(xfit, yfit)

polygon(c(lb,xfit[i],ub), c(0,yfit[i],0), col="red")

approx(xfit,yfit,xout=xnew)
geom_vline(10,43.62174)


area <- pnorm(ub, mean, sd) - pnorm(lb, mean, sd)
result <- paste("P(",lb,"< Depth <",ub,") =",
                signif(area, digits=3))
result
mtext(result,3) ## 0.13

# h<-hist(x, xlab="Depth (cm)",col="white",lty="blank",
#         main="Histogram with Normal Curve")
# xfit<-seq(min(x),max(x),length=130)
# yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
# yfit <- yfit*diff(h$mids[1:2])*length(x)
# yfit


lb <- 25
ub <- 29

i <- xfit >= lb & xfit <= ub
lines(xfit, yfit)
polygon(c(lb,xfit[i],ub), c(0,yfit[i],0), col="red")

area <- pnorm(ub, mean, sd) - pnorm(lb, mean, sd)
result <- paste("P(",lb,"< Depth <",ub,") =",
                signif(area, digits=3))
area # 0.12
mtext(result,3) # 0.18


## probabilities of abundance at a certain depth
tfit <- pnorm(90, mean=mean(x),sd=sd(x))
tfit ## 0.8142976
## depth at certain probabilities
qfit50 <- qnorm(0.5,mean=mean(x),sd=sd(x))
qfit90 <- qnorm(0.9, mean=mean(x),sd=sd(x))
qfit50

qfit90

## fit prob line at 0.5
# relate probilities to depth and density

## density data
d_data <- density(all_data_freq$Depth)

dens_df <- data.frame(matrix(nrow=length(d_data$y)))
dens_df[1] <- d_data$y
dens_df$Depth <- d_data$x
colnames(dens_df)[1] <- "density"
dens_df

dens_50 <- subset(dens_df, Depth>qfit50)
dens_50[1,1]

line05 <- dens_50[1,1] ## 0.5 line
line05
dens_90 <- subset(dens_df, Depth>qfit90)
line09 <- dens_90[1,1]
line09
plot(density(all_data_freq$Depth))
abline(line05, 0)
abline(line09,0)

## same line on histogram

x <-all_data_freq$Depth
h<-hist(x, breaks=10, col="red", xlab="Depth (cm)",
        main="Histogram with Normal Curve")
xfit<-seq(min(x),max(x),length=130)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)

hist_df <- data.frame(matrix(nrow=length(yfit)))
colnames(hist_df)<- "freq"
hist_df$depth <- xfit
hist_df$freq <- yfit

hist_50 <- subset(hist_df, Depth>qfit50)
hist_50[1,1]

hist05 <- hist_50[1,1] ## 0.5 line
hist05
hist_90 <- subset(hist_df, Depth>qfit90)
hist09 <- hist_90[1,1]
hist09

abline(hist05,0)
abline(hist09,0)

### juvenile temperature


juv_temp_con

plot(juv_temp_con$Temp, juv_temp_con$Abundance)

fit2<-lm(Abundance~poly(Temp,2,raw=TRUE), data=juv_temp_con)

summary(fit2)
range(juv_temp_con$Temp)

pred_temp <- seq(8, 30 , 0.1)
predictedcounts <- predict(fit2,list(Temp=pred_temp, Temp2=pred_temp^2))
plot(juv_temp_con$Temp, juv_temp_con$Abundance, pch=16, xlab = "Depth (cm)", 
     ylab = "Abundance", cex.lab = 1.3, col = "blue")
lines(pred_temp, predictedcounts, col = "darkgreen", lwd = 3)


### prob distributions

temp_freq_con <- juv_temp_con %>% 
  uncount(Abundance)
hist(temp_freq_con$Temp)
temp_freq_con


############## models

## get probability from histogram
## all data


x <-temp_freq_con$Temp
h<-hist(x,  col="red", xlab="Temp Degrees c",
        main="Juvenile Temperature")
xfit<-seq(min(x),max(x),length=130)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)
plot(density(temp_freq_con$Temp))



# add legend via mouse click
colfill<-c(2:(2+length(levels(data.f))))
legend(locator(1), levels(data.f), fill=colfill)



### best all together but will need some form of normalisation
x <-temp_freq_con$Temp
x
h<-hist(x, breaks=10, col="red", xlab="Temp (degrees c)",
        main="Juvenile Temperature")
xfit<-seq(min(x),max(x),length=70)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)
xfit

plot(density(temp_freq_con$Temp))#;abline(v=50, col = 2)

range(temp_freq_con$Temp) #6.9 28.6
dim(temp_freq_con)
### probabilities
d <- seq(8, 30 , 0.1)
length(d)
pfit <- dnorm(d, mean=mean,sd=sd(x))
str(pfit)
max(pfit)
pfitx <- unlist(pfit)
length(pfitx)
pfit_df <- data.frame(matrix(nrow=221, ncol=2))
pfit_df[,1] <- pfitx
pfit_df[,2] <- seq(8, 30 , 0.1)
colnames(pfit_df) <- c("density", "Temp")
pfit_df

# density curve
plot(pfit_df$Temp, pfit_df$density, xlab="Temp (degrees c)", ylab= "Probability Density")


### get cumulative probability

### best all together but will need some form of normalisation
x <-temp_freq_con$Temp
h<-hist(x, breaks=10, col="red", xlab="Temp",
        main="Juvenile Temp")
xfit<-seq(min(x),max(x),length=70)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)
xfit

### probabilities
d <- seq(8, 30 , 0.1)
length(d)
pfit <- pnorm(d, mean=mean(x),sd=sd(x))
str(pfit)
max(pfit)
pfitx <- unlist(pfit)
length(pfitx)
pfit_df <- data.frame(matrix(nrow=221, ncol=2))
pfit_df[,1] <- pfitx
pfit_df[,2] <- seq(8, 30 , 0.1)
colnames(pfit_df) <- c("density", "Temp")
pfit_df
length(x)
## cumulative
plot(pfit_df$Temp, pfit_df$density, xlab="Temp", ylab= "Probability")

plot(density(temp_freq_con$Temp))

## get actual probability

x <-temp_freq_con$Temp
h<-hist(x, breaks=10, lty="white", xlab="Temp",
        main="Adult Temp")
xfit<-seq(min(x),max(x),length=30)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)
xfit

ub <- 25
lb <- 14
mean=mean(x)
sd=sd(x)

i <- xfit >= lb & xfit <= ub
lines(xfit, yfit)
polygon(c(lb,xfit[i],ub), c(0,yfit[i],0), col="blue")

area <- pnorm(ub, mean, sd) - pnorm(lb, mean, sd)
result <- paste("P(",lb,"< Temp <",ub,") =",
                signif(area, digits=3))
mtext(result,3) ## 0.60

# axis(1, at=seq(0, 120, 20), pos=0)

# mean=mean(x)
# sd=sd(x)
# h<-hist(x, xlab="Depth (cm)",col="white",lty="blank",
#         main="Histogram with Normal Curve")
# xfit<-seq(min(x),max(x),length=130)
# yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
# yfit <- yfit*diff(h$mids[1:2])*length(x)
# yfit


lb <- 10
ub <- 14
xnew <- 10
i <- xfit >= lb & xfit <= ub
lines(xfit, yfit)

polygon(c(lb,xfit[i],ub), c(0,yfit[i],0), col="red")

approx(xfit,yfit,xout=xnew)
geom_vline(10,43.62174)


area <- pnorm(ub, mean, sd) - pnorm(lb, mean, sd)
result <- paste("P(",lb,"< Depth <",ub,") =",
                signif(area, digits=3))
result
mtext(result,3) ## 0.13

# h<-hist(x, xlab="Depth (cm)",col="white",lty="blank",
#         main="Histogram with Normal Curve")
# xfit<-seq(min(x),max(x),length=130)
# yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
# yfit <- yfit*diff(h$mids[1:2])*length(x)
# yfit


lb <- 25
ub <- 29

i <- xfit >= lb & xfit <= ub
lines(xfit, yfit)
polygon(c(lb,xfit[i],ub), c(0,yfit[i],0), col="red")

area <- pnorm(ub, mean, sd) - pnorm(lb, mean, sd)
result <- paste("P(",lb,"< Depth <",ub,") =",
                signif(area, digits=3))
area # 0.12
mtext(result,3) # 0.18


## probabilities of abundance at a certain depth
tfit <- pnorm(90, mean=mean(x),sd=sd(x))
tfit ## 0.8142976
## depth at certain probabilities
qfit50 <- qnorm(0.5,mean=mean(x),sd=sd(x))
qfit90 <- qnorm(0.9, mean=mean(x),sd=sd(x))
qfit50

qfit90

## fit prob line at 0.5
# relate probilities to depth and density

## density data
d_data <- density(all_data_freq$Depth)

dens_df <- data.frame(matrix(nrow=length(d_data$y)))
dens_df[1] <- d_data$y
dens_df$Depth <- d_data$x
colnames(dens_df)[1] <- "density"
dens_df

dens_50 <- subset(dens_df, Depth>qfit50)
dens_50[1,1]

line05 <- dens_50[1,1] ## 0.5 line
line05
dens_90 <- subset(dens_df, Depth>qfit90)
line09 <- dens_90[1,1]
line09
plot(density(all_data_freq$Depth))
abline(line05, 0)
abline(line09,0)

## same line on histogram

x <-all_data_freq$Depth
h<-hist(x, breaks=10, col="red", xlab="Depth (cm)",
        main="Histogram with Normal Curve")
xfit<-seq(min(x),max(x),length=130)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)

hist_df <- data.frame(matrix(nrow=length(yfit)))
colnames(hist_df)<- "freq"
hist_df$depth <- xfit
hist_df$freq <- yfit

hist_50 <- subset(hist_df, Depth>qfit50)
hist_50[1,1]

hist05 <- hist_50[1,1] ## 0.5 line
hist05
hist_90 <- subset(hist_df, Depth>qfit90)
hist09 <- hist_90[1,1]
hist09

abline(hist05,0)
abline(hist09,0)

### velocity

ad_vel_con <- read.csv("output_data/05a_adult_velocity_continuous.csv")
ad_vel_cat <- read.csv("output_data/05a_adult_velocity_categorical.csv")
juv_vel_con <- read.csv("output_data/05a_juvenile_velocity_continuous.csv")
juv_vel_cat <- read.csv("output_data/05a_juvenile_velocity_categorical.csv")
sp_vel_cat <- read.csv("output_data/05a_spawning_velocity_continuous.csv")


head(ad_vel_con)
ad_vel_cat
ad_vel_cat$Dataset <- "SMEA"
names(ad_vel_cat)[3] <-"Abundance"

all_vel<- rbind(ad_vel_con, ad_vel_cat)
plot(ad_depth_con$Depth, ad_depth_con$Abundance, col=ad_depth_con$Dataset)
unique(all_vel$Dataset)
summary(lm(Abundance~Depth, data=ad_depth_con))
hist(ad_depth_con$Depth)

depth_vel_con <- ad_vel_con %>%
  uncount(Abundance)
hist(depth_vel_con$Velocity)

## need to uncount data into frequency

vel_freq <- ad_vel_cat %>% 
  uncount(Abundance)
hist(vel_freq$Velocity)
depth_freq

## add data type column

depth_vel_con$Data_Type <- "continuous"
vel_freq$Data_Type <- "categorical"
depth_vel_con$Data_Type_num <- 1
vel_freq$Data_Type_num <- 2



## add together to check histogram
all_data_freq <- rbind(vel_freq, depth_vel_con)
dim(all_data_freq)
all_data_freq$Depth <- as.numeric(as.character(all_data_freq$Velocity))
hist(all_data_freq$Velocity) ## 71 category 
str(all_data_freq)
all_data_freq <- na.omit(all_data_freq)

############## models

## get probability from histogram
## all data
## includes 71 category and buffers the influence

## test with r tutorial
min(x)
x <-all_data_freq$Velocity
h<-hist(x, col="red", xlab="Velocity (ms)",
        main="Adult Velocity")
xfit<-seq(min(x),max(x),length=130)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)
plot(density(all_data_freq$Velocity))

## compare different data sets
attach(all_data_freq)

# create value labels
data.f <- factor(Data_Type_num, levels= 1:2,
                 labels = c("Continuous", "Categorical"))
data.f

# plot densities
sm.density.compare(as.vector(Velocity), Data_Type_num, xlab="Velocity")
title(main="Velocity Distribution by Data Type")

# add legend via mouse click
colfill<-c(2:(2+length(levels(data.f))))
legend(locator(1), levels(data.f), fill=colfill)

depth_vel_con$Data_Type <- "continuous"
vel_freq$Data_Type <- "categorical"
depth_vel_con$Data_Type_num <- 1
vel_freq$Data_Type_num <- 2

tx <- depth_vel_con$Dataset == "Thompson"
sx <- depth_vel_con$Dataset == "Saiki"
wx <- depth_vel_con$Dataset == "Wulff"

depth_vel_con$Dataset_num[tx] <- 1
depth_vel_con$Dataset_num[sx] <- 2
depth_vel_con$Dataset_num[wx] <- 3

vel_freq$Dataset_num <- 4
## add together to check histogram
all_data_freq <- rbind(vel_freq, depth_vel_con)
dim(all_data_freq)
all_data_freq$Velocity <- as.numeric(as.character(all_data_freq$Velocity))
hist(all_data_freq$Velocity) ## 71 category 
str(all_data_freq)
all_data_freq <- na.omit(all_data_freq)

############## models

## get probability from histogram
## all data
## includes 71 category and buffers the influence

## test with r tutorial
min(x)
x <-all_data_freq$Velocity
h<-hist(x, col="red", xlab="Velocity (ms)",
        main="Adult Velocity")
xfit<-seq(min(x),max(x),length=130)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)
plot(density(all_data_freq$Velocity))

## compare different data sets
attach(all_data_freq)
tail(Dataset_num)
# create value labels
data.f <- factor(Dataset_num, levels= 1:4,
                 labels = c("Thompson", "Saiki", "Wulff", "SMEA"))
data.f

# plot densities
sm.density.compare(as.vector(Velocity), Dataset_num, xlab="Velocity")
title(main="Velocity Distribution by Dataset")

# add legend via mouse clickt
colfill<-c(2:(2+length(levels(data.f))))
legend(locator(1), levels(data.f), fill=colfill)

## categorical

plot(depth_freq$Depth, depth_freq$Abundance) ## many 1's skewing the curve
str(all_df)
## normal dist 
x <-depth_freq$Depth
h<-hist(x, breaks=10, col="red", xlab="Depth (cm)",
        main="Histogram with Normal Curve")
xfit<-seq(min(x),max(x),length=130)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)

plot(density(depth_freq$Depth))
######## continuous

## normal dist 
x <-depth_freq_con$Depth
h<-hist(x, breaks=10, col="red", xlab="Depth (cm)",
        main="Histogram with Normal Curve")
xfit<-seq(min(x),max(x),length=130)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)

plot(density(depth_freq_con$Depth))

### best all together but will need some form of normalisation
x <-all_data_freq$Depth
h<-hist(x, breaks=10, col="red", xlab="Depth (cm)",
        main="Histogram with Normal Curve")
xfit<-seq(min(x),max(x),length=130)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)
xfit

plot(density(all_data_freq$Depth))#;abline(v=50, col = 2)

min(all_data_freq$Depth)
### probabilities
d <- seq(1,130,1)
pfit <- dnorm(d, mean=mean(x),sd=sd(x))
str(pfit)
max(pfit)
pfitx <- unlist(pfit)
length(pfitx)
pfit_df <- data.frame(matrix(nrow=130, ncol=2))
pfit_df[,1] <- pfitx
pfit_df[,2] <- seq(1,130,1)
colnames(pfit_df) <- c("density", "Depth")
pfit_df

# density curve
plot(pfit_df$Depth, pfit_df$density, xlab="Depth (cm)", ylab= "Probability Density")


### get cumulative probability

### best all together but will need some form of normalisation
x <-all_data_freq$Depth
h<-hist(x, breaks=10, col="red", xlab="Depth (cm)",
        main="Histogram with Normal Curve")
xfit<-seq(min(x),max(x),length=130)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)
xfit

plot(density(all_data_freq$Depth))#;abline(v=50, col = 2)

min(all_data_freq$Depth)
### probabilities
d <- seq(1,130,1)
pfit <- pnorm(d, mean=mean(x),sd=sd(x))
str(pfit)
max(pfit)
pfitx <- unlist(pfit)
length(pfitx)
pfit_df <- data.frame(matrix(nrow=130, ncol=2))
pfit_df[,1] <- pfitx
pfit_df[,2] <- seq(1,130,1)
colnames(pfit_df) <- c("density", "Depth")
pfit_df

## cumulative
plot(pfit_df$Depth, pfit_df$density, xlab="Depth (cm)", ylab= "Probability")

## get probability density curve

x <-all_data_freq$Depth

plot(pfit_df$Depth, pfit_df$density, xlab="Depth (cm)", ylab= "Probability")

## get actual probability

?hist
h<-hist(x, xlab="Depth (cm)",col="white",lty="blank",
        main="Histogram with Normal Curve")
xfit<-seq(min(x),max(x),length=130)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
yfit

ub <- 50
lb <- 20
mean=mean(x)
sd=sd(x)

i <- xfit >= lb & xfit <= ub
lines(xfit, yfit)
polygon(c(lb,xfit[i],ub), c(0,yfit[i],0), col="blue")

area <- pnorm(ub, mean, sd) - pnorm(lb, mean, sd)
result <- paste("P(",lb,"< Depth <",ub,") =",
                signif(area, digits=3))
mtext(result,3) ## 0.585
# axis(1, at=seq(0, 120, 20), pos=0)

mean=mean(x)
sd=sd(x)
h<-hist(x, xlab="Depth (cm)",col="white",lty="blank",
        main="Histogram with Normal Curve")
xfit<-seq(min(x),max(x),length=130)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
yfit


lb <- 5
ub <- 20

i <- xfit >= lb & xfit <= ub
lines(xfit, yfit)
polygon(c(lb,xfit[i],ub), c(0,yfit[i],0), col="red")

area <- pnorm(ub, mean, sd) - pnorm(lb, mean, sd)
result <- paste("P(",lb,"< Depth <",ub,") =",
                signif(area, digits=3))
mtext(result,3) ## 0.17

h<-hist(x, xlab="Depth (cm)",col="white",lty="blank",
        main="Histogram with Normal Curve")
xfit<-seq(min(x),max(x),length=130)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
yfit


lb <- 50
ub <- 80

i <- xfit >= lb & xfit <= ub
lines(xfit, yfit)
polygon(c(lb,xfit[i],ub), c(0,yfit[i],0), col="red")

area <- pnorm(ub, mean, sd) - pnorm(lb, mean, sd)
result <- paste("P(",lb,"< Depth <",ub,") =",
                signif(area, digits=3))
mtext(result,3) # 0.18


## probabilities of abundance at a certain depth
tfit <- pnorm(90, mean=mean(x),sd=sd(x))
tfit ## 0.8142976
## depth at certain probabilities
qfit50 <- qnorm(0.5,mean=mean(x),sd=sd(x))
qfit90 <- qnorm(0.9, mean=mean(x),sd=sd(x))
qfit50

qfit90

## fit prob line at 0.5
# relate probilities to depth and density

## density data
d_data <- density(all_data_freq$Depth)

dens_df <- data.frame(matrix(nrow=length(d_data$y)))
dens_df[1] <- d_data$y
dens_df$Depth <- d_data$x
colnames(dens_df)[1] <- "density"
dens_df

dens_50 <- subset(dens_df, Depth>qfit50)
dens_50[1,1]

line05 <- dens_50[1,1] ## 0.5 line
line05
dens_90 <- subset(dens_df, Depth>qfit90)
line09 <- dens_90[1,1]
line09
plot(density(all_data_freq$Depth))
abline(line05, 0)
abline(line09,0)

## same line on histogram

x <-all_data_freq$Depth
h<-hist(x, breaks=10, col="red", xlab="Depth (cm)",
        main="Histogram with Normal Curve")
xfit<-seq(min(x),max(x),length=130)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)

hist_df <- data.frame(matrix(nrow=length(yfit)))
colnames(hist_df)<- "freq"
hist_df$depth <- xfit
hist_df$freq <- yfit

hist_50 <- subset(hist_df, Depth>qfit50)
hist_50[1,1]

hist05 <- hist_50[1,1] ## 0.5 line
hist05
hist_90 <- subset(hist_df, Depth>qfit90)
hist09 <- hist_90[1,1]
hist09

abline(hist05,0)
abline(hist09,0)

#####################################################

## quadratic curve
## use abundance data

head(all_depth)

plot(all_depth$Depth, all_depth$Abundance)

fit2<-lm(Abundance~poly(Depth,2,raw=TRUE), data=all_depth)

summary(fit2)

pred_depth <- seq(0, 130, 1)
predictedcounts <- predict(fit2,list(Depth=pred_depth, Depth2=pred_depth^2))
plot(all_depth$Depth, all_depth$Abundance, pch=16, xlab = "Depth (cm)", 
     ylab = "Abundance", cex.lab = 1.3, col = "blue")
lines(pred_depth, predictedcounts, col = "darkgreen", lwd = 3)


## skewed due to all the 1s

one_depth <- subset(all_depth, Abundance>1)
dim(one_depth)

## remove categorised and mean data - worse
unique(all_depth$Dataset)[c(2,5)]
data_out <- droplevels(unique(all_depth$Dataset)[c(2,5)])
data_out
some_depth <- subset(all_depth, !Dataset %in% data_out)
some_depth

## try model again
plot(some_depth$Depth, some_depth$Abundance)

Depth2 <- all_depth$Depth^2

fit2<-lm(Abundance~poly(Depth,2,raw=TRUE), data=all_depth)


pred_depth <- seq(0, 130, 1)
predictedcounts <- predict(fit2,list(Depth=pred_depth, Depth2=pred_depth^2))
plot(some_depth$Depth, some_depth$Abundance, pch=16, xlab = "Depth (cm)", 
     ylab = "Abundance", cex.lab = 1.3, col = "blue")
lines(pred_depth, predictedcounts, col = "darkgreen", lwd = 3)
## skewed due to all the 1s

one_depth <- subset(all_depth, Abundance>1)
dim(one_depth)

#### log 

log(all_depth$Abundance)+1

all_depth$log_abundance <- log(all_depth$Abundance)+1

plot(all_depth$Depth, all_depth$Abundance)

fit2<-lm(log(Abundance+1)~poly(Depth,2,raw=TRUE), data=all_depth)

summary(fit2)

pred_depth <- seq(0, 130, 1)
predictedcounts <- predict(fit2,list(Depth=pred_depth, Depth2=pred_depth^2))
plot(all_depth$Depth, log(all_depth$Abundance+1), pch=16, xlab = "Depth (cm)", 
     ylab = "Abundance", cex.lab = 1.3, col = "blue")
lines(pred_depth, predictedcounts, col = "darkgreen", lwd = 3)
## skewed due to all the 1s

one_depth <- subset(all_depth, Abundance>1)
dim(one_depth)

### try with just smea

head(ad_depth_cat)

plot(ad_depth_cat$Depth, ad_depth_cat$Abundance)

fit2<-lm(Abundance~poly(Depth,2,raw=TRUE), data=ad_depth_cat)

summary(fit2)

pred_depth <- seq(0, 70, 1)
predictedcounts <- predict(fit2,list(Depth=pred_depth, Depth2=pred_depth^2))
plot(ad_depth_cat$Depth, ad_depth_cat$Abundance, pch=16, xlab = "Depth (cm)", 
     ylab = "Abundance", cex.lab = 1.3, col = "blue")
lines(pred_depth, predictedcounts, col = "darkgreen", lwd = 3)

## smea minus 70 category

ad_depth_catx <- ad_depth_cat[-15,]

head(ad_depth_catx)

plot(ad_depth_catx$Depth, ad_depth_catx$Abundance)

fit2<-lm(Abundance~poly(Depth,2,raw=TRUE), data=ad_depth_catx)

summary(fit2)

pred_depth <- seq(0, 70, 1)
predictedcounts <- predict(fit2,list(Depth=pred_depth, Depth2=pred_depth^2))
plot(ad_depth_catx$Depth, ad_depth_catx$Abundance, pch=16, xlab = "Depth (cm)", 
     ylab = "Abundance", cex.lab = 1.3, col = "blue")
lines(pred_depth, predictedcounts, col = "darkgreen", lwd = 3)
