### all probability curves

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
sp_depth_cat <- read.csv("output_data/08a_spawning_depth_abundance.csv")

## format spawning
sp_depth_catx <- sp_depth_cat[,c(4,15)]
sp_depth_catx$Depth <- sp_depth_catx$Depth..m.*100
sp_depth_catx$Abundance <- 1
sp_depth_catx$Dataset <- "Saiki"
sp_depth_catx<- sp_depth_catx[, 3:5]
sp_depth_catx$Life_Stage <- "Spawning"
sp_depth_catx
## combine data adult
all_depth <- rbind(ad_depth_con, ad_depth_cat)

all_depth$Life_Stage <- "Adult"
head(all_depth)
all_depth<- all_depth[,-1]

## combine juv depth
juv_depth <- rbind(juv_depth_cat, juv_depth_con)
juv_depth$Life_Stage <- "Juvenile"
juv_depth <- juv_depth[,-1]
## combine all
head(juv_depth)
head(all_depth)
head(sp_depth_catx)
all_depthx <- rbind(all_depth, sp_depth_catx, juv_depth)
all_depthx <- na.omit(all_depthx)
## uncount data into frequency 

## combine data adult
all_depthx <- rbind(ad_depth_con, ad_depth_cat)


depth_freq <- all_depthx %>% 
  uncount(Abundance)
hist(depth_freq$Depth)

##look at different life stages

tx <- depth_freq$Life_Stage == "Adult"
sx <- depth_freq$Life_Stage == "Juvenile"
wx <- depth_freq$Life_Stage == "Spawning"


depth_freq$Dataset_num[tx] <- 1
depth_freq$Dataset_num[sx] <- 2
depth_freq$Dataset_num[wx] <- 3
sum(is.na(depth_freq))

attach(depth_freq)

# create value labels
data.f <- factor(Dataset_num, levels= 1:3,
                 labels = c("Adult", "Juvenile", "Spawning"))
data.f
str(Depth)
as.vector(Depth)
# plot densities
sm.density.compare(as.vector(Depth), Dataset_num, xlab="Depth (cm)")
title(main="Depth Distribution by Life Stage")

# add legend via mouse click
colfill<-c(2:(2+length(levels(data.f))))
legend(locator(1), levels(data.f), fill=colfill)

### individual life stages
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

### centered and scaled histogram probability
mean ## 33.5929
(71-33.5929)/4
subset(depth_freq, Scaled_Depth >=2)

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
scaled_x
dnorm()

### spawning

## uncount data into frequency
sp_depth_catx <- sp_depth_cat[,c(4,15)]
sp_depth_catx$Depth <- sp_depth_catx$Depth..m.*100
sp_depth_catx$Abundance <- 1
sp_depth_catx$Dataset <- "Saiki"
write.csv(sp_depth_catx, "output_data/08a_spawning_depth_abundance.csv")

depth_freq <- sp_depth_catx %>% 
  uncount(Abundance)
hist(depth_freq$Depth)

##look at different sites

## compare different data sets
### get numbers for datasets
unique(depth_freq$Site)
tx <- depth_freq$Site == "MWDA"
sx <- depth_freq$Site == "MWDB"
wx <- depth_freq$Site == "SGRA"
swx <- depth_freq$Site == "SGRB"
tx
sx
wx
swx
depth_freq$Dataset_num[tx] <- 1
depth_freq$Dataset_num[sx] <- 1
depth_freq$Dataset_num[wx] <- 2
depth_freq$Dataset_num[swx] <- 2

depth_freq <- na.omit(depth_freq)
attach(depth_freq)

# create value labels
data.f <- factor(Dataset_num, levels= 1:2,
                 labels = c("Santa Ana", "San Gabriel"))
data.f
str(Depth)
as.vector(Depth)
# plot densities
as.vector(Depth)
sm.density.compare(as.vector(Depth), Dataset_num, xlab="Depth (cm)")
title(main="Depth Distribution by Site (Saiki)")

# add legend via mouse click
colfill<-c(2:(2+length(levels(data.f))))
legend(locator(1), levels(data.f), fill=colfill)

### centered and scaled histogram probability
mean ## 34.5
depth_freq <- na.omit(depth_freq)
mean(depth_freq$Depth)

depth_freq$Scaled_Depth <-scale(depth_freq$Depth, scale=T, center=T)
scaled_x <- depth_freq$Scaled_Depth
h <- hist(scaled_x)
h$counts=(h$counts/sum(h$counts))*100
plot(h, xlab="Distance from mean Depth", ylab = "Percentage of observations",main = "Spawning/Depth: Probability curve")
par(new=TRUE)
xfit<-seq(min(scaled_x),max(scaled_x),length=120)
yfit<-dnorm(xfit,mean=mean(scaled_x),sd=sd(scaled_x))
#plot the line with no axes or labels
plot(xfit, yfit, axes=FALSE, xlab='', ylab='', type='l', col='red' )
#add these now with axis
axis(4, at=pretty(range(yfit)))


### juvenile

juv_depth_con <- read.csv("output_data/05a_juvenile_depth_continuous.csv")
juv_depth_cat <- read.csv("output_data/05a_juvenile_depth_categorical.csv")
# sp_depth_cat <- read.csv("output_data/05a_spawning_depth_continuous.csv")

## combine data
all_depth <- rbind(juv_depth_con, juv_depth_cat)
unique(all_depth$Dataset)
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

### example of logistic regression with percentage

## frequency column
#### histogram
## use adult data
all_depth <- rbind(ad_depth_con, ad_depth_cat)

depth_freq <- all_depth %>% 
  uncount(Abundance)
hist(depth_freq$Depth)

sum(h$counts)
x <-depth_freq$Depth
h<-hist(x)
fr <- h$counts
per <- ((h$counts/sum(h$counts))*100)
h$counts=(h$counts/sum(h$counts))*100

xfit<-seq(min(x),max(x),length=130)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)
diff(h$mids[1:2])
xfit ## depths
yfit ## frequencies

percs <- as.data.frame(matrix(ncol=2,nrow=12))
colnames(percs) <- c("freq", "percs")
percs$freq <- fr
percs$percs <- per
percs

freq_df <- cbind(xfit, yfit)

freq_df <- as.data.frame(freq_df)
names(freq_df) <- c("Depth", "Freq")
str(freq_df)
max(freq_df$Freq)


freq_df$f140 <- ifelse(freq_df$Freq >=140, 1,0) ## around 5% of observations
range(freq_df[which(freq_df$f140==1),"Depth"]) ## depths between 64 & 120


freq_df$f300 <- ifelse(freq_df$Freq  >= 300, 1,0) ## around 12.5% of observations
range(freq_df[which(freq_df$f300==1),"Depth"])

freq_df$f493 <- ifelse(freq_df$Freq >= 493 , 1,0) ## around 20.3% of observations
range(freq_df[which(freq_df$f493==1),"Depth"])

# freq_df$fmax <- ifelse(freq_df$Freq > 493, 1,0) ## around 20.3% of observations
# range(freq_df[which(freq_df$fmax==1),"Depth"])

freq_df

## model glm

freq140_glm <- glm(f140~Depth, data=freq_df,family=binomial(link = "logit"))
summary(freq140_glm)

freq300_glm <- glm(f300~Depth, data=freq_df,family=binomial(link = "logit"))
summary(freq300_glm)

freq493_glm <- glm(f493~Depth, data=freq_df,family=binomial(link = "logit"))
summary(freq493_glm)

freqmax_glm <- glm(fmax~Depth, data=freq_df,family=binomial(link = "logit"))
summary(freqmax_glm)


# create range of depth values

xdepth  <- seq(0,130, 0.1)
## prediction

## percentage is in relation to the bins in histogram

ydepth <- predict(freq140_glm, list(Depth = xdepth), type="response")
plot(freq_df$Depth, freq_df$f140, pch = 16, xlab = "Depth (cm)", ylab = "Presence", main="Adult/Depth: 5% Obvs")
lines(xdepth, ydepth) ## need to force through 0


ydepth <- predict(freq300_glm, list(Depth = xdepth), type="response")
plot(freq_df$Depth, freq_df$f300, pch = 16, xlab = "Depth (cm)", ylab = "Presence", main="Adult/Depth: 12.5% Obvs")
lines(xdepth, ydepth) ## need to force through 0

ydepth <- predict(freq493_glm, list(Depth = xdepth), type="response")
plot(freq_df$Depth, freq_df$f493, pch = 16, xlab = "Depth (cm)", ylab = "Presence", main="Adult/Depth: 46% Obvs")
lines(xdepth, ydepth) ## need to force through 0

ydepth <- predict(freqmax_glm, list(Depth = xdepth), type="response")
plot(freq_df$Depth, freq_df$fmax, pch = 16, xlab = "Depth (cm)", ylab = "Presence", main="Adult/Depth: 46% Obvs")
lines(xdepth, ydepth) ## need to force through 0

## back to prob curves

## temperature
ad_temp_con <- read.csv("output_data/05a_adult_temperature_continuous.csv")
juv_temp_con <- read.csv("output_data/05a_juvenile_temperature_continuous.csv")
sp_temp_con <- read.csv("output_data/05a_spawning_temperature_continuous.csv")
juv_temp_con
## adult
ad_temp_con
## uncount data into frequency

temp_freq <- ad_temp_con %>% 
  uncount(abundance)
hist(temp_freq$Temp)

##look at different data sets

## compare different data sets
### get numbers for datasets
unique(temp_freq$Dataset)
sx <- temp_freq$Dataset == "Saiki"
swx <- temp_freq$Dataset == "SAWA"

temp_freq$Dataset_num[sx] <- 1
temp_freq$Dataset_num[swx] <- 2

attach(temp_freq)

# create value labels
data.f <- factor(Dataset_num, levels= 1:2,
                 labels = c("Saiki", "SAWA"))


# plot densities
sm.density.compare(as.vector(Temp), Dataset_num, xlab="Temp")
title(main="Temp Distribution by Dataset (Adult)")

# add legend via mouse click
colfill<-c(2:(2+length(levels(data.f))))
legend(locator(1), levels(data.f), fill=colfill)

mean(temp_freq$Temp) ## 19.19221
temp_freq$Scaled_Temp <-scale(temp_freq$Temp, scale=T, center=T)
scaled_x <- temp_freq$Scaled_Temp
h <- hist(scaled_x)
h$counts=(h$counts/sum(h$counts))*100
plot(h,  xlab="Distance from mean Temp", ylab = "Percentage of observations", main = "Adult/Temp: Probability curve")
par(new=TRUE)
xfit<-seq(min(scaled_x),max(scaled_x),length=120)
yfit<-dnorm(xfit,mean=mean(scaled_x),sd=sd(scaled_x))
#plot the line with no axes or labels
plot(xfit, yfit, axes=FALSE, xlab='', ylab='', type='l', col='red' )
#add these now with axis
axis(4, at=pretty(range(yfit)))
# axis(1, at=pretty(seq(0,120,1)))

## Juvenile

juv_temp_con
## adult
ad_temp_con
## uncount data into frequency

temp_freq <- juv_temp_con %>% 
  uncount(Abundance)
hist(temp_freq$Temp)

mean(temp_freq$Temp) ## 17.02222
temp_freq$Scaled_Temp <-scale(temp_freq$Temp, scale=T, center=T)
scaled_x <- temp_freq$Scaled_Temp
h <- hist(scaled_x)
h$counts=(h$counts/sum(h$counts))*100
plot(h,  xlab="Distance from mean Temp", ylab = "Percentage of observations", main = "Juvenile/Temp: Probability curve")
par(new=TRUE)
xfit<-seq(min(scaled_x),max(scaled_x),length=120)
yfit<-dnorm(xfit,mean=mean(scaled_x),sd=sd(scaled_x))
#plot the line with no axes or labels
plot(xfit, yfit, axes=FALSE, xlab='', ylab='', type='l', col='red' )
#add these now with axis
axis(4, at=pretty(range(yfit)))
# axis(1, at=pretty(seq(0,120,1)))
min(temp_freq$Temp)

## spawning
sp_temp_conx <- sp_temp_con[,c(4,14)]
sp_temp_conx$Temp <- sp_temp_conx$Temp...C.
sp_temp_conx$Temp...C. <- NULL
sp_temp_conx$Abundance <- 1
sp_temp_conx$Dataset <- "Saiki"

sp_temp_conx
sp_temp_conx <- na.omit(sp_temp_conx)
write.csv(sp_temp_conx, "output_data/08a_spawning_temp_abundance.csv")
temp_freq <- sp_temp_conx %>% 
  uncount(Abundance)
hist(temp_freq$Temp)

mean(temp_freq$Temp) ## 19.482
temp_freq$Scaled_Temp <-scale(temp_freq$Temp, scale=T, center=T)
scaled_x <- temp_freq$Scaled_Temp
h <- hist(scaled_x)
h$counts=(h$counts/sum(h$counts))*100
plot(h,  xlab="Distance from mean Temp", ylab = "Percentage of observations", main = "Spawning/Temp: Probability curve")
par(new=TRUE)
xfit<-seq(min(scaled_x),max(scaled_x),length=120)
yfit<-dnorm(xfit,mean=mean(scaled_x),sd=sd(scaled_x))
#plot the line with no axes or labels
plot(xfit, yfit, axes=FALSE, xlab='', ylab='', type='l', col='red' )
#add these now with axis
axis(4, at=pretty(range(yfit)))
# axis(1, at=pretty(seq(0,120,1)))
min(temp_freq$Temp)

## velocity

ad_vel_con <- read.csv("output_data/05a_adult_velocity_continuous.csv")
# ad_vel_cat <- read.csv("output_data/05a_adult_velocity_categorical.csv") ## bottom velocity
juv_vel_con <- read.csv("output_data/05a_juvenile_velocity_continuous.csv")
# juv_vel_cat <- read.csv("output_data/05a_juvenile_velocity_categorical.csv") ## bottom velocity
sp_vel_con <- read.csv("output_data/05a_spawning_temperature_continuous.csv")
sp_vel_conx <- sp_vel_con[,c(4,16)]
sp_vel_conx$Velocity <- sp_vel_conx$Current..m.sec.
sp_vel_conx$Current..m.sec. <- NULL
sp_vel_conx$Abundance <- 1
sp_vel_conx$Dataset <- "Saiki"
sp_vel_conx <- na.omit(sp_vel_conx)
write.csv(sp_vel_conx, "output_data/08a_spawning_velocity_abundance")
## adult
ad_vel_con2 <- subset(ad_vel_con, Dataset=="Saiki")
ad_vel_con2


## uncount frequency
vel_freq <- ad_vel_con2 %>% 
  uncount(Abundance)
hist(vel_freq$Velocity)


## compare different data sets
### get numbers for datasets


#### curve
mean(vel_freq$Velocity) ## 0.3422552, -1 = 0.09ms
min(vel_freq$Velocity)
vel_freq$Scaled_Vel <-scale(vel_freq$Velocity, scale=T, center=T)
scaled_x <- vel_freq$Scaled_Vel
h <- hist(scaled_x)
h$counts=(h$counts/sum(h$counts))*100
plot(h,  xlab="Distance from mean Velocity", ylab = "Percentage of observations", main = "Adult/Velocity: Probability curve")
par(new=TRUE)
xfit<-seq(min(scaled_x),max(scaled_x),length=120)
yfit<-dnorm(xfit,mean=mean(scaled_x),sd=sd(scaled_x))
#plot the line with no axes or labels
plot(xfit, yfit, axes=FALSE, xlab='', ylab='', type='l', col='red' )
#add these now with axis
axis(4, at=pretty(range(yfit)))
# axis(1, at=pretty(seq(0,120,1)))
dnorm(-2, mean=mean(scaled_x),sd=sd(scaled_x))

## spawning
## uncount frequency
vel_freq <- sp_vel_conx %>% 
  uncount(Abundance)
hist(vel_freq$Velocity)


## compare different data sets
### get numbers for datasets


#### curve
mean(vel_freq$Velocity) ## 0.2751097, -1 = 0.09ms
min(vel_freq$Velocity)
vel_freq$Scaled_Vel <-scale(vel_freq$Velocity, scale=T, center=T)
scaled_x <- vel_freq$Scaled_Vel
h <- hist(scaled_x)
h$counts=(h$counts/sum(h$counts))*100
plot(h,  xlab="Distance from mean Velocity", ylab = "Percentage of observations", main = "Spawning/Velocity: Probability curve")
par(new=TRUE)
xfit<-seq(min(scaled_x),max(scaled_x),length=120)
yfit<-dnorm(xfit,mean=mean(scaled_x),sd=sd(scaled_x))
#plot the line with no axes or labels
plot(xfit, yfit, axes=FALSE, xlab='', ylab='', type='l', col='red' )
#add these now with axis
axis(4, at=pretty(range(yfit)))
# axis(1, at=pretty(seq(0,120,1)))
dnorm(-2, mean=mean(scaled_x),sd=sd(scaled_x))

vel_freq

## check saiki site velocity
saiki_vel <- read.csv("output_data/05a_saiki_site_adult_velocity.csv")
saiki_vel
##look at different data sets - need site info
unique(saiki_vel$Site)
ma <- saiki_vel$Site == "MWDA"
mb <- saiki_vel$Site == "MWDB"
m8 <- saiki_vel$Site == "MWD8"
sa <- saiki_vel$Site == "SGRA"
sb <- saiki_vel$Site == "SGRB"

saiki_vel$Dataset_num[ma] <- 1
saiki_vel$Dataset_num[mb] <- 1
saiki_vel$Dataset_num[m8] <- 1
saiki_vel$Dataset_num[sa] <- 2
saiki_vel$Dataset_num[sb] <- 2

attach(saiki_vel)

# create value labels
data.f <- factor(Dataset_num, levels= 1:2,
                 labels = c("Santa Ana", "San Gabriel"))


# plot densities
sm.density.compare(as.vector(Velocity), Dataset_num, xlab="Velocity")
title(main="Velocity Distribution by Site (Saiki)")

# add legend via mouse click
colfill<-c(2:(2+length(levels(data.f))))
legend(locator(1), levels(data.f), fill=colfill)

## simplified histogram

ad_depth_con <- read.csv("output_data/05a_adult_depth_continuous.csv")
ad_depth_cat <- read.csv("output_data/05a_adult_depth_categorical.csv")

## uncount data into frequency
all_depth <- rbind(ad_depth_con, ad_depth_cat)

depth_freq <- all_depth %>% 
  uncount(Abundance)
hist(depth_freq$Depth)


### centered and scaled histogram probability - simplified and saved high res

jpeg("figures/simple_prob_curve.jpg", width = 800, height = 1000)
# subset(depth_freq, Scaled_Depth >=2)
depth_freq$Scaled_Depth <-scale(depth_freq$Depth, scale=T, center=T)
scaled_x <- depth_freq$Scaled_Depth
h <- hist(scaled_x, plot=F)
h$counts=(h$counts/sum(h$counts))*100
plot(h,lty="white",xlab="Depth (cm)", ylab = "Habitat Suitability",main = "")
par(new=TRUE)
xfit<-seq(min(scaled_x),max(scaled_x),length=120)
yfit<-dnorm(xfit,mean=mean(scaled_x),sd=sd(scaled_x))
#plot the line with no axes or labels
plot(xfit, yfit, axes=FALSE,xlab="", ylab="", type='l', col='red')
dev.off()

dnorm(1, mean=mean(scaled_x),sd=sd(scaled_x))

#add these now with axis
axis(2, at=pretty(range(yfit)))

## not scaled check
# subset(depth_freq, Scaled_Depth >=2)

x <- depth_freq$Depth
h <- hist(x, plot=F)
h$counts=(h$counts/sum(h$counts))*100
plot(h,lty="white",xlab="Depth (cm)", ylab = "Probability of Suitable Habitat",main = "Adult Santa Ana Sucker")
par(new=TRUE)
xfit<-seq(min(x),max(x),length=120)
yfit<-dnorm(xfit,mean=mean(scaled_x),sd=sd(scaled_x))
#plot the line with no axes or labels
plot(xfit, yfit, axes=FALSE,xlab="", ylab="", type='l', col='red')

#add these now with axis
axis(2, at=pretty(range(yfit)))


