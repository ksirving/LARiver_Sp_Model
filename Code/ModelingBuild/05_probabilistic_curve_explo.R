### probablistic curve exploration

## Depth - adult has the most data to take a first stab

library(tidyverse)
library(dplyr)

setwd("/Users/katieirving/Documents/git/flow_eco_mech")

## depth data

## all data upload
dep_ad_03 <- read.csv("output_data/00_SMEA_adult_depth_2003_abundance.csv")
dep_ad_04 <- read.csv("output_data/00_SMEA_adult_depth_2004_abundance.csv")
# wulff <- read.csv("output_data/00_Wulff_depth_abundance.csv")
# thomp <- read.csv("output_data/00_Thompson_all_data_clean.csv") # bottom velocity
# saiki <- read.csv("input_data/abundance_env_vars_saiki_2000.csv")
# envicraft <- read.csv("output_data/00_Envicraft_2010_Temp_abundance.csv")
# sawa <- read.csv("output_data/00_SAWA_2014_env_hab_abundance.csv")
# 
# 
# ## clean, subset into life stage and variables
# head(saiki)
# colnames(saiki)[6:15] <- c("Fish", "SL", "TL", "Weight", "Sex", "Life_stage", "Spawning", "Temp", "Depth_m", "Current_m_sec")
# subset(saiki, Life_stage == "Larvae") #16.51
# subset(saiki, Life_stage == "Juvenile")

# workflow - sep for each variable & life stage
#1 - decide on category for data
#2 - decide on trust in source of data
 # criteria # a) location - similarity to la river, b) type of study (observation, field manipulation, ultilization)

# 3.	Single field-based manipulations (often not completely controlled)
# a.	Binned results 
# b.	Continuous response curves

## any categorized field data. SMEA is a good candidate for this approach as
# 1 - categorised
# 2 - includes "absences" - i.e. habitat utlization 

head(dep_ad_03)
head(dep_ad_04)
dep_ad_04


# format to character and change names

## how to calculate mean & variance of categorised data?
## how to calulate threshold
## how to relate variance to probability
## how to incorporate different sites/years - mixed model??
  ## what are the differences between sites?
  ## why aren't the sites the same between years?


# https://www.displayr.com/how-to-calculate-an-average-value-from-categorical-data/
#https://www.southampton.ac.uk/passs/confidence_in_the_police/multivariate_analysis/simple_linear_regression_several_categories.page

## options for analysis
#1) add all data years and sites together - accumulatively (add together) - n=15 (too low?) - this one doesn't work - see below
#2) add all data years and sites together - continously (add as rows)
#3) treat each site, each year separatley - similar to mixed model, 
      #but not as do not have 4 levels with reps in each
#4) use all_sites - all sites added per year, treat year as separate
#5) mioxed model wioth velocity data? and substrate? just for fun!

# 1 & 2 - both options add data together, if response is presence/absence then it makes more sense to add
# Option 1
## convert abundance to presence absence per category
dep_ad_04
dep_ad_03
# smea_ab$Depth
## make df with only abundance data
smea_ab <- cbind(dep_ad_04[,c(2,6,7)], dep_ad_03[,c(2,10)])
## add 2004 sites together
smea_ab$all_sites_2004 <-  smea_ab[,2] + smea_ab[,3]

# add years together
smea_ab$abundance <- smea_ab$all_sites_ab + smea_ab$all_sites_2004
# depth and abundance only, remove abundance row
ab_depth_sm <- smea_ab[-16, c(4,7)]
ab_depth_sm$Depth_mid <- c(2.5, 7.5, 12.5, 17.5, 22.5, 27.5, 32.5, 37.5, 42.5, 47.5, 52.5, 57.5, 62.5, 67.5, 71)
ab_depth_sm

# # ## count frequency of observations in each category
# dep_freq_sm <- ab_depth_sm %>%
#   uncount(abundance)
# dep_freq_sm
# plot(dep_freq_sm$Depth)
# str(ab_depth_sm)
# hist(dep_freq_sm$Depth_mid)
# mean(dep_freq_sm$Depth_mid) # 57.81657
# ## get mid of each category
# unique(dep_freq_sm$Depth)
# 


# # add presence column
# dep_freq_sm$presence <- paste("1")
# dep_freq_sm$presence
ab_depth_sm$presence <-ifelse(ab_depth_sm$abundance == 0, 0,1)
ab_depth_sm
summary(glm(ab_depth_sm$presence ~ ab_depth_sm$Depth, family=binomial()))
summary(glm(ab_depth_sm$abundance ~ ab_depth_sm$Depth, family=poisson()))

plot(ab_depth_sm$Depth,ab_depth_sm$abundance)
## plot of depth and abundance (total)

## option 2 - bind together all sites and years, create dummy data
## r bind all site info
dep_ad_04
dep_ad_03
#2003
# select only abundance 
s_2003 <- select(dep_ad_03, ends_with("_ab"), -starts_with("all_s"), matches("Depth")) 
# remove total abundance row
s_2003 <- s_2003[-16,]
# add mid category column
s_2003$Depth_mid <- c(2.5, 7.5, 12.5, 17.5, 22.5, 27.5, 32.5, 37.5, 42.5, 47.5, 52.5, 57.5, 62.5, 67.5, 71)
s_2003
# change column names to match - maybe add sites as a categorical variable?
colnames(s_2003)[1:3] <- "abundance"
# bind rows together
sall_2003 <- rbind(s_2003[,c(4,5,1)], s_2003[,c(4,5,2)], s_2003[,c(4,5,3)])
str(sall_2003)

# summary(lm(sall_2003$abundance~sall_2003$Depth))

# select only abundance 
s_2004 <- select(dep_ad_04, ends_with("_ab"),  matches("Depth")) 
# remove total abundance row
s_2004 <- s_2004[-16,]
# add mid category column
s_2004$Depth_mid <- c(2.5, 7.5, 12.5, 17.5, 22.5, 27.5, 32.5, 37.5, 42.5, 47.5, 52.5, 57.5, 62.5, 67.5, 71)

s_2004
# change column names to match - maybe add sites as a categorical variable?
colnames(s_2004)[1:2] <- "abundance"
# bind rows together
sall_2004 <- rbind(s_2004[,c(3,4,1)], s_2004[,c(3,4,2)])
str(sall_2004)

all_smea <- rbind(sall_2003, sall_2004)
all_smea

## create presence/ absence column - this is the response variable

all_smea$presence <-ifelse(all_smea$abundance == 0, 0,1)


## dummy predictor variables 

## example with R data
# library(car)
# # Load the data
# data("Salaries", package = "car")
# # Inspect the data
# sample_n(Salaries, 3)
# # Compute the model
# model <- lm(salary ~ sex, data = Salaries)
# summary(model)$coef
# contrasts(Salaries$sex)
# 
# res <- model.matrix(~rank, data = Salaries)
# head(res[, -1])
# 
# model2 <- lm(salary ~ yrs.service + rank + discipline + sex,
#              data = Salaries)
# Anova(model2)

### depth data

# ab_lm <- lm(all_smea$abundance~all_smea$Depth)

all_smea

# GLM on presence absence vs categorical depth 
# treating all sites and years as separate samples to increase n
pr_glm <- glm(presence~Depth, data=all_smea,family=binomial(link = "logit"))
# summary(pr_glm$fitted.values) # 0.3867
# pr_glm$coefficients
# pr_glm$rank
# pr_glm$residuals
# pr_glm$linear.predictors
# sd(pr_glm$residuals)
summary(pr_glm)

## get probability of occurecne from GLM
## create depth values - new data
xdepth <- all_smea$Depth
## prediction
ydepth <- predict(pr_glm, list(Depth = xdepth), type="response")
## plot data - logistica curve
plot(all_smea$Depth, all_smea$presence, pch = 16, xlab = "Depth (m)", ylab = "Presence")
lines(xdepth, ydepth)
# length(xdepth)
# length(ydepth)
# xdepth
# ydepth
library(ResourceSelection)
hoslem.test(all_smea$presence, fitted(pr_glm))

## plot probability values to find threshold
prob_occs <- predict(pr_glm, list(Depth = xdepth), type="response")
pred_df <- data.frame(all_smea$Depth,prob_occs)
pred_df
plot(pred_df)
## every depth above 0.5 is a presence
abline(0.5,0)

## logistic curve for each peak
#
high_pred_df <- filter(pred_df, prob_occs > 0.49)
high_pred_df

unique(high_pred_df$all_smea.Depth)
# 31-35  36-40  41-45  46-50  51-55  56-60  71+   

## cannot plot the categorical variables in logistic curve
## try with mid point as numerical data
## glm with mid points
pr_glm_mid <- glm(presence~Depth_mid, data=all_smea,family=binomial(link = "logit"))
summary(pr_glm_mid)

## get probability of occurecne from GLM
## create depth values - new data
range(all_smea$Depth_mid) # 2.5 71.0 - 71+ do not know max value
xdepth <- seq(0,90,0.1)
## prediction
ydepth <- predict(pr_glm_mid, list(Depth_mid = xdepth), type="response")
## plot data - logistica curve
plot(all_smea$Depth_mid, all_smea$presence, pch = 16, xlab = "Depth (m)", ylab = "Presence")
lines(xdepth, ydepth)
abline(0.5,0)

all_smea$presence
all_smea

## get probability of occurecne from GLM
## create depth values - new data
range(all_smea$Depth_mid) # 2.5 71.0 - 71+ do not know max value
xdepth <- seq(0,50,0.1)
## prediction
ydepth <- predict(pr_glm_mid, list(Depth_mid = xdepth), type="response")
## plot data - logistica curve
plot(all_smea$Depth_mid, all_smea$presence, pch = 16, xlab = "Depth (m)", ylab = "Presence")
lines(xdepth, ydepth)
abline(0.5,0)

all_smea$presence
all_smea

library(ResourceSelection)
hoslem.test(all_data$presence, fitted(ad_dep_glm))

#####################################

# 
# lm_pred <- lm(prob_occs~all_smea.Depth, data=pred_df)
# # probs <- 
# pred_df$all_smea.Depth
# contrasts(droplevels(all_smea$Depth))
# 
# ## variables are n=15 - can only apply to total abundance
# 
# summary(ab_lm$fitted.values)
# ## gives mean of response
# mean(all_smea$abundance)
# 
# ## model with n=15
# 
# ab_lm <- lm(abundance~Depth, data=ab_depth_sm)
# summary(ab_lm)
# contrasts(droplevels(ab_depth_sm$Depth))
# summary(glm(presence~Depth, data=ab_depth_sm, family=binomial(link = "logit")))
### does not work with small sample size

##Figure 1. Probability of a site having a true mean CSCI score above a threshold 
# (e.g., ≥ 0.79), based on a single observation and assuming a within-site standard 
# deviation of 0.11 (as reported in Mazor et al. 2016). 

## smea categorical data does not have repeated site visits to find the mean, 
## also he has a continous variable
## also threshold is already known

## theshold could be > 3cm 


## continuous data

# workflow - sep for each variable & life stage
#1 - decide on category for data
#2 - decide on trust in source of data
# criteria # a) location - similarity to la river, b) type of study (observation, field manipulation, ultilization)


# b.	Continuous data – plot percent mortality/occurrence vs. physical variable


library(tidyverse)
library(dplyr)

setwd("/Users/katieirving/Documents/git/flow_eco_mech")

wulff <- read.csv("output_data/00_Wulff_depth_abundance.csv")
thomp <- read.csv("output_data/00_Thompson_all_data_clean.csv") # bottom velocity
saiki <- read.csv("input_data/abundance_env_vars_saiki_2000.csv") ## santa ana / san gabriel
# envicraft <- read.csv("output_data/00_Envicraft_2010_Temp_abundance.csv")
sawa <- read.csv("output_data/00_SAWA_2014_env_hab_abundance.csv")
# 
head(saiki)
## IMP = Santa Ana River - Imperial Highway - no fish presences
## MWDA = Santa Ana River - Metropolitan pipeline
## SGR = San Gabriel River

## 5 pass electrofishing, 5 sampling trips at 3 month intervals
## env varioables measured where ever fish captured
## depth and velocity measured at 5 intervals over cross section
## no absence data


dim(saiki) # 715 

adults <- droplevels(unique(saiki$Life.Stage)[1:3])
adults
# saiki_juv <- subset(saiki, Life.Stage == "Juvenile")
# saiki_juv
## extract adults that aren't spawning (in spawning condition)

saiki_adult <- filter(saiki, Life.Stage %in% adults & Spawning..Y.N. == "N") 
dim(saiki_adult) ## 687

# clean data
saiki_adult$Site <- ifelse(saiki_adult$Site=="MWD8", paste("MWDB"), paste(saiki_adult$Site))
saiki_adult$Site
colnames(saiki_adult)[14] <- "Depth"

## extract depth 
saiki_adult_depth <- select(saiki_adult, Site, Date, Depth)
saiki_adult_depth
dim(saiki_adult_depth) 
## apply weights
saiki_adult_depth$weight <- ifelse(saiki_adult_depth$Site =="SGRA" | saiki_adult_depth$Site == "SGRB", 5 , 10)
saiki_adult_depth <- na.omit(saiki_adult_depth) # 348 NAs removed
# subset(saiki_adult_depth, Site =="SGRB")
# sum(is.na(saiki_adult_depth$Depth..m.))
saiki_adult_depth
dim(saiki_adult_depth) # 339
str(saiki_adult_depth)
## what model? GLM = no absences

## add random pseudo absences
## 60 values of depth between 
max(saiki_adult_depth$Depth) # 1.2
min(saiki_adult_depth$Depth) # 0.04
x <- seq(0.04,1.2, 0.01)

ran_dep <- sample(x,60)
ran_dep
unique(saiki_adult_depth$Site)
psu_df <- data.frame(matrix(ncol=4, nrow=60))
colnames(psu_df) <- colnames(saiki_adult_depth)
colnames(saiki_adult_depth)
colnames(psu_df)
psu_df$Depth<- ran_dep
psu_df$Site <- "pseudo_site"
psu_df$Date <- "pseudo_date"
psu_df$weight <- 10

## combine datasets
all_data <- rbind(saiki_adult_depth, psu_df)


## add presence/absence
all_data$presence <- ifelse(all_data$Site == "pseudo_site", 0, 1)
all_data$presence <- as.integer(all_data$presence)
all_data$weight <- as.integer(all_data$weight)
all_data
# tail(all_data)
# head(all_data)
# str(all_data)
# unique(all_data$presence)
## glm

ad_dep_glm <- glm(presence~Depth, data=all_data, family=binomial, weights=weight)
ad_dep_glm_zero <- glm(presence~Depth-1, data=all_data, family=binomial, weights=weight)

summary(ad_dep_glm$fitted.values) # 
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.4010  0.9721  0.9871  0.9600  0.9917  0.9967

summary(ad_dep_glm_zero$fitted.values)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.6051  0.9082  0.9608  0.9410  0.9918  1.0000 
summary(ad_dep_glm)
install.packages("ResourceSelection")
library(ResourceSelection)
hoslem.test(all_data$presence, fitted(ad_dep_glm))
## plot predictions

range(all_data$Depth)
# create range of depth values
xdepth  <- seq(0,1.5, 0.0005)

ydepth <- predict(ad_dep_glm, list(Depth = xdepth), type="response")
plot(all_data$Depth, all_data$presence, pch = 16, xlab = "Depth (m)", ylab = "Presence")
lines(xdepth, ydepth)
length(xdepth)
length(ydepth)

ydepthz <- predict(ad_dep_glm_zero, list(Depth = xdepth), type="response")
plot(all_data$Depth, all_data$presence, pch = 16, xlab = "Depth (m)", ylab = "Presence")
lines(xdepth, ydepthz)
length(xdepth)
length(ydepthz)

#####################
## logistic curve forced through origin - zero intercept model _ 
## DOES NOT CONVERGE WITHOUT ZEROS!!!!!!
## saiki_adult_depth = df without pseudo absences
saiki_adult_depth
# add presences
saiki_adult_depth$presence <- 1
# format to integer
saiki_adult_depth$presence <- as.integer(saiki_adult_depth$presence)
saiki_adult_depth$weight <- as.integer(saiki_adult_depth$weight)

ad_dep_glm_zero <- glm(presence~Depth, data=saiki_adult_depth, 
                       family=binomial, weights=weight)
bar <- update(ad_dep_glm_zero, ~ . -1)

summary(ad_dep_glm_zero$fitted.values) # 
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.01667 0.65503 0.82084 0.72724 0.88976 0.96161 

summary(ad_dep_glm_zero)
## plot predictions

range(saiki_adult_depth$Depth)
# create range of depth values
xdepth  <- seq(0.04,2, 0.0005)

ydepth <- predict(ad_dep_glm, list(Depth = xdepth), type="response")
plot(all_data$Depth, all_data$presence, pch = 16, xlab = "Depth (m)", ylab = "Presence")
lines(xdepth, ydepth)
length(xdepth)
length(ydepth)


### combine data

library(tidyverse)
library(dplyr)

setwd("/Users/katieirving/Documents/git/flow_eco_mech")


wulff <- read.csv("output_data/00_Wulff_depth_abundance.csv")
thomp <- read.csv("output_data/00_Thompson_all_data_clean.csv") # bottom velocity
saiki <- read.csv("input_data/abundance_env_vars_saiki_2000.csv") ## santa ana / san gabriel
# envicraft <- read.csv("output_data/00_Envicraft_2010_Temp_abundance.csv")
sawa <- read.csv("output_data/00_SAWA_2014_env_hab_abundance.csv")


dim(saiki) # 715 

adults <- droplevels(unique(saiki$Life.Stage)[1:3])
adults
# saiki_juv <- subset(saiki, Life.Stage == "Juvenile")
# saiki_juv
## extract adults that aren't spawning (in spawning condition)

saiki_adult <- filter(saiki, Life.Stage %in% adults & Spawning..Y.N. == "N") 
dim(saiki_adult) ## 687

# clean data
saiki_adult$Site <- ifelse(saiki_adult$Site=="MWD8", paste("MWDB"), paste(saiki_adult$Site))
saiki_adult$Site
colnames(saiki_adult)[14] <- "Depth"

## extract depth 
saiki_adult_depth <- select(saiki_adult, Site, Date, Depth)
saiki_adult_depth
dim(saiki_adult_depth) 
## apply weights
saiki_adult_depth$weight <- ifelse(saiki_adult_depth$Site =="SGRA" | saiki_adult_depth$Site == "SGRB", 5 , 10)
saiki_adult_depth <- na.omit(saiki_adult_depth) # 348 NAs removed




head(wulff) ## cm has coordinates
## do not know number of passes for electrofishing, seining also used. aim of study to compare 
## methods so assume ok data
unique(wulff$Section) ## santa ana river
# weighting = 1

head(thomp) # m
# 3 pass electrofishing/removal method & capture probability applied
# santa ana river
# no coordinates - may be able to cllect with published map and clever GIS work
# mean depth of cross sections
# weighting = 0.7 

head(sawa)
dim(sawa)
## has max depth & edge depth
## santa ana river
## no coordinates but could work them out from map
## 1 pass seine
## has absences
## weighting 1
# which(is.na(sawa)) ## 0+ fish
# sum(sawa$abundance)
# ab <- na.omit(sawa$abundance)
# sum(ab)
sawap <- na.omit(sawa)
## remove absences for now - do separate glm later for curiosity
sawap # cm
head(saiki_adult_depth) # m
saiki_adult_depth$dataset <- "Saiki"
## extract only columns needed and format depth & add weighting
wulff <- wulff[,c(9:10)]
wulff$Depth <- wulff$Depth_cm/100
wulff$weight <- 10
wulff$dataset <- "Wulff"
head(wulff)

thomp <- thomp[,c(1:2,7)]
thomp$weight <- 7
thomp$dataset <- "Thompson"
thomp

sawap <- sawap[,c(2:3,5)]
sawap$Depth <- sawap$Max_depth_cm/100
sawap$dataset <- "SAWA"
sawap$weight <- 10
sawap
#### all data merge

# column names - only really need depth and weight
head(saiki_adult_depth)
saiki_adult_depthw <- saiki_adult_depth[, c(3:4,5)]

head(wulff)
wulffw <- wulff[, c(3:4,5)]
head(thomp)
thomp <- thomp[, c(3:4,5)]
sawap
sawap <- sawap[, c(4,6,5)]

colnames(saiki_adult_depthw)
colnames(wulffw)
colnames(thomp)
colnames(sawap)
colnames(thomp) <- colnames(wulffw)
colnames(sawap) <- colnames(wulffw)


## combine data

all_data <- rbind(saiki_adult_depthw, wulffw, thomp,sawap)
head(all_data)
dim(all_data) ## 501

## glm with all data 
## add absences
range(all_data$Depth) # 0.04 1.2
set.seed(678)
# ?set.seed
x <- seq(0,1.2, 0.01)
## absences
ran_dep <- sample(x,100)
ran_dep

psu_df <- data.frame(matrix(ncol=3, nrow=100))
colnames(psu_df) <- colnames(all_data)
# colnames(all_data)
# colnames(psu_df)
psu_df$Depth<- ran_dep
psu_df$dataset <- "psuedo"
psu_df$weight <- 10
colnames(psu_df) <- colnames(all_data)

## combine datasets
all_data <- rbind(all_data, psu_df)


## add presence/absence
all_data$presence <- ifelse(all_data$dataset == "psuedo", 0, 1)
all_data$presence <- as.integer(all_data$presence)
all_data$weight <- as.integer(all_data$weight)
all_data
tail(all_data)
# head(all_data)
str(all_data)
# unique(all_data$presence)

###################
## glm

ad_dep_glm <- glm(presence~Depth, data=all_data, family=binomial, weights=weight)

# ad_dep_glm_ds <- glm(presence~Depth + dataset, data=all_data, family=binomial, weights = weight)
# 
# unique(all_data$dataset) 
# Warning message:
#   glm.fit: algorithm did not converge

ad_dep_glm_zero <- glm(presence~Depth-1, data=all_data, family=binomial, weights=weight)

summary(ad_dep_glm$fitted.values) # 
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.5281  0.8815  0.9079  0.8976  0.9289  0.9592 

summary(ad_dep_glm_zero$fitted.values)
library(ResourceSelection)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.6051  0.9082  0.9608  0.9410  0.9918  1.0000 
summary(ad_dep_glm)
summary(ad_dep_glm_ds)
summary(ad_dep_glm_zero)

library(ResourceSelection)
hoslem.test(all_data$presence, fitted(ad_dep_glm))
hoslem.test(all_data$presence, fitted(ad_dep_glm_zero))

## plot predictions
all_data$Depth
range(all_data$Depth)
# create range of depth values
xdepth  <- seq(0,1.5, 0.0005)

ydepth <- predict(ad_dep_glm, list(Depth = xdepth), type="response")
plot(all_data$Depth, all_data$presence, pch = 16, xlab = "Depth (m)", ylab = "Presence")
## 

lines(xdepth, ydepth)
length(xdepth)
length(ydepth)

ydepthz <- predict(ad_dep_glm_zero, list(Depth = xdepth), type="response")
plot(all_data$Depth, all_data$presence, pch = 16, xlab = "Depth (m)", ylab = "Presence")
lines(xdepth, ydepthz)
length(xdepth)
length(ydepthz)


## glm with sawa data for fun!!!

head(sawa_dep)
sawa_dep <- sawa[, c(5,13,14,15)]
sawa_dep$Depth <- sawa_dep$Max_depth_cm/100

## add presence/absence
sawa_dep
sawa_dep$presence <- ifelse(is.na(sawa_dep$abundance), 0, 1)
sawa_dep$presence <- as.integer(sawa_dep$presence)

colnames(sawa_dep)[1] <- "Depth"

####################
# GLM
sawa_glm <- glm(presence~Depth, data=sawa_dep, family=binomial)

summary(sawa_glm$fitted.values) # 
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# .2198  0.2621  0.2902  0.2895  0.3049  0.4301 

summary(sawa_glm)

hoslem.test(sawa_dep$presence, fitted(sawa_glm))

## plot predictions
sawa_dep$Depth
range(sawa_dep$Depth)
# create range of depth values
xdepth  <- seq(0,1.5, 0.0005)

ydepth <- predict(sawa_glm, list(Depth = xdepth), type="response")
plot(sawa_dep$Depth, sawa_dep$presence, pch = 16, xlab = "Depth (m)", ylab = "Presence")
## 

lines(xdepth, ydepth)
length(xdepth)
length(ydepth)

# GLM with temp
sawa_glm_t <- glm(presence~Depth*Temp, data=sawa_dep, family=binomial)
sawa_glm_t <- glm(presence~Temp, data=sawa_dep, family=binomial)
summary(sawa_glm_t$fitted.values) # 
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.05507 0.14341 0.25545 0.28947 0.43758 0.65370 

summary(sawa_glm_t)
hoslem.test(sawa_dep$presence, fitted(sawa_glm_t))
## plot predictions
sawa_dep$Depth
range(sawa_dep$Depth)
range(sawa_dep$Temp)
# create range of depth values
xdepth  <- seq(0,1.5, 0.0005)
xtemp <- seq(15,30, 0.01)

ytemp <- predict(sawa_glm_t, list(Temp = xtemp), type="response")
plot(sawa_dep$Temp, sawa_dep$presence, pch = 16, xlab = "Temp", ylab = "Presence")
## 

lines(xtemp, ytemp)
length(xdepth)
length(ydepth)

#### just plot data

plot(all_data$Depth,all_data$presence)


