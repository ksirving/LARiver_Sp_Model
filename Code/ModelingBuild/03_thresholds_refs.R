## finding thrsholds 
library(tidyverse)

## all data upload
smea <- read.csv("output_data/00_SMEA_depth_adult.csv")
wulff <- read.csv("output_data/00_Wulff_depth_abundance.csv")
thomp <- read.csv("output_data/00_Thompson_all_data_clean.csv") # bottom velocity
saiki <- read.csv("input_data/abundance_env_vars_saiki_2000.csv")
envicraft <- read.csv("output_data/00_Envicraft_2010_Temp_abundance.csv")
sawa <- read.csv("output_data/00_SAWA_2014_env_hab_abundance.csv")
## clean, subset into life stage and variables
head(saiki)
colnames(saiki)[6:15] <- c("Fish", "SL", "TL", "Weight", "Sex", "Life_stage", "Spawning", "Temp", "Depth_m", "Current_m_sec")
subset(saiki, Life_stage == "Larvae") #16.51
subset(saiki, Life_stage == "Juvenile")

ad_saiki0 <- subset(saiki, Life_stage ==c("0+ adult"))
ad_saiki1 <- subset(saiki, Life_stage ==c("1+ adult"))
ad_saiki2 <- subset(saiki, Life_stage ==c("2+ adult"))

ad_saiki <- rbind(ad_saiki0, ad_saiki1, ad_saiki2)
ad_saiki_sp <- subset(ad_saiki, Spawning =="N")

ad_saiki_temp <- ad_saiki_sp[, c(1:13)]
ad_saiki_dep <- ad_saiki_sp[, c(1:12, 14)]
ad_saiki_vel <- ad_saiki_sp[, c(1:12, 15)]


sum(is.na(ad_saiki_dep)) # 405
ad_saiki_dep <- na.omit(ad_saiki_dep)
sum(is.na(ad_saiki_temp)) # 79
ad_saiki_temp <- na.omit(ad_saiki_temp)
sum(is.na(ad_saiki_vel)) # 408
ad_saiki_vel <- na.omit(ad_saiki_vel)

## temperature

##### temperature

setwd("/Users/katieirving/Documents/git/flow_eco_mech")


# saiki <- read.csv("input_data/Saiki_2000_temp_life_stage.csv")


envicraft
sawa <- sawa[, c(2,3,13, 15)]
sawa


## general - <22c
## can survive 38c

# h.	0 – 80mm : 0+ yrs., SL 81 – 120mm : 1+ yrs, and SL 121mm+ : 2+ yrs. (sawa 2014, saiki 2000)

range(sawa$Temp)

names(sawa)
ab_temp <- sawa[,c(3,4)]


ab_temp <- na.omit(ab_temp)
sum(ab_temp$abundance)
head(ab_temp)



tem_freq <- ab_temp %>% 
  uncount(abundance)
str(tem_freq)
tem_freq
hist(tem_freq$Temp) # 
quantile(tem_freq$Temp) ## higher than others but extremes the same
# 0%  25%  50%  75% 100% 
# 20.2 23.9 26.1 28.3 28.3 

# 18.0 28.3 - backed up by others e.g. less than 30 degrees
# min 15.9 degrees - envicraft
# saiki may have some for life stage
range(ad_saiki_temp$Temp)
## 6.93 29.24


# breeding - 22-25 (SMEA)
# adults - <28
# fry/eggs - 18-24
# juvenile - 15-22 (to avoid warmth) Feeney and Swift
# survival - <30
# 
# breeding - 0 (<15 >28), 1 (15-18, 26-27), 2 (22-25)
# fry/eggs - 0 (<10 >30), 1 (10-17, 25-30), 2 (18-24)
# juvenile - 0 (<5 >28), 1 (5-14, 23-28), 2 (15-22)
# adults - 0 (>28), 1 (22-28), 2 (<22)

quantile(ad_saiki_temp$Temp) # from df created below
#0%    25%    50%    75%   100% 
# 6.930 10.800 16.800 20.765 28.610 

# add together
t_sk_df <- as.data.frame(ad_saiki_temp$Temp)
names(t_sk_df) <- "Temp"
t_sk_df$dataset <- "Saiki"
t_sw_df <- as.data.frame(tem_freq$Temp)
names(t_sw_df) <- "Temp"
t_sw_df$dataset <- "SAWA"


all_df <- rbind(t_sk_df, t_sw_df)
dim(all_df) # 956
head(all_df)

hist(all_df$Temp)
quantile(all_df$Temp)

write.csv(all_df, "output_data/03_saiki_sawa_temp_frequency.csv")
# 0%   25%   50%   75%  100% 
# 6.93 12.63 20.23 23.90 28.61 

### depth ##################

# adult
# upload data
# smea_2003 <- read.csv("output_data/00_SMEA_depth_adult_2003.csv")
# smea_2004 <- read.csv("output_data/00_SMEA_depth_adult_2004.csv")



hist(ad_saiki_dep$Depth_m)
quantile(ad_saiki_dep$Depth_m)
# 0%  25%  50%  75% 100% 
# 0.04 0.22 0.30 0.41 1.20 

#### Wulff
range(wulff$Depth_cm) # adult 20 - 120cm
hist(wulff$Depth_cm)
quantile(wulff$Depth_cm)
 
names(wulff)
ab_depth <- wulff[,c(9,10)]


ab_depth
sum(ab_depth$Number)

dep_freq <- ab_depth %>% 
  uncount(Number)
str(dep_freq)

hist(dep_freq$Depth_cm)
quantile(dep_freq$Depth_cm)
# 0%  25%  50%  75% 100% 
# 20   38   46   50  120 

### thompson
names(thomp)
range(thomp$Depth_m) # 0.10 0.33
plot(thomp$ab_mean~thomp$Depth_m)

ab_depth_th <- thomp[,6:7]
ab_depth_th
dep_freq_th <- ab_depth_th %>%
  uncount(ab_mean)
dep_freq_th
hist(dep_freq_th$Depth_m)
quantile(dep_freq_th$Depth_m)

# 0%  25%  50%  75% 100% 
# 0.10 0.18 0.19 0.26 0.33 

smea_ab_03 <- read.csv("output_data/00_SMEA_adult_depth_2003_abundance.csv")
smea_ab_04 <- read.csv("output_data/00_SMEA_adult_depth_2004_abundance.csv")

smea_ab_03

smea_ab <- cbind(smea_ab_04[,c(2,6,7)], smea_ab_03[,c(2,9)])
smea_ab$all_sites_2004 <-  smea_ab[,2] + smea_ab[,3]
smea_ab$abundance <- smea_ab$all_sites_ab + smea_ab$all_sites_2004

ab_depth_sm <- smea_ab[-16, c(4,7)]
dep_freq_sm <- ab_depth_sm %>% 
  uncount(abundance)
dep_freq_sm 
plot(dep_freq_sm$Depth)
# quantile(dep_freq_sm$Depth) ## over 30 used more

### all data

d_th_df <- as.data.frame(dep_freq_th$Depth_m)
d_wf_df <- as.data.frame(dep_freq$Depth_cm)
d_sk_df <- as.data.frame(ad_saiki_dep$Depth_m)
names(d_th_df) <- "depth"
names(d_wf_df) <- "depth"
names(d_sk_df) <- "depth"
d_wf_df$depth <- d_wf_df$depth/100

d_all_df <- rbind(d_th_df, d_wf_df, d_sk_df)
dim(d_all_df) # 1938


hist(d_all_df$depth)
quantile(d_all_df$depth)
# brown <- read.csv("output_data/00_Brown_2000_abundance_env_vars.csv") # p/a data perhaps not reliable/useful

## juveniles 


subset(saiki, Life_stage == "Juvenile") # 0.2 - 0.39

# 25-45 cm - SMEA
# backed up by saiki & 

smea_03 <- read.csv("output_data/00_SMEA_juvenile_depth_2003_abundance.csv")
smea_04 <- read.csv("output_data/00_SMEA_juvenile_depth_2004_abundance.csv")
smea_04
smea_ab <- cbind(smea_04[,c(2,6,7)], smea_03[,c(2,10)])
smea_ab
smea_ab$all_sites_2004 <-  smea_ab[,2] + smea_ab[,3]
smea_ab$abundance <- smea_ab$all_sites_ab + smea_ab$all_sites_2004

ab_depth_sm <- smea_ab[-15, c(4,7)]
ab_depth_sm
dep_freq_sm <- ab_depth_sm %>% 
  uncount(abundance)
dep_freq_sm 
plot(dep_freq_sm$Depth)
quantile(dep_freq_sm$Depth) ## over 11 used more

# larvae

subset(saiki, Life_stage == "Larvae") #16.51

# 5-10 - feeney and swift

## spawning

##### velocity
## saiki - Current_m_sec 
## Velocity_0.6_ms used - mid column??

quantile(ad_saiki_vel$Current_m_sec) # 
# 0%      25%      50%      75%     100% 
# 0.000000 0.130683 0.310642 0.450088 1.243787  
thomp_vel <- thomp[,c(6, 13)]

smea_03 <- read.csv("output_data/00_SMEA_adult_velocity_2003_abundance.csv")
smea_04 <- read.csv("output_data/00_SMEA_adult_velocity_2004_abundance.csv")
vel_17 <- read.csv("output_data/00_Wulff_2017_velocity_abundance.csv")
vel_16 <- read.csv("output_data/00_Wulff_2016_velocity_abundance.csv")
vel_15 <- read.csv("output_data/00_Wulff_2015_velocity_abundance.csv")

### wulff
vel_15 <- vel_15[, c(9,11)] %>%
  na.omit(vel_15[, c(9,11)])

vel_16 <- vel_16[, c(10,12)] %>%
  na.omit(vel_16[, c(10,12)])


vel_17 <- vel_17[, c(6,9)] %>%
  na.omit(vel_17[, c(6,9)])

names(vel_15)[1] <- "Count"
names(vel_16)
names(vel_17)

wulff_vel <- rbind(vel_15, vel_16, vel_17)
wulff_vel
vel_freq_wf <- wulff_vel %>%
  uncount(Count)
hist(vel_freq_wf$Velocity_0.6_ms)
quantile(vel_freq_wf$Velocity_0.6_ms )
# 0%    25%    50%    75%   100% 
# 0.1330 0.5500 0.6700 0.8875 1.5700 

# Saiki

sum(is.na(ad_saiki_vel)) # 404
ad_saiki_vel <- na.omit(ad_saiki_vel)
hist(ad_saiki_vel$Current_m_sec)
quantile(ad_saiki_vel$Current_m_sec) # 
# 0%      25%      50%      75%     100% 
# 0.000000 0.130683 0.310642 0.450088 1.243787  

### Thompson
thomp_vel
vel_freq_th <- thomp_vel %>%
  uncount(ab_mean)
hist(vel_freq_th$Mid._column_velocity_)
quantile(vel_freq_th$Mid._column_velocity_)
# 0%  25%  50%  75% 100% 
# 0.11 0.23 0.48 0.53 0.64 

# FWS - bottom velocity more than 0.03m/s
# smea_04 # bottom velocity
thomp_vel # mid column velocity m/s
wulff_vel # velocity 0.6 m/s
ad_saiki_vel # current m/s

## join together

wf_df <- as.data.frame(vel_freq_wf$Velocity_0.6_ms)
th_df <- as.data.frame(vel_freq_th$Mid._column_velocity)
sk_df <- as.data.frame(ad_saiki_vel$Current_m_sec)

names(wf_df) <- "velocity"
names(th_df) <- "velocity"
names(sk_df) <- "velocity"

## combine
all_df <- rbind(wf_df, th_df, sk_df)
dim(all_df) ## 2269 

hist(all_df$velocity)
quantile(all_df$velocity)
# 0%       25%       50%       75%      100% 
# 0.0000000 0.3494024 0.5100000 0.6400000 1.5700000 
