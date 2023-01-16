#### combine data for easy upload in prob curves

## upload each dataset and combine variables for each life stage

## in this format

#   Temp abundance dataset
# 1 25.6         1    SAWA
# 3 25.1        11    SAWA
# 4 23.9        71    SAWA
# 5 26.1        59    SAWA
# 6 28.3       111    SAWA
# 7 23.3         9    SAWA

library(tidyverse)
library(dplyr)


## continuous
# wulff <- read.csv("output_data/bio/00_Wulff_depth_abundance.csv")
thomp <- read.csv("output_data/bio/00_Thompson_all_data_clean.csv") # bottom velocity
saiki <- read.csv("input_data/bio/abundance_env_vars_saiki_2000.csv") ## santa ana / san gabriel
envicraft <- read.csv("output_data/bio/00_Envicraft_2010_Temp_abundance.csv")
sawa <- read.csv("output_data/bio/00_SAWA_2014_env_hab_abundance.csv")

vel_17 <- read.csv("output_data/bio/00_Wulff_2017_velocity_abundance.csv")
vel_16 <- read.csv("output_data/bio/00_Wulff_2016_velocity_abundance.csv")
vel_15 <- read.csv("output_data/bio/00_Wulff_2015_velocity_abundance.csv")


## categorical
dep_ad_03 <- read.csv("output_data/bio/00_SMEA_adult_depth_2003_abundance.csv")
dep_ad_04 <- read.csv("output_data/bio/00_SMEA_adult_depth_2004_abundance.csv")
smea_03 <- read.csv("output_data/bio/00_SMEA_adult_velocity_2003_abundance.csv")
smea_04 <- read.csv("output_data/bio/00_SMEA_adult_velocity_2004_abundance.csv")

# smea_03 <- read.csv("output_data/bio/00_SMEA_juvenile_depth_2003_abundance.csv")
# smea_04 <- read.csv("output_data/bio/00_SMEA_juvenile_depth_2004_abundance.csv")
juv_03 <- read.csv("output_data/bio/00_SMEA_juvenile_velocity_2003_abundance.csv")
juv_04 <- read.csv("output_data/bio/00_SMEA_juvenile_velocity_2004_abundance.csv")

## depth

## continuous

wulff15 <- read.csv("output_data/bio/00_Wulff_2015_depth_abundance.csv")
wulff16 <- read.csv("output_data/bio/00_Wulff_2016_depth_abundance.csv")
wulff17 <- read.csv("output_data/bio/00_Wulff_2017_depth_abundance.csv")
head(wulff15)
## wulff data 2015 & 2016

thomp <- read.csv("output_data/bio/00_Thompson_all_data_clean.csv") # bottom velocity
saiki <- read.csv("input_data/bio/abundance_env_vars_saiki_2000.csv") ## santa ana / san gabriel
sawa <- read.csv("output_data/bio/00_SAWA_2014_env_hab_abundance.csv")

# #categorical
# dep_ad_03 <- read.csv("output_data/bio/00_SMEA_adult_depth_2003_abundance.csv")
# dep_ad_04 <- read.csv("output_data/bio/00_SMEA_adult_depth_2004_abundance.csv")

## check data
 ## cm
wulff15 <- wulff15 %>%
  separate(Date, c( "month", "day", "year"))
wulff15$year <- 2015

wulff16 <- wulff16 %>%
  separate(Date, c( "month", "day", "year"))
unique(wulff15$year)

wulff17 <- wulff17 %>%
  separate(Date, c( "month", "day", "year"))
unique(wulff17$year)

head(thomp) ## meters - mean
head(saiki) ## meters
head(sawa) ## cm

## extract abundance temp and add dataset name
## also keep site as column

# Temp abundance dataset
# 1 25.6         1    SAWA
# 3 25.1        11    SAWA
# 4 23.9        71    SAWA
# 5 26.1        59    SAWA
# 6 28.3       111    SAWA
# 7 23.3         9    SAWA

names(wulff17)

wulff_dep15 <- wulff15[, c(12,11, 7)]
names(wulff_dep15) <- c("Depth", "Abundance", "Year")
wulff_dep15$Dataset <- "Wulff"
head(wulff_dep15)

wulff_dep16 <- wulff16[, c(13,12, 10)]
names(wulff_dep16) <- c("Depth", "Abundance", "Year")
wulff_dep16$Dataset <- "Wulff"
head(wulff_dep16)

wulff_dep17 <- wulff17[, c(10,8, 6)]
names(wulff_dep17) <- c("Depth", "Abundance", "Year")
wulff_dep17$Dataset <- "Wulff"
head(wulff_dep17)

wulff_dep_all <- rbind(wulff_dep15, wulff_dep16, wulff_dep17)
wulff_dep <- rbind(wulff_dep15, wulff_dep16)
unique(wulff_dep$Year)
write.csv(wulff_dep, "output_data/bio/01_wulff_depth_2015_2016.csv")

# names(thomp)
# thomp_dep <- thomp[, c(7,6)]
# names(thomp_dep) <- c("Depth", "Abundance")
# thomp_dep$Depth <- thomp_dep$Depth*100
# thomp_dep$Dataset <- "Thompson"
# head(thomp_dep)

names(saiki)
## sunset adults only
adults <- unique(saiki$Life.Stage)[1:3]
saiki_ad<- filter(saiki, Life.Stage %in% adults & Spawning..Y.N. == "N") ## may keep all in
dim(saiki_ad) ## 687

## count frequency in place of abundance (data is presence only)
saiki_dep <- saiki_ad[, c(14)]
saiki_dep <- as.data.frame(table(saiki_dep))

names(saiki_dep) <- c("Depth", "Abundance")
saiki_dep$Depth <- as.numeric(as.character(saiki_dep$Depth))
saiki_dep$Depth <- saiki_dep$Depth*100
saiki_dep$Dataset <- "Saiki"
head(saiki_dep)

names(sawa)
sawa_dep <- sawa[, c(5,15)]
names(sawa_dep) <- c("Depth", "Abundance")
sawa_dep <- na.omit(sawa_dep) ## remove NAs - absences! can use them later
sawa_dep$Dataset <- "SAWA"
sawa_dep$Depth <- round(sawa_dep$Depth, digits=0)
head(sawa_dep)

## join together 
names(wulff_dep_all)
wulff_dep_all$Year <- NULL

con_dep <- rbind(wulff_dep_all, saiki_dep,sawa_dep)
dim(con_dep) ## 227
head(con_dep)

## save dataset 

write.csv(con_dep, "output_data/bio/01_adult_depth_continuous_updated.csv")

## categorical
dep_ad_03
dep_ad_04

## add together all sites years abundance

smea_ab <- cbind(dep_ad_04[,c(2,6,7)], dep_ad_03[,c(2,10)])
## add 2004 sites together
smea_ab$all_sites_2004 <-  smea_ab[,2] + smea_ab[,3]
smea_ab
# add years together
smea_ab$abundance <- smea_ab$all_sites_ab + smea_ab$all_sites_2004
# depth and abundance only, remove abundance row
ab_depth_sm <- smea_ab[-16, c(4,7)]
ab_depth_sm$Depth_mid <- c(2.5, 7.5, 12.5, 17.5, 22.5, 27.5, 32.5, 37.5, 42.5, 47.5, 52.5, 57.5, 62.5, 67.5, 71)

ab_depth_sm <- ab_depth_sm[,c(3,2)]
names(ab_depth_sm) <- c("Depth", "Abundance")
ab_depth_sm$Depth <- round(ab_depth_sm$Depth,digits=0)
ab_depth_sm$Dataset <- "SMEA"
head(ab_depth_sm)

## save 

write.csv(ab_depth_sm, "output_data/bio/01_adult_depth_categorical.csv")


### juvenile

## continuous
saiki <- read.csv("input_data/bio/abundance_env_vars_saiki_2000.csv") ## santa ana / san gabriel

# categorical
smea_03 <- read.csv("output_data/bio/00_SMEA_juvenile_depth_2003_abundance.csv")
smea_04 <- read.csv("output_data/bio/00_SMEA_juvenile_depth_2004_abundance.csv")

## sunset juv only
unique(saiki$Life.Stage)
juv <- unique(saiki$Life.Stage)[4]

saiki_juv<- filter(saiki, Life.Stage %in% juv)
dim(saiki_juv) ## 10 - but 4 x NAs

## count frequency in place of abundance (data is presence only)
saiki_juv <- saiki_juv[, c(14)]
saiki_juv <- as.data.frame(table(saiki_juv))

names(saiki_juv) <- c("Depth", "Abundance")
saiki_juv$Depth <- as.numeric(as.character(saiki_juv$Depth))
saiki_juv$Depth <- saiki_juv$Depth*100
saiki_juv$Dataset <- "Saiki"
head(saiki_juv)


## categorical
smea_03
smea_04

## add together all sites years abundance

smea_ab <- cbind(smea_04[,c(2,8)], smea_03[,c(2,10)])
## add 2004 sites together

smea_ab
# add years together
smea_ab$abundance <- smea_ab$all_sites_ab + smea_ab$total_ab
# depth and abundance only, remove abundance row
ab_depth_sm <- smea_ab[-15, c(3,5)]
ab_depth_sm$Depth_mid <- c(2.5, 7.5, 12.5, 17.5, 22.5, 27.5, 32.5, 37.5, 42.5, 47.5, 52.5, 57.5, 62.5, 67.5)

## format as above
ab_depth_sm <- ab_depth_sm[,c(3,2)]
names(ab_depth_sm) <- c("Depth", "Abundance")
ab_depth_sm$Depth <- round(ab_depth_sm$Depth,digits=0)
ab_depth_sm$Dataset <- "SMEA"
head(ab_depth_sm)

## save both

write.csv(saiki_juv, "output_data/bio/01_juvenile_depth_continous.csv")
write.csv(ab_depth_sm, "output_data/bio/01_juvenile_depth_categorical.csv")

## fry

head(saiki)

unique(saiki$Life.Stage)
fry <- unique(saiki$Life.Stage)[5]
saiki_fry<- filter(saiki, Life.Stage %in% fry)
dim(saiki_fry) ## 1 datapoint!!!!


## spawning
names(saiki)
## sunset adults only
adults <- unique(saiki$Life.Stage)[1:3]
saiki_ad<- filter(saiki, Life.Stage %in% adults & Spawning..Y.N. == "Y") ## may keep all in
dim(saiki_ad) ## 15

## count frequency in place of abundance (data is presence only)
saiki_dep <- saiki_ad[, c(3,14)]
saiki_dep <- as.data.frame(table(saiki_dep))
saiki_dep
names(saiki_dep) <- c("Site", "Depth", "Abundance")
saiki_dep$Depth <- as.numeric(as.character(saiki_dep$Depth))
saiki_dep$Depth <- saiki_dep$Depth*100
saiki_dep$Dataset <- "Saiki"
head(saiki_dep)

write.csv(saiki_ad, "output_data/bio/01_spawning_depth_continous.csv")

#################################################


## temperature

sawa <- read.csv("output_data/bio/00_SAWA_2014_env_hab_abundance.csv")
saiki <- read.csv("input_data/bio/abundance_env_vars_saiki_2000.csv")
envicraft <- read.csv("output_data/bio/00_Envicraft_2010_Temp_abundance.csv")


sawa <- sawa[, c(2,3,13, 15)]
ab_temp <- sawa[,c(3,4)]
ab_temp <- na.omit(ab_temp)
ab_temp$Dataset <- "SAWA"
sum(ab_temp$abundance)
head(ab_temp)


saiki <- read.csv("input_data/bio/abundance_env_vars_saiki_2000.csv") ## santa ana / san gabriel
dim(saiki) # 715 
head(saiki)
adults <-unique(saiki$Life.Stage)[1:3]
adults

## extract adults that aren't spawning (in spawning condition)

saiki_adult <- filter(saiki, Life.Stage %in% adults & Spawning..Y.N. == "N") ## may keep all in
dim(saiki_adult) ## 687

# clean data
saiki_adult$Site <- ifelse(saiki_adult$Site=="MWD8", paste("MWDB"), paste(saiki_adult$Site))

colnames(saiki_adult)[13] <- "Temp"

## extract temp 
saiki_adult_temp <- select(saiki_adult, Temp)
saiki_adult_temp

## round temp to removes all the 1 abundances and standardise with sawa
saiki_adult_temp$Temp <- round(saiki_adult_temp$Temp,digits = 1)

## convert Saiki to abundance

saiki_freq <- as.data.frame(table(saiki_adult_temp$Temp))
saiki_freq
colnames(saiki_freq)<- c("Temp", "abundance")
saiki_freq$Dataset <- "Saiki"

## join together
names(ab_temp)
names(saiki_freq)

head(ab_temp)

all_df <- rbind(ab_temp, saiki_freq)
dim(all_df) # 123

head(all_df)
tail(all_df)

write.csv(all_df, "output_data/bio/01_adult_temperature_continous.csv")

##### juvenile

## sunset juv only
unique(saiki$Life.Stage)
juv <- unique(saiki$Life.Stage)[4]
saiki_juv<- filter(saiki, Life.Stage %in% juv)
dim(saiki_juv) ## 10 - but 1 x NAs
saiki_juv
## count frequency in place of abundance (data is presence only)
saiki_juv <- saiki_juv[, c(13)]
saiki_juv <- as.data.frame(table(saiki_juv))

names(saiki_juv) <- c("Temp", "Abundance")
saiki_juv$Temp<- as.numeric(as.character(saiki_juv$Temp))
saiki_juv$Temp <- round(saiki_juv$Temp, digits = 1)
saiki_juv$Dataset <- "Saiki"
head(saiki_juv)

write.csv(saiki_juv, "output_data/bio/01_juvenile_temperature_continous.csv")

### spawning

## spawning
names(saiki)
## sunset adults only
adults <- unique(saiki$Life.Stage)[1:3]
saiki_ad<- filter(saiki, Life.Stage %in% adults & Spawning..Y.N. == "Y") ## may keep all in
dim(saiki_ad) ## 15

## count frequency in place of abundance (data is presence only)
saiki_dep <- saiki_ad[, c(13)]
saiki_dep <- as.data.frame(table(saiki_dep))

names(saiki_dep) <- c("Temp", "Abundance")
saiki_dep$Temp <- as.numeric(as.character(saiki_dep$Temp))
saiki_dep$Temp <- round(saiki_dep$Temp, digits=1)
saiki_dep$Dataset <- "Saiki"
head(saiki_dep)

write.csv(saiki_ad, "output_data/bio/01_spawning_temperature_continous.csv")

############  Velocity


## continuous

thomp <- read.csv("output_data/bio/00_Thompson_all_data_clean.csv") # Mid._column_velocity - mean
saiki <- read.csv("input_data/bio/abundance_env_vars_saiki_2000.csv") ## Current..m.sec.

vel_17 <- read.csv("output_data/bio/00_Wulff_2017_velocity_abundance.csv") ## velocity at fish or velocity 0.6 ms
vel_16 <- read.csv("output_data/bio/00_Wulff_2016_velocity_abundance.csv")
vel_15 <- read.csv("output_data/bio/00_Wulff_2015_velocity_abundance.csv")

## check data
head(vel_15)
head(thomp)
head(saiki) 

## wulff
## 2 datasets - 1 with each measurement

head(vel_15)
head(vel_16)
head(vel_17)
names(vel_17)

vel_15x <- vel_15[,c(10,11,9)]

names(vel_15x) <- c("Vel_fish", "Vel_0.6", "Abundance")
cor.test(vel_15x$Vel_fish, vel_15x$Vel_0.6, method="pearson")
vel_15x$year <- 2015

vel_16x <- vel_16[,c(11,12,10)]
names(vel_16x) <- c("Vel_fish", "Vel_0.6", "Abundance")
cor.test(vel_16x$Vel_fish, vel_16x$Vel_0.6, method="pearson")
vel_16x$year <- 2016

vel_17x <- vel_17[,c(8,9,6)]
names(vel_17x) <- c("Vel_fish", "Vel_0.6", "Abundance")
cor.test(vel_17x$Vel_fish, vel_17x$Vel_0.6, method="pearson")
## moderately correlated
vel_17x$year <- 2017

## join datasets

all_wulff <- rbind(vel_15x, vel_16x, vel_17x)
all_wulff$Dataset <- "Wulff"
write.csv(all_wulff, "output_data/bio/01_velocity_wulff_2015_2016_both_vels.csv")
write.csv(all_wulff, "output_data/bio/01_velocity_wulff_all_years_both_vels.csv")
all_wulff <- read.csv("output_data/bio/01_velocity_wulff_all_years_both_vels.csv")
head(all_wulff)
dim(all_wulff)
## separate vell_0.6 to add to toher dataset

all_wulff06 <- all_wulff[,-c(1,2,5)]
head(all_wulff06)
names(all_wulff06)[1] <- "Velocity"

## Saiki

head(saiki)
adults <- unique(saiki$Life.Stage)[1:3]

saiki_adult <- filter(saiki, Life.Stage %in% adults & Spawning..Y.N. == "N") ## may keep all in
dim(saiki_adult) ## 687
names(saiki_adult)

colnames(saiki_adult)[15] <- "Velocity"

## extract velcocity 
saiki_adult_vel <- select(saiki_adult, Velocity, Site)
saiki_adult_vel

## round temp to removes all the 1 abundances and standardise with sawa
saiki_adult_vel$Velocity<- round(saiki_adult_vel$Velocity,digits = 2)

## convert Saiki to abundance
saiki_adult_vel <- na.omit(saiki_adult_vel)
saiki_freq <- as.data.frame(table(saiki_adult_vel$Velocity))
sum(saiki_freq$Freq) # 337
colnames(saiki_freq)<- c("Velocity", "Abundance")
saiki_freq$Dataset <- "Saiki"
saiki_freq$Velocity <- as.numeric(as.character(saiki_freq$Velocity))
head(saiki_freq)
head(saiki_adult_vel)
## keep site in saiki
saiki_adult_vel$Dataset <- "Saiki"
hist(saiki_adult_vel$Velocity)

write_csv(saiki_adult_vel, "output_data/bio/01_saiki_site_adult_velocity.csv")
####### Thompson

head(thompx)
thompx <- thomp[-c(9,11,12),c(13,6)]
thompx
names(thompx) <- c("Velocity", "Abundance")
thompx$Dataset <- "Thompson"
all_wulff06
## combine
str(saiki_freq)
all_vel_con <- rbind(saiki_freq , all_wulff06 )
head(all_vel_con )
plot(all_vel_con$Velocity,all_vel_con$Abundance)
str(all_vel_con)
all_vel_con$Velocity <- as.numeric(as.character(all_vel_con$Velocity))

## wulff = velocity 0.6
## saiki = current
## Thompson = mid column velocity

write.csv(all_vel_con, "output_data/bio/01_adult_velocity_continuous_updated.csv")
hist(all_vel_con$Velocity)
## categorical

## bottom velocity - feet per sec
smea_03 <- read.csv("output_data/bio/00_SMEA_adult_velocity_2003_abundance.csv") 
smea_04 <- read.csv("output_data/bio/00_SMEA_adult_velocity_2004_abundance.csv")
juv_03 <- read.csv("output_data/bio/00_SMEA_juvenile_velocity_2003_abundance.csv")
juv_04 <- read.csv("output_data/bio/00_SMEA_juvenile_velocity_2004_abundance.csv")
smea_03

## get total abundance column and remove abundance row
smea_03 <- smea_03[-7,c(2,10)]
smea_04 <- smea_04[-7, c(2,6,7)]
smea_04$Abundance <- smea_04$july_Highway60_ab+smea_04$Aug_Highway60_Mission_ab
smea_04 <- smea_04[,c(1,4)]
## join toegtheer
smeavel <- cbind(smea_03, smea_04)
smeavel$Abundance1 <- smeavel$all_sites_ab + smeavel$Abundance
smea_ad_vel <- smeavel[,c(1,5)]
## get mid point
smea_ad_vel$mid_velocity <- c(0.25,0.75,1.25,1.75,2.25,2.75)
# convert from feet to meters
smea_ad_vel$Velocity <- smea_ad_vel$mid_velocity * 0.3048
smea_ad_vel

smea <- smea_ad_vel[,c(4,2)]
str(smea)
plot(smea$Velocity, smea$Abundance1)

## save

write.csv(smea, "output_data/bio/01_adult_velocity_categorical.csv)")

## juvenile
juv_04
smea_04
## get total abundance column and remove abundance row
juv_03 <- juv_03[-7,c(2,10)]
juv_03
juv_04 <- juv_04[-7, c(2,6,7)]
juv_04$Abundance <- juv_04$july_Highway60_ab+juv_04$Aug_Highway60_Mission_ab
juv_04 <- juv_04[,c(1,4)]
## join toegtheer
juvvel <- cbind(juv_03, juv_04)
juvvel$Abundance1 <- juvvel$all_sites_ab + juvvel$Abundance
smea_juv_vel <- juvvel[,c(1,5)]
## get mid point
smea_juv_vel$mid_velocity <- c(0.25,0.75,1.25,1.75,2.25,2.75)
# convert from feet to meters
smea_juv_vel$Velocity <- smea_juv_vel$mid_velocity * 0.3048
smea_juv_vel

juv <- smea_juv_vel[,c(4,2)]

str(juv)
plot(juv$Velocity, juv$Abundance1)

write.csv(juv, "output_data/bio/01_juvenile_velocity_categorical.csv")


#######
## subset juv only
unique(saiki$Life.Stage)
juv <- unique(saiki$Life.Stage)[4]
saiki_juv<- filter(saiki, Life.Stage %in% juv)
dim(saiki_juv) ## 10 - but 4 x NAs
saiki_juv
## count frequency in place of abundance (data is presence only)
saiki_juv <- saiki_juv[, c(15)]
saiki_juv <- as.data.frame(table(saiki_juv))

names(saiki_juv) <- c("Velocity", "Abundance")
saiki_juv$Velocity<- as.numeric(as.character(saiki_juv$Velocity))

saiki_juv$Dataset <- "Saiki"
head(saiki_juv)

write.csv(saiki_juv, "output_data/bio/01_juvenile_velocity_continous.csv")
## use for range?

## fry = NA in saiki

## spawning

head(saiki)
adults <-unique(saiki$Life.Stage)[1:3]

saiki_adult <- filter(saiki, Life.Stage %in% adults & Spawning..Y.N. == "Y") ## may keep all in
dim(saiki_adult) ## 687
names(saiki)

colnames(saiki_adult)[15] <- "Velocity"

## extract velcocity 
saiki_adult_vel <- select(saiki_adult, Velocity)
saiki_adult_vel

## round temp to removes all the 1 abundances and standardise with sawa
saiki_adult_vel$Velocity<- round(saiki_adult_vel$Velocity,digits = 2)

## convert Saiki to abundance

saiki_freq <- as.data.frame(table(saiki_adult_vel$Velocity))
sum(saiki_freq$Freq) # 11
colnames(saiki_freq)<- c("Velocity", "Abundance")
saiki_freq$Dataset <- "Saiki"
saiki_freq

write.csv(saiki, "output_data/bio/01_spawning_velocity_continous.csv")
