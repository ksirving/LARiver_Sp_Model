#### probability on curve - take 2

library(tidyverse)
library(dplyr)

setwd("/Users/katieirving/Documents/git/flow_eco_mech")

## continuous
wulff <- read.csv("output_data/00_Wulff_depth_abundance.csv")
thomp <- read.csv("output_data/00_Thompson_all_data_clean.csv") # bottom velocity
saiki <- read.csv("input_data/abundance_env_vars_saiki_2000.csv") ## santa ana / san gabriel
envicraft <- read.csv("output_data/00_Envicraft_2010_Temp_abundance.csv")
sawa <- read.csv("output_data/00_SAWA_2014_env_hab_abundance.csv")

## temperature

sawa <- read.csv("output_data/00_SAWA_2014_env_hab_abundance.csv")
sawa <- sawa[, c(2,3,13, 15)]
ab_temp <- sawa[,c(3,4)]
ab_temp <- na.omit(ab_temp)
ab_temp$dataset <- "SAWA"
sum(ab_temp$abundance)
head(ab_temp)
# plot(ab_temp$Temp, ab_temp$abundance)

saiki <- read.csv("input_data/abundance_env_vars_saiki_2000.csv") ## santa ana / san gabriel
dim(saiki) # 715 
head(saiki)
adults <- droplevels(unique(saiki$Life.Stage)[1:3])
adults

## extract adults that aren't spawning (in spawning condition)

saiki_adult <- filter(saiki, Life.Stage %in% adults & Spawning..Y.N. == "N") ## may keep all in
dim(saiki_adult) ## 687

# clean data
saiki_adult$Site <- ifelse(saiki_adult$Site=="MWD8", paste("MWDB"), paste(saiki_adult$Site))
saiki_adult$Site
colnames(saiki_adult)[13] <- "Temp"

## extract temp 
saiki_adult_temp <- select(saiki_adult, Site, Date, Temp)
saiki_adult_temp

## round temp to removes all the 1 abundances
saiki_adult_temp$Temp <- round(saiki_adult_temp$Temp,digits = 1)

## convert Saiki to abundance

saiki_freq <- as.data.frame(table(saiki_adult_temp$Temp))
saiki_freq
colnames(saiki_freq)<- c("Temp", "abundance")
saiki_freq$dataset <- "Saiki"

## join together
names(ab_temp)
names(saiki_freq)

head(ab_temp)

all_df <- rbind(ab_temp, saiki_freq)
dim(all_df) # 123

plot(all_df$Temp, all_df$abundance) ## many 1's skewing the curve
str(all_df)
## format to numbers
all_df$Temp <- as.numeric(as.character(all_df$Temp))
## get probability

Meantemp <- mean(all_df$Temp)
Meantemp
length(all_df$Temp)
sum(all_df$abundance)
max(all_df$abundance)
hist(all_df$Temp)

### this needs a fix
expectedProportion <- dpois(0:123, lambda = Meantemp)
expectedFrequency <- expectedProportion * 963
expectedFrequency

hist(all_df$Temp, right = FALSE, breaks = seq(0, 110, 5), las = 1, col = "firebrick", xaxt = "n", main = "", xlab = "Temperature")

axis(1, at = seq(5, 30, 5), labels = seq(5,30,5))
lines(expectedFrequency, lwd = 2)

## depth
## make data frame to include all datasets so can upload easily
# temp abundance dataset - also try site

## depth data

## all data upload
## categorical
## smea
dep_ad_03 <- read.csv("output_data/00_SMEA_adult_depth_2003_abundance.csv")
dep_ad_04 <- read.csv("output_data/00_SMEA_adult_depth_2004_abundance.csv")
dep_ad_04
## format data into mid point
## make df with only abundance data
smea_ab <- cbind(dep_ad_04[,c(2,6,7)], dep_ad_03[,c(2,10)])
## add 2004 sites together
smea_ab$all_sites_2004 <-  smea_ab[,2] + smea_ab[,3]
smea_ab
# add years together
smea_ab$abundance <- smea_ab$all_sites_ab + smea_ab$all_sites_2004
# depth and abundance only, remove abundance row
ab_depth_sm <- smea_ab[-16, c(4,7)]
ab_depth_sm$Depth_mid <- c(2.5, 7.5, 12.5, 17.5, 22.5, 27.5, 32.5, 37.5, 42.5, 47.5, 52.5, 57.5, 62.5, 67.5, 71)
ab_depth_sm

## on abundance or presence/absence?

library(tidyverse)
library(dplyr)

setwd("/Users/katieirving/Documents/git/flow_eco_mech")


wulff <- read.csv("output_data/00_Wulff_depth_abundance.csv")
thomp <- read.csv("output_data/00_Thompson_all_data_clean.csv") # bottom velocity
saiki <- read.csv("input_data/abundance_env_vars_saiki_2000.csv") ## santa ana / san gabriel
envicraft <- read.csv("output_data/00_Envicraft_2010_Temp_abundance.csv")
sawa <- read.csv("output_data/00_SAWA_2014_env_hab_abundance.csv")

