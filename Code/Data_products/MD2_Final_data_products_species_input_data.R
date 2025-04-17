## species curves input data format

library(tidyr)
library(tidyverse)


# SAS ---------------------------------------------------------------------


## adult depth
ad_depth_con <- read.csv("output_data/05a_adult_depth_continuous_updated.csv") ## all wulff incl and thompson removed - remove SAWA?
ad_depth_cat <- read.csv("output_data/05a_adult_depth_categorical.csv")

# ad_depth_red <- subset(ad_depth_con, !Dataset=="Thompson")
all_depth <- rbind(ad_depth_con, ad_depth_cat)

unique(all_depth$Dataset) # 4 datasets, 

depth_freq <- all_depth %>% 
  uncount(Abundance)
# hist(depth_freq$Depth)

depth_freq <- subset(depth_freq, !Dataset=="SAWA")
dim(depth_freq) ## 1376


### juvenile depth

juv_depth_con <- read.csv("output_data/05a_juvenile_depth_continuous.csv")
juv_depth_cat <- read.csv("output_data/05a_juvenile_depth_categorical.csv")


ad_vel_con <- read.csv("output_data/05a_adult_velocity_continuous.csv")
juv_vel_con <- read.csv("output_data/05a_juvenile_velocity_continuous.csv")

ad_vel_con <- subset(ad_vel_con, Dataset !="Thompson")


# Cladophora --------------------------------------------------------------


depth <- read.csv("input_data/Depth_2_Higgins_etal_2005.csv")

pres_velo25 <- rnorm(4000, 0.68, 0.19 )
abs_velo25 <- rnorm(4000, 0.41, 0.2 )

t.test(pres_velo25, abs_velo25)

dfp <- as.data.frame(pres_velo25) %>%
  rename(Velocity = pres_velo25) %>%
  mutate(Occurrence = 1, Species = "Cladophora glomerata")

dfa <- as.data.frame(abs_velo25) %>%
  rename(Velocity = abs_velo25) %>%
  mutate(Occurrence = 0, Species = "Cladophora glomerata")

df <- rbind(dfp, dfa)
head(df)
mean(df$Velocity)
dim(df)


# Steelhead ---------------------------------------------------------------

## thresholds only


# Willow ------------------------------------------------------------------

# see LARiver_Sp_Model/Code/ModelingBuild/M1_willow_seedling_inundation.R
# LARiver_Sp_Model/Code/ModelingBuild/M1_willow_seedling_shear_stress.R
# LARiver_Sp_Model/Code/ModelingBuild/M1_willow_germination_inundation.R

# Typha -------------------------------------------------------------------

## see LARiver_Sp_Model/Code/ModelingBuild/typha_models_and_figures.R
