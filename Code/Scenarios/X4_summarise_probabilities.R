## summarising proabilities 

library(tidyverse)
library(tidyr)

## work flow
## get mean probability of occurence for critical period per hydraulic
## mean propbability of occurence of hydraulics
## mean prob of life stage

## scenario probability data
getwd()

setwd("/Users/katieirving/Documents/git/flow_eco_mech")

data <- read.csv("results/scenarios/X2_senarios_all_species_time_stats_updated_hyd.csv")

head(data)
