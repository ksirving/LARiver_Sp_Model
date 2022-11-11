## combine probability results

library(tidyverse)
library(tidyr)

# Work flow
## list and combine all nodes
## mean between hydraulics
## mean between life stages (growth)

setwd("/Users/katieirving/Documents/git/flow_eco_mech")
## upload all time stats csvs

## time stats
ts <- list.files("results/scenarios/probs", pattern="baseline")
length(ts) ## 32
# ts <- ts[-33]
ts <- ts[-c(37:41)]
ts
j
prob_statsx <- NULL
nasx <- NULL

for(j in 1: length(ts)) {
  
  prob_stats <- read.csv(file=paste("results/scenarios/probs/", ts[j], sep=""))
  head(prob_stats)
  
  nas <- sum(is.na(prob_stats))

  
  nasx <- rbind(nasx, nas)
  
}

nasx

for(j in 1: length(ts)) {
  
  prob_stats <- read.csv(file=paste("results/scenarios/probs/", ts[j], sep=""))
  head(prob_stats)
  
  NodeName <- str_split(ts[j], "_", 3)[[1]]
  NodeName <- NodeName[2]
  # NodeName
  prob_stats <- prob_stats %>% 
    select(-X) %>% 
    group_by(SpeciesName, LifeStageName, Position, Scenario, season) %>%
    # mutate(MeanProb = mean(MeanProbability),
    #        SDProb = mean(SDProbability)) %>%
    # select(-water_year, -MeanProbability, -SDProbability) %>%
    mutate(Node = NodeName, Number = j) %>%
    distinct()
  
  
  
  prob_statsx <- rbind(prob_statsx, prob_stats)
  
}

prob_statsx <- prob_statsx %>%
  select(-Type) %>%
  distinct()

head(prob_statsx)
sum(is.na(prob_statsx))
dim(prob_statsx)

prob_statsx <- na.omit(prob_statsx)

## scale SAS

finalsas <- prob_statsx %>% 
  filter(SpeciesName == "SAS") %>%
  mutate(MeanProbability = (MeanProbability-min(MeanProbability))/(max(MeanProbability) 
                                                                   - min(MeanProbability))) %>%
  mutate(ScaledProbability = MeanProbability)

finalothers<- prob_statsx %>% 
  filter(!SpeciesName == "SAS") %>%
  mutate(ScaledProbability = (MeanProbability-min(MeanProbability))/(max(MeanProbability) 
                                                                     - min(MeanProbability)))

finalx <- bind_rows(finalsas, finalothers)

sum(is.na(finalx))
sum(is.na(finalx$ScaledProbability))
sum(is.na(finalx$MeanProbability))
nas <- which(is.na(finalx$ScaledProbability))
## NAs are adult willow, always 1 
finalx[nas,]
finalx[nas, "ScaledProbability"] <- 1

unique(finalx$Node)
unique(finalx$SpeciesName)

head(finalx)
names(finalx)

## flag slice for each species and node

## slice data

slices <- read.csv("flow_recs/all_spp_slice_used.csv")

slices <- slices %>%
  rename(SpeciesName = Species, Position = BestSlice) %>%
  unite(Code, SpeciesName:Position, sep="_", remove=F)

Codes <- slices$Code
Codes
head(finalx)

finalx$SpeciesName <- gsub("willow", "Willow", finalx$SpeciesName)

finalx <- finalx %>%
  unite(Code, c(SpeciesName, Node, Position), sep="_", remove = F)

## species without min 
final <- finalx %>%
  filter(Code %in% Codes)
head(final)
sum(is.na(final))
## define species/nodes min limit needed

minlimits <- read.csv("/Users/katieirving/Documents/git/flow_eco_mech/flow_recs/R1_limits_calcs_best_slice_bounds.csv")
head(minlimits)
sum(is.na(minlimits))

unique(minlimits$Species)
unique(minlimits$Life_Stage)
unique(minlimits$Hydraulic)

minlimits$Life_Stage <- gsub("Migration", "Burst", minlimits$Life_Stage)
minlimits$Life_Stage <- gsub("Migration_Prolonged", "Prolonged", minlimits$Life_Stage)
## species to calculate prob with  min limit
minlimits <- minlimits %>%
  filter(Flag == "With Min Limit") %>%
  unite(CodeMin, c(Species, Life_Stage, Hydraulic, Node, Position), sep="_", remove=F) %>%
  select(CodeMin, Species, Life_Stage, Hydraulic, Node, Position) %>%
  distinct()


# 
unique(finalmins$LifeStageName)
unique(finalmins$HydraulicName)
unique(finalmins$SpeciesName)

final$LifeStageName <- gsub("adult", "Adult", final$LifeStageName)
final$LifeStageName <- gsub("juvenile", "Juvenile", final$LifeStageName)
final$LifeStageName <- gsub("seedling", "Seedling", final$LifeStageName)
final$LifeStageName <- gsub("smolts", "Burst", final$LifeStageName)

final$HydraulicName <- gsub("depth", "Depth", final$HydraulicName)
final$HydraulicName <- gsub("velocity", "Velocity", final$HydraulicName)
final$HydraulicName <- gsub("shear", "Shear", final$HydraulicName)
## 
finalmins <- final %>%
  unite(CodeMin, c(SpeciesName, LifeStageName, HydraulicName, Node, Position), sep="_", remove=F)

m <- unique(minlimits$CodeMin)
f <- unique(finalmins$CodeMin)

sum(finalmins$CodeMin %in% minlimits$CodeMin)
sum(f %in% m) 
f
m
Steelhead_Burst_Velocity_11101250_LOB
## filter to species needed to change
minlimitstochange <- finalmins %>%
  filter(CodeMin %in% minlimits$CodeMin) %>%
  unite(Codex, c(water_year, Node, Position, season), sep="_", remove=F) 
  
sum(is.na(minlimitstochange))
sum(is.na(minlimitsnottochange))

## filter to species that don't need changing
minlimitsnottochange <- finalmins %>%
  filter(!CodeMin %in% minlimits$CodeMin)
sum(is.na(minlimitsnottochange))
## min limits for each node position and year
mins <- finalx %>%
  ungroup() %>%
  filter(SpeciesName == "min")

## merge with the min limit values
head(mins)
mins <- mins %>%
  unite(Codex, c(water_year, Node, Position, season), sep="_", remove=F) %>%
  select(Codex, MeanProbability) %>%
  rename(MinMean = MeanProbability)

minlimitstochangex <- full_join(minlimitstochange, mins, by="Codex")

minlimitstochangex <- na.omit(minlimitstochangex)

minlimitstochangex <- minlimitstochangex %>%
  mutate(MeanProbability = MeanProbability*MinMean) %>%
  select(-Codex, -MinMean)

## join back together


finalwithmin <- bind_rows(minlimitstochangex, minlimitsnottochange)


### upload baseline max probs
baseline <- read.csv("results/scenarios/probs/A2a_mean_probs_plant_spp_GLEN_best_slices_baseline_scenarios.csv")
head(baseline)

## format for join and summarise max prob over years
# baseline <- baseline %>%
#   # group_by(Spp_code, LifeStageName, season) %>% 
#   unite(Code, c(SpeciesName,Node, Position), sep="_", remove=F) %>%
#   filter(Code %in% Codes) %>%
#   select(water_year, Spp_code, SpeciesName, Code, LifeStageName, season, MaxProbability, Node) %>%
#   group_by(Spp_code, SpeciesName, Code, LifeStageName, season, Node) %>%
#   summarise(MaxProbability = max(MaxProbability)) %>%
#   distinct()
# 
# 
# dim(baseline)
# 
# ## just for TAC
# FinalTac <- final %>%
#   filter(Node == "GLEN", SpeciesName %in% c("Willow", "Typha", "Cladophora")) %>%
#   unite(Spp_code, c(SpeciesName, HydraulicName), sep="_", remove=F) %>%
#   filter(!Spp_code %in% c("Typha_velocity", "Cladophora_depth", "Willow_depth"))
# 
# unique(FinalTac$Spp_code)
# 
# baselinejoin <- left_join(FinalTac, baseline, by=c( "Spp_code","Code", "season", "LifeStageName","SpeciesName", "Node"))
# dim(baselinejoin)
# 
# head(baselinejoin)
# head(baselinejoin$MeanProbability)
# head(baselinejoin$MeanProbability/baselinejoin$MaxProbability)
# 
# FinalTacScaled <- baselinejoin %>%
#   group_by(SpeciesName, LifeStageName, Position, Scenario, season) %>%
#   mutate(ScaledProbability = MeanProbability/MaxProbability.y) %>%
#   select(-MaxProbability.x) %>% 
#   rename(MaxProbability = MaxProbability.y)
# 
# names(FinalTacScaled)


# write.csv(FinalTacScaled, "results/scenarios/probs/A2a_scaled_probs_plant_spp_GLEN_best_slices_baselines.csv")

write.csv(finalwithmin, "results/scenarios/probs/A2a_mean_probs_all_spp_best_slices_baseline_updatedApril2021.csv")


head(finalwithmin)
names(finalwithmin)

scen_order <- read.csv("/Users/katieirving/Documents/git/flow_eco_mech/flow_recs/iterations_labeled.csv")
head(scen_order)
scen_order <- scen_order %>%
  select(-Burbank,-Glendale,-Tillman, -Med_Q_cfs)

finalPlants <- finalwithmin %>%
  filter(Node == "GLEN", SpeciesName %in% c("Willow", "Typha", "Cladophora")) %>%
  unite(Spp_code, SpeciesName:LifeStageName, sep="_")

finalFish <- finalwithmin %>%
  filter(Node == "GLEN", SpeciesName %in% c("SAS", "Steelhead")) %>%
  unite(Spp_code, SpeciesName:LifeStageName, sep="_")


head(finalPlants)
unique(final$SpeciesName)

finalPlants <- full_join(finalPlants, scen_order, by = "Scenario")

ggplot(aes(x=Scenario_order, y=MeanProbability), data= finalPlants)+
  geom_point(aes(color = Spp_code)) +
  facet_wrap(~season)

finalFish <- full_join(finalFish, scen_order, by = "Scenario")


ggplot(aes(x=Scenario_order, y=MeanProbability), data= finalFish)+
  geom_point(aes(color = Spp_code )) +
  facet_wrap(~season)








