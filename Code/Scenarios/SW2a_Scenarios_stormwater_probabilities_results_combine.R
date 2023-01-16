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
ts <- list.files("results/scenarios/probs", pattern="stormwater_scenarios")
length(ts) ## 16
ts
ts <- ts[-c(31:34)]

prob_statsx <- NULL


j
for(j in 1: length(ts)) {
  
  prob_stats <- read.csv(file=paste("results/scenarios/probs/", ts[j], sep=""))
  # head(prob_stats)
  
  NodeName <- str_split(ts[j], "_", 3)[[1]]
  NodeName <- NodeName[2]
  # NodeName
  prob_stats <- prob_stats %>% 
    select(-X) %>% 
    group_by(SpeciesName, LifeStageName, Position, Scenario, season) %>%
    mutate(Scenario = as.character(Scenario)) %>%
    mutate(Node = NodeName) %>%
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

rm(prob_statsx, finalsas, finalothers)

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

## min limits for each node position and year
mins <- finalx %>%
  ungroup() %>%
  filter(SpeciesName == "min")

rm(finalx)
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
final$LifeStageName <- gsub("smolts", "Smolts", final$LifeStageName)

final$HydraulicName <- gsub("depth", "Depth", final$HydraulicName)
final$HydraulicName <- gsub("velocity", "Velocity", final$HydraulicName)
final$HydraulicName <- gsub("shear", "Shear", final$HydraulicName)
## 
finalmins <- final %>%
  unite(CodeMin, c(SpeciesName, LifeStageName, HydraulicName, Node, Position), sep="_", remove=F)

names(minlimitstochange)
## filter to species needed to change
minlimitstochange <- finalmins %>%
  ungroup() %>%
  filter(CodeMin %in% minlimits$CodeMin) %>%
  unite(Codex, c(water_year, Node, Position, season, Scenario), sep="_")  %>%
  select(Codex, MeanProbability)

minlimitstochangecols <- finalmins %>%
  ungroup() %>%
  filter(CodeMin %in% minlimits$CodeMin) %>%
  unite(Codex, c(water_year, Node, Position, season, Scenario), sep="_", remove=F)  %>%
  rename(MeanProbabilityTest = MeanProbability, CodeTest = Codex) 

dim(minlimitstochangecols)
sum(is.na(minlimitstochange))
sum(is.na(minlimitsnottochange))

head(minlimitstochange)
dim(minlimitstochange)

print(object.size(minlimitstochange), units = "auto") ## 37.6 Mb, 21.5mb
print(object.size(mins), units = "auto") ## 21.5 Mb, 2.2mb
## filter to species that don't need changing
names(minlimitstochange)


minlimitsnottochange <- finalmins %>%
  filter(!CodeMin %in% minlimits$CodeMin) #%>%
# select(-c(Code, CodeMin, SDProbability, ScaledProbability))


sum(is.na(minlimitsnottochange))


rm(final)

## keep finalmins to join


## merge with the min limit values

head(mins)
dim(mins)
mins <- mins %>%
  unite(Codex, c(water_year, Node, Position, season, Scenario), sep="_") %>%
  select(Codex, MeanProbability) %>%
  rename(MinMean = MeanProbability) %>%
  filter(Codex %in% minlimitstochange$Codex)

## check each df is the same

length(unique(mins$Codex))
length(unique(minlimitstochange$Codex))

sum(minlimitstochange$Codex %in% mins$Codex)
sum(mins$Codex %in% minlimitstochange$Codex)

?full_join
minlimitstochangex <- left_join(minlimitstochange, mins, by="Codex")

head(minlimitstochangex)
sum(is.na(minlimitstochangex))
# rm(minlimitstochange) 
# # minlimitstochangex <- na.omit(minlimitstochangex)
# print(object.size(minlimitstochangex), units = "auto") ## 37.6 Mb, 21.5mb


minlimitstochangex <- minlimitstochangex %>%
  mutate(MeanProbabilityx = MeanProbability*MinMean) #%>%
# select( -MinMean)

# join columns back on

minlimitstochangex <- cbind(minlimitstochangex, minlimitstochangecols)
sort(names(minlimitsnottochange))
sort(names(minlimitstochangex))

str(minlimitstochangex)
str(minlimitstochangecols)
?separate

minlimitstochangex <- minlimitstochangex %>%
  separate(Codex, into=c("water_year", "Node", "Position", "season", "Scenario")) %>%
  select( - MeanProbability, -MinMean, -CodeTest, -MeanProbabilityTest) %>% 
  mutate(water_year = as.integer(water_year)) %>%
  rename(MeanProbability = MeanProbabilityx)


## join back together


finalwithmin <- bind_rows(minlimitstochangex, minlimitsnottochange)

head(finalwithmin)
write.csv(finalwithmin, "results/scenarios/probs/SW2a_mean_probs_all_spp_best_slices_stormwater_scenarios_updatedApril2021.csv")

head(final)
names(final)

unique(finalwithmin$Scenario)

# scen_order <- read.csv("/Users/katieirving/Documents/git/flow_eco_mech/flow_recs/iterations_labeled.csv")
# head(scen_order)
# scen_order <- scen_order %>%
#   select(-Burbank,-Glendale,-Tillman, -Med_Q_cfs)
# 
finalPlants <- finalx %>%
  filter(Node == "GLEN", SpeciesName %in% c("Willow", "Typha", "Cladophora")) %>%
  unite(Spp_code, SpeciesName:LifeStageName, sep="_")

finalFish <- finalx %>%
  filter(Node == "GLEN", SpeciesName %in% c("SAS", "Steelhead")) %>%
  unite(Spp_code, SpeciesName:LifeStageName, sep="_")
# 
# 
# head(finalPlants)
# unique(final$SpeciesName)

# finalPlants <- full_join(finalPlants, scen_order, by = "Scenario")

ggplot(aes(x=Scenario, y=MeanProbability), data= finalPlants)+
  geom_point(aes(color = Spp_code)) +
  facet_wrap(~season)

# finalFish <- full_join(finalFish, scen_order, by = "Scenario")


ggplot(aes(x=Scenario, y=MeanProbability), data= finalFish)+
  geom_point(aes(color = Spp_code )) +
  facet_wrap(~season)




