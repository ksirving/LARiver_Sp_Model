## combine probability results

library(tidyverse)
library(tidyr)
library(sm)
library(lubridate) # work with dates
library(dplyr)     # data manipulation (filter, summarize, mutate)
library(ggplot2)   # graphics
library(gridExtra) # tile several plots next to each other
library(scales)
library(data.table)

# Work flow
## list and combine all nodes
## mean between hydraulics
## mean between life stages (growth)

setwd("/Users/katieirving/Documents/git/flow_eco_mech")
## upload all time stats csvs

## time stats
ts <- list.files("results/scenarios/probs", pattern="WRP_scenarios")
length(ts) ## 16
ts <- ts[-c(35:39)]
ts

prob_statsx <- NULL
j = 20

for(j in 1: length(ts)) {
  
  prob_stats <- read.csv(file=paste("results/scenarios/probs/", ts[j], sep=""))

  NodeName <- str_split(ts[j], "_", 3)[[1]]
  NodeName <- NodeName[2]
  # NodeName
  prob_stats <- prob_stats %>% 
    select(-X) %>% 
    group_by(SpeciesName, LifeStageName, Position, Scenario, season) %>%
    mutate(Scenario = as.character(Scenario)) %>%
    mutate(Node = NodeName) %>%
    distinct()

  
  prob_statsx <- bind_rows(prob_statsx, prob_stats)
  
}

prob_statsx <- prob_statsx %>%
  select(-Type) %>%
  distinct()

head(prob_statsx)
sum(is.na(prob_statsx))
dim(prob_statsx)

prob_statsx <- na.omit(prob_statsx)

test <- prob_statsx %>%
  filter(Node =="F34D", SpeciesName == "Steelhead", Scenario == 315)
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
write.csv(finalwithmin, "results/scenarios/probs/X2a_mean_probs_all_spp_best_slices_WRP_scenarios_updatedApril2021.csv")
# normalized = (x-min(x))/(max(x)-min(x))

finalwithmin <- read.csv("results/scenarios/probs/X2a_mean_probs_all_spp_best_slices_WRP_scenarios_updatedApril2021.csv")
scen_order <- read.csv("/Users/katieirving/Documents/git/flow_eco_mech/flow_recs/iterations_labeled.csv")
head(scen_order)

scen_order <- scen_order %>%
  select(-Burbank,-Glendale,-Tillman, -Med_Q_cfs) %>%
  mutate(Scenario = as.character(Scenario)) 

  
# tidal_scen <- finalx %>%
#   filter(Node %in% c("F37BHigh")) %>%
#   unite(Spp_code, SpeciesName:LifeStageName, sep="_")
# 
# 
# 
# ggplot(aes(x=Scenario, y=MeanProbability), data= tidal_scen)+
#   geom_point(aes(color = Spp_code)) +
#   facet_wrap(~season)
  
  unique(finalwithmin$Node)

## plants 
finalPlants <- finalwithmin %>%
  filter(Node == "LA3", SpeciesName %in% c( "Typha")) %>%
  unite(Spp_code, SpeciesName:LifeStageName, sep="_")

head(finalPlants)

finalPlants <- full_join(finalPlants, scen_order, by = "Scenario")

ggplot(aes(x=Scenario_order, y=MeanProbability), data= finalPlants)+
  geom_point(aes(color = Spp_code)) +
  facet_wrap(~season)

finalPlants <- finalwithmin %>%
  filter(Node == "LA3", SpeciesName %in% c( "Willow")) %>%
  unite(Spp_code, SpeciesName:LifeStageName, sep="_")

head(finalPlants)

finalPlants <- full_join(finalPlants, scen_order, by = "Scenario")

ggplot(aes(x=Scenario_order, y=MeanProbability), data= finalPlants)+
  geom_point(aes(color = Spp_code)) +
  facet_wrap(~season)

finalPlants <- finalwithmin %>%
  filter(Node == "LA3", SpeciesName %in% c( "Cladophora")) %>%
  unite(Spp_code, SpeciesName:LifeStageName, sep="_")

head(finalPlants)

finalPlants <- full_join(finalPlants, scen_order, by = "Scenario")

ggplot(aes(x=Scenario_order, y=MeanProbability), data= finalPlants)+
  geom_point(aes(color = Spp_code)) +
  facet_wrap(~season)

## fish
finalFish <- finalwithmin %>%
  filter(Node == "LA3", SpeciesName %in% c("SAS")) %>%
  unite(Spp_code, SpeciesName:LifeStageName, sep="_")

unique(finalFish$Scenario)
finalFish <- full_join(finalFish, scen_order, by = "Scenario")


ggplot(aes(x=Scenario_order, y=MeanProbability), data= finalFish)+
  geom_point(aes(color = Spp_code )) +
  facet_wrap(~season)

unique(finalFish$Spp_code)

finalFish <- finalwithmin %>%
  filter(Node == "LA8", SpeciesName %in% c("Steelhead")) %>%
  unite(Spp_code, SpeciesName:LifeStageName, sep="_") #%>%
  # filter(Spp_code == "Steelhead_Depth_Prolonged")


finalFish <- full_join(finalFish, scen_order, by = "Scenario")


ggplot(aes(x=Scenario_order, y=MeanProbability), data= finalFish)+
  geom_point(aes(color = Spp_code )) +
  facet_wrap(~season)

####




ggplot(finalPlants, aes(x = MeanProbability)) +
  stat_ecdf(aes(color = Spp_code,linetype = season), 
            geom = "step", size = 1.5) +
  # scale_color_manual(values = c("#00AFBB", "#E7B800"))+
  labs(y = "f(Probability)")


# Testing hydraulic data --------------------------------------------------


## get mean depth of concrete reaches for each season
# ## upload hydraulic data
setwd("/Users/katieirving/SCCWRP/LA River Eflows Study - General/Data/RawData/Results_Hydraulics/WRP_Scenarios/results-hydraulics_postprocessed")
dir <- "/Users/katieirving/SCCWRP/LA River Eflows Study - General/Data/RawData/Results_Hydraulics/WRP_Scenarios/results-hydraulics_postprocessed/"
h <- list.files(pattern="predictions")
length(h) ## 18
h

n=2

NodeData <- read.csv(file=paste(dir, h[n], sep=""))

names(NodeData)

# head(NodeData)
NodeName <- str_split(h[n], "_", 3)[[1]]
NodeName <- NodeName[1]
## format hydraulic data


cat(paste("Running Node", NodeName))

# hydraul <-NodeData[,-1]

## change some names
hydraul <- NodeData %>%
  rename(Q = Flow) %>%
  mutate(node = NodeName)

# names(hydraul)
## convert units and change names - depending on concrete/soft bottom. if/else to determine changes to data

if(length(NodeData) <= 9) {
  hyd_dep <- hydraul %>%
    mutate(depth_cm_MC = (Max..Depth..ft..MC*0.3048)*100) %>%
    mutate(Shear_pa_MC = (Shear..lb.sq.ft..MC/0.020885)) %>%
    mutate(StreamPower_w_MC = (Stream.Power..lb.ft.s..MC*4.44822)/0.3048) %>%
    mutate(velocity_m_MC = (Avg..Vel...ft.s..MC*0.3048)) %>%
    select(-contains("ft")) %>%
    mutate(date_num = seq(1,length(DateTime), 1))
} else {
  hyd_dep <- hydraul %>%
    mutate(depth_cm_LOB = (Max..Depth..ft..LOB*0.3048)*100,
           depth_cm_MC = (Max..Depth..ft..MC*0.3048)*100,
           depth_cm_ROB = (Max..Depth..ft..ROB*0.3048)*100) %>%
    mutate(Shear_pa_LOB = (Shear..lb.sq.ft..LOB/0.020885),
           Shear_pa_MC = (Shear..lb.sq.ft..MC/0.020885),
           Shear_pa_ROB = (Shear..lb.sq.ft..ROB/0.020885)) %>%
    mutate(StreamPower_w_LOB = (Stream.Power..lb.ft.s..LOB*4.44822)/0.3048,
           StreamPower_w_MC = (Stream.Power..lb.ft.s..MC*4.44822)/0.3048,
           StreamPower_w_ROB = (Stream.Power..lb.ft.s..LOB*4.44822)/0.3048) %>%
    mutate(velocity_m_LOB = (Avg..Vel...ft.s..LOB*0.3048),
           velocity_m_MC = (Avg..Vel...ft.s..MC*0.3048),
           velocity_m_ROB = (Avg..Vel...ft.s..ROB*0.3048)) %>%
    select(-contains("ft")) %>%
    mutate(date_num = seq(1,length(DateTime), 1))

}

all_data<-reshape2::melt(hyd_dep, id=c("Scenario", "DateTime","Q", "node", "date_num"))
## format date time
all_data$DateTime<-as.POSIXct(all_data$DateTime,
                              format = "%m/%d/%Y",
                              tz = "GMT")
# head(all_data)
## create year, month, day and hour columns and add water year

all_data <- all_data %>%
  mutate(month = month(DateTime)) %>%
  mutate(year = year(DateTime)) %>%
  mutate(day = day(DateTime)) %>%
  # mutate(hour = hour(DateTime)) %>%
  mutate(water_year = ifelse(month == 10 | month == 11 | month == 12, year, year-1))

###  define season
dry <- c(5,6,7,8,9)

all_data <- all_data %>%
  mutate(season = ifelse(month %in% dry, "Dry", "Wet"))
head(all_data)

## get stats
stats_depth <- all_data %>%
  group_by(Scenario, variable, season) %>%
  summarise(MeanHydraulic = mean(value)) %>%
  distinct() %>%
  mutate(Scenario = as.character(Scenario)) 

## plot scenarios

scen_order <- read.csv("/Users/katieirving/Documents/git/flow_eco_mech/flow_recs/iterations_labeled.csv")
head(scen_order)

scen_order <- scen_order %>%
  select(-Burbank,-Glendale,-Tillman, -Med_Q_cfs) %>%
  mutate(Scenario = as.character(Scenario)) 

stats_depth <- full_join(stats_depth, scen_order, by = "Scenario")


ggplot(aes(x=Scenario_order, y=MeanHydraulic), data= stats_depth)+
  geom_point(aes(color = variable)) +
  facet_wrap(~season)



prob_stats_6 <- all_data %>%
  group_by(Scenario) %>%
  filter(variable %in% c( "depth_cm_ROB"), Scenario %in% c(315,46,88,17,193,164), value < 50) %>%
  distinct()
unique(prob_stats_6$Scenario)
prob_stats_6$Scenario <- factor(prob_stats_6$Scenario, levels=c(315,46,88,17,193,164))

prob_stats_5 <- all_data %>%
  group_by(Scenario) %>%
  filter(variable %in% c( "depth_cm_ROB"), Scenario %in% c(1,100,200,300,400,500), Q<200) %>%
  distinct()

q1 <- ggplot(aes(x=Scenario, y=Q), data= prob_stats_depth)+
  geom_point(size=1, color = "blue") +
  scale_y_continuous() +
  facet_wrap(~water_year)

q1

t1 <- ggplot(prob_stats_5, aes(x =DateTime, y=Q)) +
  geom_line() +
  facet_wrap(~Scenario)

t1
head(prob_stats_depth)
lbels <- c(1,100,200,300,400,500)
unique(prob_stats_6$Scenario)

t2 <- ggplot(prob_stats_6, aes(x =DateTime, y= value)) +
  geom_line() +
  facet_grid(~Scenario)

t2

# p <- ggplot(prob_stats_depth, aes(x=Scenario)) +
  # stat_ecdf(aes(colour = water_year))

p1 <- ggplot(aes(x=Scenario, y=value), data= prob_stats_depth)+
  geom_point(size=1, color = "blue") +
  scale_y_continuous() +
  facet_wrap(~water_year)

p1

p2 <- ggplot(aes(x=Scenario, y=value), data= prob_stats_depth)+
  geom_point(aes(color=water_year))

p2




p3 <- ggplot(aes(x=Scenario, y=value), data= prob_stats_5)+
  geom_boxplot(aes(color=water_year))
p3
###### summary mets

prob_stats_depth <- all_data %>%
  filter(variable %in% c("depth_cm_LOB", "depth_cm_MC", "depth_cm_ROB" )) %>%
  group_by(water_year, variable, Scenario) %>%
  mutate(MeanHydraulicx = mean(value)) %>%
  ungroup() %>%
  group_by(Scenario, variable) %>%
  summarise(MeanHydraulic = mean(MeanHydraulicx), SDProbability = sd(MeanHydraulicx)) %>%
  distinct()

ggplot(aes(x=Scenario, y=MeanHydraulic), data= prob_stats_depth)+
  geom_point(aes(color = variable)) +
  scale_y_continuous(limits= c(0,35))


prob_stats_velocity <- all_data %>%
  filter(variable %in% c("velocity_m_LOB", "velocity_m_MC", "velocity_m_ROB" )) %>%
  group_by(water_year, variable, Scenario) %>%
  mutate(MeanHydraulicx = mean(value)) %>%
  ungroup() %>%
  group_by(Scenario, variable) %>%
  summarise(MeanHydraulic = mean(MeanHydraulicx), SDProbability = sd(MeanHydraulicx)) %>%
  distinct()

ggplot(aes(x=Scenario, y=MeanHydraulic), data= prob_stats_velocity)+
  geom_point(aes(color = variable)) #+
  scale_y_continuous(limits= c(0,0.4))

head(prob_stats_depth)
range(prob_stats_depth$MeanHydraulic)
head(final)


