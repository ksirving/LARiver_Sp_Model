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
ts <- ts[-c(35:38)]
ts

prob_statsx <- NULL
j = 9

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
  
  
  names(prob_statsx)
  names(prob_stats)
  
  prob_statsx <- bind_rows(prob_statsx, prob_stats)
  
}

prob_statsx <- prob_statsx %>%
  select(-Type) %>%
  distinct()

head(prob_statsx)

unique(prob_statsx$SpeciesName)

## flag slice for each species and node

## slice data

slices <- read.csv("flow_recs/all_spp_slice_used.csv")

slices <- slices %>%
  rename(SpeciesName = Species, Position = BestSlice) %>%
  unite(Code, SpeciesName:Position, sep="_", remove=F)

Codes <- slices$Code
Codes
head(prob_statsx)

prob_statsx$SpeciesName <- gsub("willow", "Willow", prob_statsx$SpeciesName)

prob_statsx <- prob_statsx %>%
  unite(Code, c(SpeciesName, Node, Position), sep="_", remove = F)

## filter all probabilities to relevant stuff
final <- prob_statsx %>%
  filter(Code %in% Codes)
head(final)

# ### upload baseline max probs
# baseline <- read.csv("results/scenarios/probs/A2a_mean_probs_plant_spp_GLEN_best_slices_baseline_scenarios.csv")
# head(baseline)
# 
# ## format for join and summarise max prob over years
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
# write.csv(FinalTac, "results/scenarios/probs/X2a_mean_probs_plant_spp_GLEN_best_slices_WRP_scenarios.csv")
# # probs_slices <- full_join(prob_statsx, slices, by = c("SpeciesName", "Position", "Node"))
# 
# head(baselinejoin)
# head(baselinejoin$MeanProbability)
# head(baselinejoin$MeanProbability/baselinejoin$MaxProbability)
# 
# FinalTacScaled <- baselinejoin %>%
#   group_by(SpeciesName, LifeStageName, Position, Scenario, season) %>%
#   mutate(ScaledProbability = MeanProbability/MaxProbability)
# 
# write.csv(FinalTacScaled, "results/scenarios/probs/X2a_scaled_probs_plant_spp_GLEN_best_slices_WRP_scenarios.csv")

head(final)
names(final)

## scale SAS

finalsas <- final %>% 
  filter(SpeciesName == "SAS") %>%
  mutate(MeanProbability = (MeanProbability-min(MeanProbability))/(max(MeanProbability) 
                                                                   - min(MeanProbability))) %>%
  mutate(ScaledProbability = MeanProbability)

finalothers<- final %>% 
  filter(!SpeciesName == "SAS") %>%
  mutate(ScaledProbability = (MeanProbability-min(MeanProbability))/(max(MeanProbability) 
                                                                     - min(MeanProbability)))

finalx <- bind_rows(finalsas, finalothers)
# finalx <- finalx %>%
#   select(-Spp_code, - MaxProbability)

sum(is.na(finalx$ScaledProbability))
sum(is.na(finalx$MeanProbability))
nas <- which(is.na(finalx$ScaledProbability))
## NAs are adult willow, always 1 

finalx[nas, "ScaledProbability"] <- 1

unique(finalx$Node)

write.csv(finalx, "results/scenarios/probs/X2a_mean_probs_all_spp_best_slices_WRP_scenarios_updatedMarch2021.csv")
# normalized = (x-min(x))/(max(x)-min(x))

scen_order <- read.csv("/Users/katieirving/Documents/git/flow_eco_mech/flow_recs/iterations_labeled.csv")
head(scen_order)
scen_order <- scen_order %>%
  select(-Burbank,-Glendale,-Tillman, -Med_Q_cfs) %>%
  mutate(Scenario = as.character(Scenario)) 
unique(finalx$Node)
## plants 
finalPlants <- finalx %>%
  filter(Node == "GLEN", SpeciesName %in% c("Willow", "Typha", "Cladophora")) %>%
  unite(Spp_code, SpeciesName:LifeStageName, sep="_")

head(finalPlants)

finalPlants <- full_join(finalPlants, scen_order, by = "Scenario")

ggplot(aes(x=Scenario_order, y=MeanProbability), data= finalPlants)+
  geom_point(aes(color = Spp_code)) +
  facet_wrap(~season)

## fish
finalFish <- finalx %>%
  filter(Node == "LA202", SpeciesName %in% c("SAS")) %>%
  unite(Spp_code, SpeciesName:LifeStageName, sep="_")


finalFish <- full_join(finalFish, scen_order, by = "Scenario")


ggplot(aes(x=Scenario_order, y=MeanProbability), data= finalFish)+
  geom_point(aes(color = Spp_code )) +
  facet_wrap(~season)

finalFish <- finalx %>%
  filter(Node == "LA8", SpeciesName %in% c("Steelhead")) %>%
  unite(Spp_code, SpeciesName:LifeStageName, sep="_")


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



# ## upload hydraulic data
# setwd("/Users/katieirving/SCCWRP/LA River Eflows Study - General/Data/RawData/Results_Hydraulics/WRP_Scenarios/results-hydraulics_postprocessed")
# dir <- "/Users/katieirving/SCCWRP/LA River Eflows Study - General/Data/RawData/Results_Hydraulics/WRP_Scenarios/results-hydraulics_postprocessed/"
# h <- list.files(pattern="predictions")
# length(h) ## 18
# h
# 
# n=8
# 
# NodeData <- read.csv(file=paste(dir, h[n], sep=""))
# 
# names(NodeData)
# 
# # head(NodeData)
# NodeName <- str_split(h[n], "_", 3)[[1]]
# NodeName <- NodeName[1]
# ## format hydraulic data
# 
# 
# cat(paste("Running Node", NodeName))
# 
# # hydraul <-NodeData[,-1]
# 
# ## change some names
# hydraul <- NodeData %>%
#   rename(Q = Flow) %>%
#   mutate(node = NodeName)
# 
# # names(hydraul)
# ## convert units and change names - depending on concrete/soft bottom. if/else to determine changes to data
# 
# if(length(NodeData) <= 9) {
#   hyd_dep <- hydraul %>%
#     mutate(depth_cm_MC = (Max..Depth..ft..MC*0.3048)*100) %>%
#     mutate(Shear_pa_MC = (Shear..lb.sq.ft..MC/0.020885)) %>%
#     mutate(StreamPower_w_MC = (Stream.Power..lb.ft.s..MC*4.44822)/0.3048) %>%
#     mutate(velocity_m_MC = (Avg..Vel...ft.s..MC*0.3048)) %>%
#     select(-contains("ft")) %>%
#     mutate(date_num = seq(1,length(DateTime), 1))
# } else {
#   hyd_dep <- hydraul %>%
#     mutate(depth_cm_LOB = (Max..Depth..ft..LOB*0.3048)*100,
#            depth_cm_MC = (Max..Depth..ft..MC*0.3048)*100,
#            depth_cm_ROB = (Max..Depth..ft..ROB*0.3048)*100) %>%
#     mutate(Shear_pa_LOB = (Shear..lb.sq.ft..LOB/0.020885),
#            Shear_pa_MC = (Shear..lb.sq.ft..MC/0.020885),
#            Shear_pa_ROB = (Shear..lb.sq.ft..ROB/0.020885)) %>%
#     mutate(StreamPower_w_LOB = (Stream.Power..lb.ft.s..LOB*4.44822)/0.3048,
#            StreamPower_w_MC = (Stream.Power..lb.ft.s..MC*4.44822)/0.3048,
#            StreamPower_w_ROB = (Stream.Power..lb.ft.s..LOB*4.44822)/0.3048) %>%
#     mutate(velocity_m_LOB = (Avg..Vel...ft.s..LOB*0.3048),
#            velocity_m_MC = (Avg..Vel...ft.s..MC*0.3048),
#            velocity_m_ROB = (Avg..Vel...ft.s..ROB*0.3048)) %>%
#     select(-contains("ft")) %>%
#     mutate(date_num = seq(1,length(DateTime), 1))
#   
# }
# 
# all_data<-reshape2::melt(hyd_dep, id=c("Scenario", "DateTime","Q", "node", "date_num"))
# ## format date time
# all_data$DateTime<-as.POSIXct(all_data$DateTime,
#                               format = "%m/%d/%Y",
#                               tz = "GMT")
# # head(all_data)
# ## create year, month, day and hour columns and add water year
# 
# all_data <- all_data %>%
#   mutate(month = month(DateTime)) %>%
#   mutate(year = year(DateTime)) %>%
#   mutate(day = day(DateTime)) %>%
#   # mutate(hour = hour(DateTime)) %>%
#   mutate(water_year = ifelse(month == 10 | month == 11 | month == 12, year, year-1))
# 
# ### raw values
# 
# head(all_data)
# 
# prob_stats_depth <- all_data %>%
#   group_by(Scenario) %>%
#   filter(variable %in% c( "depth_cm_ROB")) %>%
#   distinct()
# 
# prob_stats_6 <- all_data %>%
#   group_by(Scenario) %>%
#   filter(variable %in% c( "depth_cm_ROB"), Scenario %in% c(315,46,88,17,193,164), value < 50) %>%
#   distinct()
# unique(prob_stats_6$Scenario)
# prob_stats_6$Scenario <- factor(prob_stats_6$Scenario, levels=c(315,46,88,17,193,164))
# 
# prob_stats_5 <- all_data %>%
#   group_by(Scenario) %>%
#   filter(variable %in% c( "depth_cm_ROB"), Scenario %in% c(1,100,200,300,400,500), Q<200) %>%
#   distinct()
# 
# q1 <- ggplot(aes(x=Scenario, y=Q), data= prob_stats_depth)+
#   geom_point(size=1, color = "blue") +
#   scale_y_continuous() +
#   facet_wrap(~water_year)
# 
# q1
# 
# t1 <- ggplot(prob_stats_5, aes(x =DateTime, y=Q)) +
#   geom_line() +
#   facet_wrap(~Scenario)
# 
# t1
# head(prob_stats_depth)
# lbels <- c(1,100,200,300,400,500)
# unique(prob_stats_6$Scenario)
# 
# t2 <- ggplot(prob_stats_6, aes(x =DateTime, y= value)) +
#   geom_line() +
#   facet_grid(~Scenario)
# 
# t2
# 
# # p <- ggplot(prob_stats_depth, aes(x=Scenario)) +
#   # stat_ecdf(aes(colour = water_year))
# 
# p1 <- ggplot(aes(x=Scenario, y=value), data= prob_stats_depth)+
#   geom_point(size=1, color = "blue") +
#   scale_y_continuous() +
#   facet_wrap(~water_year)
# 
# p1
# 
# p2 <- ggplot(aes(x=Scenario, y=value), data= prob_stats_depth)+
#   geom_point(aes(color=water_year)) 
# 
# p2
# 
# 
# 
# 
# p3 <- ggplot(aes(x=Scenario, y=value), data= prob_stats_5)+
#   geom_boxplot(aes(color=water_year))
# p3
# ###### summary mets
# 
# prob_stats_depth <- all_data %>%
#   filter(variable %in% c("depth_cm_LOB", "depth_cm_MC", "depth_cm_ROB" )) %>%
#   group_by(water_year, variable, Scenario) %>%
#   mutate(MeanHydraulicx = mean(value)) %>%
#   ungroup() %>%
#   group_by(Scenario, variable) %>%
#   summarise(MeanHydraulic = mean(MeanHydraulicx), SDProbability = sd(MeanHydraulicx)) %>%
#   distinct()
# 
# ggplot(aes(x=Scenario, y=MeanHydraulic), data= prob_stats_depth)+
#   geom_point(aes(color = variable)) +
#   scale_y_continuous(limits= c(0,35))
# 
# 
# prob_stats_velocity <- all_data %>%
#   filter(variable %in% c("velocity_m_LOB", "velocity_m_MC", "velocity_m_ROB" )) %>%
#   group_by(water_year, variable, Scenario) %>%
#   mutate(MeanHydraulicx = mean(value)) %>%
#   ungroup() %>%
#   group_by(Scenario, variable) %>%
#   summarise(MeanHydraulic = mean(MeanHydraulicx), SDProbability = sd(MeanHydraulicx)) %>%
#   distinct()
# 
# ggplot(aes(x=Scenario, y=MeanHydraulic), data= prob_stats_velocity)+
#   geom_point(aes(color = variable)) #+
#   scale_y_continuous(limits= c(0,0.4))
# 
# head(prob_stats_depth)
# range(prob_stats_depth$MeanHydraulic)
# head(final)
# 
# 
