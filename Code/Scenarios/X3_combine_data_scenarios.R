### combine for overall 
library(tidyr)
library(tidyverse)
getwd()
## upload all species suitability data
## upload data

setwd("/Users/katieirving/Documents/git/flow_eco_mech/results/scenarios")

## time stats
time_statsx <- read.csv("X2_senarios_all_species_time_stats_updated_hyd.csv")
unique(time_statsx$Life_Stage)

time_statsx$Life_Stage[time_statsx$Life_Stage == "adult"] <- "Adult"

time_stats <- time_statsx  %>%
  select(Species, Life_Stage, Node,Hydraulic, water_year,TimePeriod, position,
         Scenario, Suitability_Class) %>%
  distinct()


SuitClassOverYears <- time_stats %>%
  group_by(Species, Life_Stage, Hydraulic, position, Node, Scenario) %>%
  summarise(Overall_Class = tail(names(sort(table(Suitability_Class))), 1))

SuitClassOverYears


write.csv(SuitClassOverYears, "X3_scenarios_all_suitability_combined_years.csv")


full_df <- SuitClassOverYears %>%
  ungroup() %>%
  mutate(pos_code = paste(Node, "_", position, sep="")) %>%
  distinct()

head(full_df)
write.csv(full_df, "X3_scenarios_Overall_Class_per_node_species_position.csv")

### if one hydraulic varibale per species and per position is low/partial, position is low

full_df <- full_df %>%
  mutate(sp_code = paste(Species, "_", Life_Stage, sep="")) %>%
  mutate(ClassOrder = Overall_Class) %>%
  mutate(ClassOrder = replace(ClassOrder, Overall_Class == "Low", 1)) %>%
  mutate(ClassOrder = replace(ClassOrder, Overall_Class == "Partial", 2)) %>%
  mutate(ClassOrder = replace(ClassOrder, Overall_Class == "High", 3))


## subset by scenario, then position/node, then species
## rule: if one hydraulic variable is low, then node position is low

scen <- unique(full_df$Scenario)
sp_dfx <- NULL

for(sc in 1:length(scen)) {
  
  ## filter to scenario
  
  scen_df <- full_df %>%
    filter(Scenario == scen[sc])
  
  ## define position/node
  pos <- unique(scen_df$pos_code)
  
  for(p in 1: length(pos)) {
    
    ## filters to node and postion
    pos_df <- scen_df %>%
      filter(pos_code == pos[p])
    
    ## define species $ lifstage at node/position
    species <- unique(pos_df$sp_code)
    
    for(s in 1:length(species)) {
      ## filter to 1 species lifestage
      sp_df <- pos_df %>%
        filter(sp_code == species[s])
      head(sp_df)
      if (length(unique(sp_df$Overall_Class)) >1) {
        ## if more than 1 suitbaility class then take the lowest one
        MinClass <- filter(sp_df, ClassOrder ==min(sp_df$ClassOrder))
        sp_df$Check_Class <- MinClass$Overall_Class[1]
        ## if only 1 class then take that class
      } else if (length(unique(sp_df$Overall_Class)) == 1){
        sp_df$Check_Class <-  paste(sp_df$Overall_Class[1])
      }
      
      sp_dfx <- rbind(sp_dfx, sp_df)
      
      
    }
  }
}



sp_dfx

sp_dfx ## data frame has overall class per node, position, species, life stage and hydraulic variable
names(sp_dfx)

write.csv(sp_dfx, "X3_scenarios_Overall_class_with_each_hydraulic_updated_hyd_prolonged.csv")

hp_dfx <- sp_dfx %>%
  select(Node, Species, Life_Stage, position, Scenario, sp_code, Check_Class) %>%
  distinct()

unique(hp_dfx$sp_code)

write.csv(hp_dfx, "X3_scenarios_Overall_class_per_node_all_species_updated_hyd_prolonged.csv")

### if one position is high then node is high
## subset by scenarios, species,  then node
hp_dfx <- hp_dfx %>%
  mutate(ClassOrder = Check_Class) %>%
  mutate(ClassOrder = replace(ClassOrder, Check_Class == "Low", 1)) %>%
  mutate(ClassOrder = replace(ClassOrder, Check_Class == "Partial", 2)) %>%
  mutate(ClassOrder = replace(ClassOrder, Check_Class == "High", 3))

node <- unique(hp_dfx$Node)

node


node_dfx <- NULL
s
p=5

scen <- unique(hp_dfx$Scenario)
sp_dfx <- NULL

for(sc in 1:length(scen)) {
  
  ## filter to scenario
  
  scen_df <- hp_dfx %>%
    filter(Scenario == scen[sc])
  
  ## define species
  species <- unique(scen_df$sp_code)

for(s in 1: length(species)) {
  
  sp_df <- scen_df %>%
    filter(sp_code == species[s])
  ## define nodes
  nd <- unique(sp_df$Node)
  
  for(p in 1: length(nd)) {
    
    node_df <- sp_df %>%
      filter(Node == nd[p])
    

    if(length(unique(node_df$Check_Class)) == 1) {
      node_df$Node_Class <- node_df$Check_Class
    } else if (length(unique(node_df$Check_Class)) > 1){
      ## if more than 1 suitbaility class then take the highest one
      MaxClass <- filter(node_df, ClassOrder ==max(node_df$ClassOrder))
      node_df$Node_Class <- MaxClass$Check_Class[1]
    }
    
    node_dfx <- rbind(node_dfx, node_df)
    
    }
  }
}


head(node_dfx)

df <- node_dfx %>%
  select(Node, Species, Life_Stage, sp_code, Scenario, Node_Class) %>%
  distinct()

df

write.csv(df, "X3_scenarios_All_Species_suit_class_Per_Node_updated_hyd_prolonged.csv")

### make wide


df_wide <- df %>%
  pivot_wider(id_cols = c(Scenario, Node), names_from = sp_code, values_from = Node_Class)

write.csv(df_wide, "X3_scenarios_All_species_suit_class_wide_updated_hyd_prolonged.csv")
df_wide
getwd()

### 
