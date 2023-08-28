### combine all results and condense

library(tidyverse)
library(tidyr)

## upload data
setwd("/Users/katieirving/Library/CloudStorage/OneDrive-SCCWRP/Documents - Katie’s MacBook Pro/git/LARiver_Sp_Model/")

data <- read.csv("results/S1_all_suitability_combined_years.csv")
head(data)

full_df <- data %>%
  mutate(pos_code = paste(Node, "_", position, sep="")) %>%
  distinct()

write.csv(full_df, "results/S2_Overall_Class_per_node_species_position.csv")

### if one hydraulic varibale per species and per position is low/partial, position is low

full_df <- full_df %>%
  mutate(sp_code = paste(Species, "_", Life_Stage, sep="")) %>%
  mutate(ClassOrder = Overall_Class) %>%
  mutate(ClassOrder = replace(ClassOrder, Overall_Class == "Low", 1)) %>%
  mutate(ClassOrder = replace(ClassOrder, Overall_Class == "Partial", 2)) %>%
  mutate(ClassOrder = replace(ClassOrder, Overall_Class == "High", 3))


## subset by speices, then pos_code, then hydraulic
## rule: if one hydraulic variable is low, then node position is low
pos <- unique(full_df$pos_code)

sp_dfx <- NULL
s=1
p=1


for(p in 1: length(pos)) {
  
  ## filters to node and postion
  pos_df <- full_df %>%
    filter(pos_code == pos[p])
  pos_df
  ## define species $ lifstage at node/position
  species <- unique(pos_df$sp_code)
  
  for(s in 1:length(species)) {
    ## filter to 1 species lifestage
    sp_df <- pos_df %>%
      filter(sp_code == species[s])
    
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

sp_dfx

sp_dfx ## data frame has overall class per node, position, species, life stage and hydraulic variable
names(sp_dfx)
getwd()
write.csv(sp_dfx, "synthesis/Overall_class_with_each_hydraulic_updated_hyd_prolonged.csv")

hp_dfx <- sp_dfx %>%
  select(Node, Species, Life_Stage, position, sp_code, Check_Class) %>%
  distinct()

unique(hp_dfx$sp_code)

write.csv(hp_dfx, "synthesis/Overall_class_per_node_all_species_updated_hyd_prolonged.csv")

### if one position is high then node is high
## subset by species, then time period, then node
hp_dfx <- hp_dfx %>%
  mutate(ClassOrder = Check_Class) %>%
  mutate(ClassOrder = replace(ClassOrder, Check_Class == "Low", 1)) %>%
  mutate(ClassOrder = replace(ClassOrder, Check_Class == "Partial", 2)) %>%
  mutate(ClassOrder = replace(ClassOrder, Check_Class == "High", 3))

node <- unique(hp_dfx$Node)
species <- unique(hp_dfx$sp_code)
node


node_dfx <- NULL
s
p=5

for(s in 1: length(species)) {
  
  sp_df <- hp_dfx %>%
    filter(sp_code == species[s])

  
  nd <- unique(sp_df$Node)
  for(p in 1: length(nd)) {
    
    node_df <- sp_df %>%
      filter(Node == nd[p])
    
    node_df
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


head(node_dfx)

df <- node_dfx %>%
  select(Node, Species, Life_Stage, sp_code, Node_Class) %>%
  distinct()

df

write.csv(df, "synthesis/All_Species_suit_class_Per_Node_updated_hyd_prolonged.csv")

### make wide
df_wide

df_wide <- df %>%
  pivot_wider(id_cols = Node, names_from = sp_code, values_from = Node_Class)

write.csv(df_wide, "synthesis/All_species_suit_class_wide_updated_hyd_prolonged.csv")

getwd()

### 

# find most suitable slice ------------------------------------------------


# ## upload data
# 
# SuitabilityPerSlice <- read.csv("/Users/katieirving/Documents/git/SOC_tier_3/output_data/results/S1_suitability_per_slice.csv")
# 
# SuitabilityPerSlice <- SuitabilityPerSlice %>%
#   mutate(pos_code = paste(Node, "_", position, sep="")) %>%
#   distinct()
# 
# ## filter by only maximum %
# 
# ifelse(SuitabilityPerSlice$MaxPercentage == max(SuitabilityPerSlice$MaxPercentage), "Yes", "No")
# 
# BestSlicePerNode <- SuitabilityPerSlice %>%
#   group_by(Node, Species,Life_Stage, Hydraulic) %>%
#   mutate(BestSlice = ifelse(MaxPercentage == max(MaxPercentage), "Yes", "No")) %>%
#   filter(BestSlice == "Yes")
# 
# BestSlicePerNode
# 
# write.csv(BestSlicePerNode, "Best_slice_per_node_species.csv")

# Number of days ----------------------------------------------------------

## upload data

setwd("/Users/katieirving/Documents/git/flow_eco_mech/results")

## time stats
ts <- list.files(pattern="total_days_updated_hyd")
length(ts) ## 1
ts

ts <- ts[-c(1:4, 8:13)]
# ts <- ts[-c(10,12)]
## dataframes
total_daysx <- NULL
SuitClassOverYearsx <- NULL
# 
# ## probs needed
# probs <- c("Low", "Low.Seasonal", "High", "High.Seasonal")
# probs

# ## just for ones with prob threshold
# ts_con <- ts[c(1,3,4,7,8,13,14,15,18,19)]
# ts_cat <- ts[-c(1,3,4,7,8,13,14,15,18,19)]
# ts_con
# ts_cat
## create DF of nodes to merge
total_days <- read.csv(file=paste(ts[1], sep=""))
head(total_days)

suit_df <- total_days %>%
  mutate(pos_code = paste(Node, "_", position, sep="")) %>%
  select(pos_code) %>%
  distinct()

suit_df
j=1
j
for (j in 1:length(ts)) {
  
  total_days <- read.csv(file=paste(ts[j], sep=""))
  head(total_days)
  
  suit_df <- total_days %>%
    mutate(pos_code = paste(Node, "_", position, sep="")) %>%
    select(pos_code) %>%
    distinct()
  
  total_days <- total_days %>%
    select(Node, bottom_type, Species, Life_Stage, Hydraulic, water_year, TimePeriod, position,
           Suitability_Class) %>%
    mutate(pos_code = paste(Node, "_", position, sep="")) %>%
    distinct()
  
  
  SuitClassOverMonths <- total_days %>%
    group_by(Node, Species, Life_Stage, Hydraulic, TimePeriod, position, pos_code, water_year) %>%
    summarise(Overall_Class_Month = tail(names(sort(table(Suitability_Class))), 1))
  
  SuitClassOverYears <- SuitClassOverMonths %>%
    group_by(Node, Species, Life_Stage, Hydraulic, TimePeriod, position, pos_code) %>%
    summarise(Overall_Class_Year = tail(names(sort(table(Overall_Class_Month))), 1))
  
  
  SuitClassOverYearsx <- rbind(SuitClassOverYearsx, SuitClassOverYears)
  
}
j
SuitClassOverYearsx
## merge together
setwd("/Users/katieirving/Documents/git/flow_eco_mech")
full_df <- merge(suit_df, SuitClassOverYearsx, by="pos_code", all=T)
head(full_df)

write.csv(full_df, "synthesis/Overall_Class_per_node_species_position_total_days_updated_hyd_prolonged.csv")

### if one hydraulic varibale per species and per position is low/partial, position is low

full_df <- full_df %>%
  mutate(sp_code = paste(Species, "_", Life_Stage, sep="")) %>%
  mutate(ClassOrder = Overall_Class_Year) %>%
  mutate(ClassOrder = replace(ClassOrder, Overall_Class_Year == "Low", 1)) %>%
  mutate(ClassOrder = replace(ClassOrder, Overall_Class_Year == "Partial", 2)) %>%
  mutate(ClassOrder = replace(ClassOrder, Overall_Class_Year == "High", 3))


## subset by speices, then pos_code, then hydraulic
## rule: if one variable is low, then node is low
pos <- unique(full_df$pos_code)

sp_dfx <- NULL
sp_df


for(p in 1: length(pos)) {
  
  ## filters to node and postion
  pos_df <- full_df %>%
    filter(pos_code == pos[p])
  
  ## define species $ lifstage at node/position
  species <- unique(pos_df$sp_code)
  
  for(s in 1:length(species)) {
    ## filter to 1 species lifestage
    sp_df <- pos_df %>%
      filter(sp_code == species[s])
    
    
    if (length(unique(sp_df$Overall_Class_Year)) >1) {
      ## if more than 1 suitbaility class then take the lowest one
      MinClass <- filter(sp_df, ClassOrder ==min(sp_df$ClassOrder))
      sp_df$Check_Class <- MinClass$Overall_Class_Year[1]
      ## if only 1 class then take that class
    } else if (length(unique(sp_df$Overall_Class)) == 1){
      sp_df$Check_Class <-  paste(sp_df$Overall_Class_Year[1])
    }
    
    sp_dfx <- rbind(sp_dfx, sp_df)
    
    
  }
}
sp_dfx ## datsa frame has overall class per node, position, species, life stage and hydraulic variable
names(sp_dfx)
write.csv(sp_dfx, "synthesis/Overall_class_with_each_hydraulic_total_days_updated_hyd_prolonged.csv")

hp_dfx <- sp_dfx %>%
  select(Node, Species, Life_Stage, position, sp_code, Check_Class) %>%
  distinct()

unique(hp_dfx$sp_code)

write.csv(hp_dfx, "synthesis/Overall_class_per_node_all_species_total_days_updated_hyd_prolonged.csv")

### if one position is high then node is high
## subset by species, then time period, then node
hp_dfx <- hp_dfx %>%
  mutate(ClassOrder = Check_Class) %>%
  mutate(ClassOrder = replace(ClassOrder, Check_Class == "Low", 1)) %>%
  mutate(ClassOrder = replace(ClassOrder, Check_Class == "Partial", 2)) %>%
  mutate(ClassOrder = replace(ClassOrder, Check_Class == "High", 3))

node <- unique(hp_dfx$Node)
species <- unique(hp_dfx$sp_code)
node


node_dfx <- NULL
s
p

for(s in 1: length(species)) {
  
  sp_df <- hp_dfx %>%
    filter(sp_code == species[s])
  
  
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


head(node_dfx)

df <- node_dfx %>%
  select(Node, Species, Life_Stage, sp_code, Node_Class) %>%
  distinct()

df

write.csv(df, "synthesis/All_Species_suit_class_Per_Node_total_days_updated_hyd_prolonged.csv")

### make wide
df_wide

df_wide <- df %>%
  pivot_wider(id_cols = Node, names_from = sp_code, values_from = Node_Class)

write.csv(df_wide, "synthesis/All_species_suit_class_wide_total_days_updated_hyd_prolonged.csv")


