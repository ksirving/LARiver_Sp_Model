### combine flow limits with Q calculation
### idea is to use calculation to define upper and lower limits
## add best slice info

getwd()

library(dplyr) 
library(tidyverse)
library(tidyr)

## empty df
LimitsCalcDatax <- NULL
r=1
## list limits data
## upload and format Q limits and calculation
L <- list.files(path = "ignore/Probs/", pattern="_limits_updated_hyd")
L

### list Q calculations

C <- list.files(path = "ignore/Probs/", pattern="_calculation_updated_hyd")
C
r=2
r
for (r in 1: length(L)) {
  
  LimitData <- read.csv(file=paste0("ignore/Probs/", L[r]))

  ## define limits name
  LimitName <- str_split(L[r], "_", 3)[[1]]
  LimitName <- LimitName[2]
  LimitName
  ## define species, life stage, hydraulic
  QName <- str_split(L[r], "_", 6)[[1]]
  SpeciesName <- QName[3]
  LifeStageName <- QName[4]
  HydraulicName <- QName[5]
  
  QName 

  ## extract matching file - limits file matches Q calulation file
  QC <- Filter(function(x) grepl(paste0(LimitName, "_", SpeciesName, "_", LifeStageName, "_", HydraulicName), x), C)
  QC
  
  ## upload calculation
  head(LimitData)
  dim(LimitData)
  CalcData <- read.csv(file=paste0("ignore/Probs/", QC))

  ## rename & format to match dfs
  ### add prob thresholds to limit data and make longer
  
  if (dim(LimitData)[1] == 4 && dim(LimitData)[2] == 7) {
    
    ## concrete channels with threshold models
    LimitData <- LimitData %>%
      select(-X) %>%
      rename(MC = V1) %>%
      mutate(ProbabilityThreshold = "Thresh") 
     
    LimitData <- LimitData %>%
      pivot_longer(MC, names_to = "Position", values_to = "Limits")
  
  } else if (dim(LimitData)[1] == 4 && dim(LimitData)[2] > 7) {
  
    ## soft bottom channels with threshold models
    LimitData <- LimitData %>%
      select(-X) %>%
      rename(LOB = V1, MC = V2, ROB = V3) %>%
      mutate(ProbabilityThreshold = "Thresh") 
    
    LimitData <- LimitData %>%
      pivot_longer(LOB:ROB, names_to = "Position", values_to = "Limits")
    
  } else if (dim(LimitData)[1] > 4 && dim(LimitData)[2] == 7) {
  
      ## concrete channelswith curve models
      LimitData <- LimitData %>%
        select(-X) %>%
        rename(MC = V1) %>%
        mutate(ProbabilityThreshold = NA) 
      
      LimitData$ProbabilityThreshold[c(1:4, 13:16)] <- paste("Low")
      LimitData$ProbabilityThreshold[c(5:8, 17:20)] <- paste("Medium")
      LimitData$ProbabilityThreshold[c(9:12, 21:24)] <- paste("High")
      
      LimitData <- LimitData %>%
        pivot_longer(MC, names_to = "Position", values_to = "Limits")
    
    } else if (dim(LimitData)[1] > 4 && dim(LimitData)[2] > 7) {
  
      ## soft bottom channels with curve models
      LimitData <- LimitData %>%
        select(-X) %>%
        rename(LOB = V1, MC = V2, ROB = V3) %>%
        mutate(ProbabilityThreshold = NA) 
      
      LimitData$ProbabilityThreshold[c(1:4, 13:16)] <- paste("Low")
      LimitData$ProbabilityThreshold[c(5:8, 17:20)] <- paste("Medium")
      LimitData$ProbabilityThreshold[c(9:12, 21:24)] <- paste("High")
      
      LimitData <- LimitData %>%
        pivot_longer(LOB:ROB, names_to = "Position", values_to = "Limits")
}

  LimitData <- na.omit(LimitData)
  LimitData
  
  ## calculation make wide and separate position column
  if (dim(CalcData)[2] == 7){
    
    ## threshold  models
    CalcData <- CalcData %>%
      select(-X) %>%
      pivot_longer(Thresh, names_to = "ProbabilityThreshold", values_to = "Q_Calculation") %>%
      separate(Position, into = c("Hydraulic", "Unit", "Position"), sep="_")
    
  } else {
    ## curve models
    CalcData <- CalcData %>%
      select(-X) %>%
      pivot_longer(Low:High, names_to = "ProbabilityThreshold", values_to = "Q_Calculation") %>%
      separate(Position, into = c("Hydraulic", "Unit", "Position"), sep="_")
  }


  ## combine limits and calculation

  
  LimitsCalcData <- left_join(LimitData, CalcData, by= c("ProbabilityThreshold", "Position"))
  
  
  LimitsCalcDatax <- rbind(LimitsCalcDatax, LimitsCalcData)
  
}

dim(LimitsCalcDatax)
# ?round

LimitsCalcDatax$Limits <- round(LimitsCalcDatax$Limits, digits = 2)
LimitsCalcDatax$Limits

LimitsCalcDatax <- na.omit(LimitsCalcDatax)

write.csv(LimitsCalcDatax, "synthesis/R1_all_limits_calc_combined.csv")


## add best slice info

SuitabilityPerSlice <- read.csv("results/S1_suitability_per_slice.csv")
LimitsCalcDatax <- read.csv("synthesis/R1_all_limits_calc_combined.csv")

SuitabilityPerSlice <- SuitabilityPerSlice %>%
  select(-X) %>%
  rename(ProbabilityThreshold = Type, Position = position)

LimitsCalcDatax <- LimitsCalcDatax %>% 
  select(-ends_with(".y"), -X) %>%
  rename(Species = Species.x, Life_Stage = Life_Stage.x, 
         Hydraulic = Hydraulic.x, Node = Node.x)
  
names(SuitabilityPerSlice)
names(LimitsCalcDatax)

all_data <- left_join(LimitsCalcDatax, SuitabilityPerSlice, 
                      by=c("Species", "Life_Stage", "Hydraulic", "Node",
                           "ProbabilityThreshold", "Position"))

head(all_data)

write_csv(all_data, "synthesis/R1_limits_calcs_best_slice.csv")

### add upper/lower thresholds

head(all_data)
# ?gsub
all_data$Q_Calculation <- gsub("newx1a", "limit",all_data$Q_Calculation)
all_data$Q_Calculation <- gsub("newx2a", "limit",all_data$Q_Calculation)
all_data$Q_Calculation <- gsub("newx3a", "limit",all_data$Q_Calculation)
all_data$Type <- gsub("Q_limit1", "Q_limit", all_data$Type)
all_data$Type <- gsub("Q_limit2", "Q_limit", all_data$Type)

calcs <- unique(all_data$Q_Calculation)
calcs

subset(all_data, Q_Calculation == "Q >= limit & Q <= limit" )

## do for all calcs
## remove hydraulic limit and add back in

##  "Q >= limit"
all_data1 <- all_data %>%
  filter(Q_Calculation == calcs[1], Type=="Q_limit") %>%
  group_by(Species, Life_Stage, Hydraulic, Node, ProbabilityThreshold, Position) %>%
  mutate(LimitBound = paste("Lower"), Flag = paste("N"))

## "Q >= min_limit & Q <= limit"
all_data2 <- all_data %>%
  filter(Q_Calculation == calcs[2], Type=="Q_limit") %>%
  group_by(Species, Life_Stage, Hydraulic, Node, ProbabilityThreshold, Position, Type) %>%
  mutate(LimitBound = paste( "Upper"), Flag = paste("With Min Limit"))

## "Q >= limit[1] & Q <= limit[2]"
all_data3 <- all_data %>%
  filter(Q_Calculation == calcs[3], Type=="Q_limit") %>%
  group_by(Species, Life_Stage, Hydraulic, Node, ProbabilityThreshold, Position, Type) %>%
  mutate(LimitBound = paste(c("lower", "Upper")), Flag = paste("N"))

all_data1

## "Q <= limit[1] & Q >= limit[2]"
all_data4 <- all_data %>%
  filter(Q_Calculation == calcs[4], Type=="Q_limit") %>%
  group_by(Species, Life_Stage, Hydraulic, Node, ProbabilityThreshold, Position, Type) %>%
  mutate(LimitBound = paste(c("upper", "lower")), Flag = paste("N"))

##  Q <= limit
### fix this one!!! Typha LA14, SAS, Willow 
all_data5 <- all_data %>%
  filter(Q_Calculation == calcs[5], Type=="Q_limit") %>%
  group_by(Species, Life_Stage, Hydraulic, Node, ProbabilityThreshold, Position, Type) %>%
  mutate(LimitBound = paste(c("upper")), Flag = paste("N"))

## "Q >= limit & Q <= limit" 

all_data6 <- all_data %>%
  filter(Q_Calculation == calcs[6], Type=="Q_limit") %>%
  group_by(Species, Life_Stage, Hydraulic, Node, ProbabilityThreshold, Position, Type) %>%
  mutate(LimitBound = paste(c( "upper")), Flag = paste("Y"))



### combine back together and with hydraulic limits

all_data_Q <- bind_rows(all_data1, all_data2, all_data3, all_data4, all_data5, all_data6)

hyd_type <- unique(all_data$Type)
hyd_type <- hyd_type[-1] ## remove Q_limit
hyd_type

all_data_hyd <- all_data %>%
  filter(Type %in% hyd_type) %>%
  mutate(Flag = paste("H"))

all_data_bounds <- bind_rows(all_data_Q, all_data_hyd)

write.csv(all_data_bounds, "synthesis/R1_limits_calcs_best_slice_bounds.csv")

