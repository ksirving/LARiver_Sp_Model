## algorithm for substrate

#  adult
#  substrate

library(tidyverse)

setwd("/Users/katieirving/Documents/git/flow_eco_mech")

# upload habitat

smea <- read.csv("output_data/00_smea_total_percentage.csv")
smea

# gravel == 1
# sand_gravel == 2
# gravel_cobble == 3
# sand == 4 
# boulder == 5
# cobble == 6
# silt == 7

# the last 2 are bad, cobble not as bad because with gravel is ok. any presence of silt is bad
# no info on concrete, but assume that 90% concrete is bad
# sand cobble & silt cobble mix is also bad


# use data from liesl

#upload and clean substrate data
lar_data <- read.csv("input_data/LAR_sites_PHab_metrics_env.csv")
head(lar_data)
unique(lar_data$grouptype)

# subset into only 
lar_substrate <- subset(lar_data, grouptype == "Substrate Size and Composition" )
dim(lar_substrate) #3200

head(lar_substrate)
unique(lar_substrate$variablename)
unique(lar_substrate$sampledate) # 2009 - 2019
unique(lar_substrate$LAR_sites) # 52 sites

# adults

# assign scoring system to the substrate present - 

# if the substrate is 90% concrete = 0
# if the substrate is gravel (find and coarse)  less than 20% = 0, 20% = 7, 40% = 8, 60%+ = 9
# if the substrate is sand 20% = 20% = 3, 40% = 4, 60%+ = 5
# boulder 20% = 20% = 1, 40% = 2, 60%+ = 3
# silt = 0 #Percent Substrate Smaller than Sand (<2 mm)
# concrete = 0

# if the substrate is 

# gravel == 1 - 5
# sand_gravel == 2 - 5
# gravel_cobble == 3 - 3
# sand == 4 - 3
# boulder == 5 -1 
# cobble == 6 - 1
# silt == 7 - 0 
# concrete == 0

# the last 2 are bad, cobble not as bad because with gravel is ok. any presence of silt is bad
# no info on concrete, but assume that 90% concrete is bad
# sand cobble & silt cobble mix is also bad

# if the substrate is 90% concrete = 0
# if the substrate is gravel (fine and coarse)  less than 20% = 1, 20% = 7, 40% = 8, 60%+ = 9
# if the substrate is sand 20% = 20% = 3, 40% = 4, 60%+ = 5
# boulder 20% = 20% = 1, 40% = 2, 60%+ = 3
# cobble - 20% = 20% = 1, 40% = 2, 60%+ = 3
# silt = 0 #Percent Substrate Smaller than Sand (<2 mm)
# concrete = 0
# mixed - all except concrete - >60 = 15, 40-60 = 10, 20-40 = 5, below 5 = 0

# [3] Percent Substrate as Bedrock                       
# [4] Percent Substrate Larger than Fine Gravel  (>16 mm)
# [5] Percent Cobble                                     
# [6] CPOM Presence                                      
# [7] Percent Fines (silts/clay/muck)                    
# [8] Percent Gravel - coarse                            
# [9] Percent Gravel - fine                              
# [10] Percent Hardpan                                    
# [11] Percent Other Substrate                            
# [12] Percent Concrete/Asphalt                           
# [13] Percent Bedrock - rough                            
# [14] Percent Bedrock - smooth                           
# [15] Percent Sand                                       
# [16] Percent Substrate Smaller than Sand (<2 mm)        
# [17] Percent Boulders - small                           
# [18] Percent Substrate Fine Gravel or Smaller (<16 mm)  
# [19] Percent Wood                                       
# [20] Percent Boulders - large 

# clean data for loop
lar_substrate$result <- as.numeric(as.character(lar_substrate$result))
lar_substrate <- separate(lar_substrate, col=sampledate, into =c("date", "time", "time2"), sep=c(" "), remove=F)
lar_substrate$date <- gsub("/", "_", lar_substrate$date)

# fine sediment - 15,16,18,7
# coarse sediment - 4, 8,9,
# extra coarse sediment - 3,5,10,13,14,17,20

# list sites
sites <- unique(lar_substrate$LAR_sites)

#may add "other substrate"
#  get variables of interest
vars <- droplevels(unique(lar_substrate$variablename) [c(3,5,7,8,9,10,12,13,14,15,17,20)]) # 

# separate into categories
fines <-vars[c(3,10)]
coars <-vars[c(4,5)] 
ex_coars <-vars[c(1,2,6,8,9,11,12)] 
conc <- vars[7]


df_cats <- as.data.frame(matrix(ncol=11))
colnames(df_cats) <- c("site", "date", "sitename", "lat", "lon", "fine_sediment","coarse_sediment", "extra_coarse", "concrete", "total", "mixed")

# loop to get main category of substrate on overall percentage of category

for(s in 1: length(sites)){
  
  # subset site
  data <- subset(lar_substrate, LAR_sites == sites[s])
  
  # list sites
  sep_date <- unique(data$sampledate)
  
  
  for(y in 1:length(sep_date)) {
    
    
    #then subset date from each site
    dfx <- subset(data, sampledate == sep_date[y])
    
    vx <- dfx$variablename %in% vars
    
    # create dataframe from varaibles and percentages
    data_dat <- as.data.frame(matrix(ncol=2, nrow=12))
    colnames(data_dat) <- c("variablename", "result")
    
    data_dat[,1] <- vars
    data_dat[,2] <- dfx[vx, "result"][1:12]
    
    
    fx <-  data_dat$variablename %in% fines
    cx <-  data_dat$variablename %in% coars
    ecx <-  data_dat$variablename %in% ex_coars
    co <-  data_dat$variablename %in% conc
    
    df <- as.data.frame(matrix(ncol=11))
    colnames(df) <- c("site", "date", "sitename", "lat", "lon", "fine_sediment","coarse_sediment", "extra_coarse", "concrete", "total", "mixed")
    
    df$site <- paste(dfx$LAR_sites[1])
    df$date <- paste(dfx$sampledate[1])
    df$sitename <- paste(dfx$sitename[1])
    df$lat <- paste(dfx$targetlatitude[1])
    df$lon <- paste(dfx$targetlongitude[1])
    df$fine_sediment <- sum(data_dat[fx,"result"])
    df$coarse_sediment <- sum(data_dat[cx,"result"])
    df$extra_coarse <- sum(data_dat[ecx,"result"])
    df$concrete <- sum(data_dat[co,"result"])
    df$total <- sum(df[,6:9])
    df$mixed <- sum(df[,6:8])
    
    df_cats <- rbind(df_cats, df)
    
  }
  
}

## data frame with percentage cover per category
write.csv(df_cats, "output_data/substrate_scores/02_substrate_categories.csv")
df_cats

# add score for sites  per life stage
df_cats <- read.csv("output_data/substrate_scores/02_substrate_categories.csv")
rownames(df_cats)

v=10

df_cats$fine_sediment[v]
df_cats$coarse_sediment[v]
df_cats$extra_coarse[v]
df_cats$concrete[v]
df_cats$mixed[v]

df_cats$fine_sediment[v] <=1 || df_cats$fine_sediment[v] >=80 #&& if true 
df_cats$coarse_sediment[v]  <=1 #&&
df_cats$extra_coarse[v] <= 10 || df_cats$extra_coarse[v] >=80 # &&
df_cats$concrete[v]  > 50 || df_cats$mixed[v] <=13

df_cats$fine_sediment[v] >=1 &&  df_cats$fine_sediment[v] >=20 
df_cats$coarse_sediment[v]  >=1  &&  df_cats$coarse_sediment[v]  >=20
df_cats$extra_coarse[v] >= 10  && df_cats$extra_coarse[v] <= 20
df_cats$concrete[v]  <= 50 && df_cats$concrete[v]  >= 25 || df_cats$mixed[v] <=30 && df_cats$mixed[v] >= 14

df_cats$fine_sediment[v] >=21 &&  df_cats$fine_sediment[v] <=80
df_cats$coarse_sediment[v]  >=21 ##  &&  df_cats$coarse_sediment[v]  >=20
df_cats$extra_coarse[v] >= 21  && df_cats$extra_coarse[v] <= 80
df_cats$concrete[v]  <= 25 || df_cats$mixed[v]  > 31

# only concentrate on concrete and  mixed sediment

for(v in 2: length(rows)) {
  
  if(df_cats$fine_sediment[v] <=5 || df_cats$fine_sediment[v] >=80)  { # less than or equal to 5, more than 80
    if(df_cats$coarse_sediment[v]  <=5) { # less than or equal to 5
      if(df_cats$extra_coarse[v] <= 10 || df_cats$extra_coarse[v] >=40) { # less than or equal to 10, more than or equal to 40
        if(df_cats$concrete[v]  > 50 || df_cats$mixed[v] <=13) { # concrete more than 50, mixed substrate less than 13
          
          df_cats$substrate_score[v] <- 0
          
        } else { if (df_cats$fine_sediment[v] >5 &&  df_cats$fine_sediment[v] >=20) {
          if(df_cats$coarse_sediment[v]  >5  &&  df_cats$coarse_sediment[v]  >=20) {
            if(df_cats$extra_coarse[v] >= 10  && df_cats$extra_coarse[v] <= 20) {
              if(df_cats$concrete[v]  <= 50 && df_cats$concrete[v]  >= 25 || df_cats$mixed[v] <=30 && df_cats$mixed[v] >= 14) {
                
                df_cats$substrate_score[v] <- 1
                
              } else { if (df_cats$fine_sediment[v] >=21 &&  df_cats$fine_sediment[v] <=80) {
                if(df_cats$coarse_sediment[v]  >=21){
                  if(df_cats$extra_coarse[v] >= 21  && df_cats$extra_coarse[v] <= 40) {
                    if(df_cats$concrete[v]  <= 25 || df_cats$mixed[v]  > 31) {
                      
                      df_cats$substrate_score[v] <- 2
                      
                    } else {
                      
                      df_cats$substrate_score[v] <- NA
                    }
                    
                  }
                }
              }
              }  
            }
          }
        }
        }
      }
    }
  }
}


}     






df_cats   
## carry on here with adding mixed variable

# how large is the site?
# thresholds revise in line with life stage coarse sediment = 40% for  

# assign score to each variable proportion

for(v in 2:length(rows)) {
  
  
  
  if(vars[s] == "Percent Cobble" && data_dat[data_dat$variablename == vars[s], "result"]  <= 20) {
    data_dat$sub_score [s] <- 0
    
  } else if (vars[s] == "Percent Cobble" && data_dat[data_dat$variablename == vars[s], "result"]  > 20 && data_dat[data_dat$variablename == vars[s], "result"] <= 40) {
    data_dat$sub_score [s] <- 1
    
  } else if (vars[s] == "Percent Cobble" && data_dat[data_dat$variablename == vars[s], "result"]  > 40 && data_dat[data_dat$variablename == vars[s], "result"] <= 60) {
    data_dat$sub_score [s] <- 2
    
  } else if (vars[s] == "Percent Cobble" && data_dat[data_dat$variablename == vars[s], "result"]  > 60) {
    data_dat$sub_score [s] <- 3
    
  } else if (vars[s] == "Percent Fines (silts/clay/muck)" && data_dat[data_dat$variablename == vars[s], "result"]  >= 0.0001) {
    data_dat$sub_score [s] <- 0
    
  } else if (vars[s] == "Percent Gravel - coarse" && data_dat[data_dat$variablename == vars[s], "result"]  < 5) {
    data_dat$sub_score [s] <- 0
    
  }else if (vars[s] == "Percent Gravel - coarse" && data_dat[data_dat$variablename == vars[s], "result"]  >= 5 && data_dat[data_dat$variablename == vars[s], "result"] <= 20) {
    data_dat$sub_score [s] <- 1
    
  } else if (vars[s] == "Percent Gravel - coarse" && data_dat[data_dat$variablename == vars[s], "result"]  > 20 && data_dat[data_dat$variablename == vars[s], "result"] <= 40) {
    data_dat$sub_score [s] <- 7
    
  } else if (vars[s] == "Percent Gravel - coarse" && data_dat[data_dat$variablename == vars[s], "result"]  > 40 && data_dat[data_dat$variablename == vars[s], "result"] <= 60) {
    data_dat$sub_score [s] <- 8
    
  } else if (vars[s] == "Percent Gravel - coarse" && data_dat[data_dat$variablename == vars[s], "result"]  > 60) {
    data_dat$sub_score [s] <- 9
    
  } else if (vars[s] == "Percent Gravel - fine" && data_dat[data_dat$variablename == vars[s], "result"]  < 5) {
    data_dat$sub_score [s] <- 0
    
  }else if (vars[s] == "Percent Gravel - fine" && data_dat[data_dat$variablename == vars[s], "result"]  >= 5 && data_dat[data_dat$variablename == vars[s], "result"] <= 20) {
    data_dat$sub_score [s] <- 1
    
  } else if (vars[s] == "Percent Gravel - fine" && data_dat[data_dat$variablename == vars[s], "result"]  > 20 && data_dat[data_dat$variablename == vars[s], "result"] <= 40) {
    data_dat$sub_score [s] <- 7
    
  } else if (vars[s] == "Percent Gravel - fine" && data_dat[data_dat$variablename == vars[s], "result"]  > 40 && data_dat[data_dat$variablename == vars[s], "result"] <= 60) {
    data_dat$sub_score [s] <- 8
    
  } else if (vars[s] == "Percent Gravel - fine" && data_dat[data_dat$variablename == vars[s], "result"]  > 60) {
    data_dat$sub_score [s] <- 9
    
  } else if (vars[s] == "Percent Sand" && data_dat[data_dat$variablename == vars[s], "result"]  < 5) {
    data_dat$sub_score [s] <- 0
    
  }else if (vars[s] == "Percent Sand" && data_dat[data_dat$variablename == vars[s], "result"]  >= 5 && data_dat[data_dat$variablename == vars[s], "result"] <= 20) {
    data_dat$sub_score [s] <- 1
    
  } else if (vars[s] == "Percent Sand" && data_dat[data_dat$variablename == vars[s], "result"]  > 20 && data_dat[data_dat$variablename == vars[s], "result"] <= 40) {
    data_dat$sub_score [s] <- 3
    
  } else if (vars[s] == "Percent Sand" && data_dat[data_dat$variablename == vars[s], "result"]  > 40 && data_dat[data_dat$variablename == vars[s], "result"] <= 60) {
    data_dat$sub_score [s] <- 4
    
  } else if (vars[s] == "Percent Sand" && data_dat[data_dat$variablename == vars[s], "result"]  > 60) {
    data_dat$sub_score [s] <- 5
    
  } else if (vars[s] == "Percent Boulders - small" && data_dat[data_dat$variablename == vars[s], "result"]  < 5) {
    data_dat$sub_score [s] <- 0
    
  }else if (vars[s] == "Percent Boulders - small" && data_dat[data_dat$variablename == vars[s], "result"]  >= 5 && data_dat[data_dat$variablename == vars[s], "result"] <= 20) {
    data_dat$sub_score [s] <- 1
    
  } else if (vars[s] == "Percent Boulders - small" && data_dat[data_dat$variablename == vars[s], "result"]  > 20 && data_dat[data_dat$variablename == vars[s], "result"] <= 40) {
    data_dat$sub_score [s] <- 1
    
  } else if (vars[s] == "Percent Boulders - small" && data_dat[data_dat$variablename == vars[s], "result"]  > 40 && data_dat[data_dat$variablename == vars[s], "result"] <= 60) {
    data_dat$sub_score [s] <- 2
    
  } else if (vars[s] == "Percent Boulders - small" && data_dat[data_dat$variablename == vars[s], "result"]  > 60) {
    data_dat$sub_score [s] <- 3
    
  } else if (vars[s] == "Percent Concrete/Asphalt" && data_dat[data_dat$variablename == vars[s], "result"]  >= 80) {
    data_dat$sub_score [s] <- 0
    
  } else if (vars[s] == "Mixed" && data_dat[data_dat$variablename == vars[s], "result"]  > 60) {
    data_dat$sub_score [s] <- 15
    
  } else if (vars[s] == "Mixed" && data_dat[data_dat$variablename == vars[s], "result"]  > 40 && data_dat[data_dat$variablename == vars[s], "result"] <= 60) {
    data_dat$sub_score [s] <- 10
    
  } else if (vars[s] == "Mixed" && data_dat[data_dat$variablename == vars[s], "result"]  >= 20 && data_dat[data_dat$variablename == vars[s], "result"] <= 40) {
    data_dat$sub_score [s] <-5
    
  } else {
    data_dat$sub_score [s] <- 0
    
    
    
    
  }
  
}

# # add any combination of gravel, cobble, sand and boulder >50 = 15
# data_dat[data_dat$variablename == c("Percent Boulders - small", "Percent Sand", 
#                                     "Percent Gravel - fine", "Percent Gravel - coarse"), "result"]

sx<- paste(dfx$LAR_sites[s])
dx <- paste(dfx$date[s])

## save each site/date separately
write.csv(data_dat, paste("output_data/substrate_scores/02_", sx, "_", dx, "_", y, "_substrate_scores.csv", sep="" ))

df <- as.data.frame(matrix(ncol=6))
colnames(df) <- c("site", "date", "sitename", "lat", "lon", "substrate_score")


df$site <- paste(dfx$LAR_sites[1])
df$date <- paste(dfx$sampledate[1])
df$sitename <- paste(dfx$sitename[1])
df$lat <- paste(dfx$targetlatitude[1])
df$lon <- paste(dfx$targetlongitude[1])
df$substrate_score <- sum(data_dat$sub_score)

dfc <- rbind(dfc, df)

}

}


dim(dfc)
dfc

write.csv(dfc, "output_data/02_substrate_scores_all_LAR_sites.csv")

#  validate hiugh scores with raw data and spatial 
high_scores <- subset(dfc, substrate_score >= 15)
dim(high_scores) # 42
high_scores

high_sites <- unique(high_scores$site) # 21
high_sites

# 412M08638 good for example why mixed gives higher results

# [1] "412M08608" "412M08638" "412M08641" "412M08647" "412WE0552"
# [6] "LALT502"   "SMC00520"  "SMC00924"  "SMC01040"  "SMC01096" 
# [11] "SMC01196"  "SMC01320"  "SMC01544"  "SMC01692"  "SMC02088" 
# [16] "SMC02092"  "SMC02568"  "SMC04880"  "SMC05020"  "SMC05640" 
# [21] "SMC06044" 

list.files("output_data/substrate_scores/", pattern = high_sites[7])


### temperature

#upload and clean substrate data
lar_data <- read.csv("input_data/LAR_sites_PHab_metrics_env.csv")
head(lar_data)
unique(lar_data$grouptype)

temp <- subset(lar_data, variablename == "Mean Water Temperature (C)")
temp$result <- as.numeric(as.character(temp$result))
dim(temp)
str(temp)
min(temp$result)
max(temp$result)
mean(temp$result)
sort(temp$result)

## juveniles 15 - 22
## adults ok 23-27
## survival 30-33/38

# time dependent 
# larvae -  may to june (march - july) 
# spawning - may to june (march - july)
# larvae - 18 - 24 
# spawning 10-26

## look at timing of high temps

high_temps <- subset(temp, result >  29.8)
high_temps # may, june and july

h_sites <- high_temps$LAR_sites

highs <- subset(lar_data, LAR_sites )





