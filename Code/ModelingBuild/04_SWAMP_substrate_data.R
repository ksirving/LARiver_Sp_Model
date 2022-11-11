### SWAMP data stuff

setwd("/Users/katieirving/Documents/git/flow_eco_mech")

library(tidyverse)
library(raster)


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


# clean data for loop
lar_substrate$result <- as.numeric(as.character(lar_substrate$result))
lar_substrate <- separate(lar_substrate, col=sampledate, into =c("date", "time", "time2"), sep=c(" "), remove=F)
lar_substrate$date <- gsub("/", "_", lar_substrate$date)

# list sites
sites <- unique(lar_substrate$LAR_sites)
sites

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
write.csv(df_cats, "output_data/substrate_scores/04_substrate_categories.csv")

df_cats <- read.csv("output_data/substrate_scores/04_substrate_categories.csv")
df_cats <- df_cats[-1,]
str(df_cats)
df_cats

df_cats$lat <- as.numeric(as.character(df_cats$lat))
df_cats$lon <- as.numeric(as.character(df_cats$lon))
coordinates(df_cats) <- c("lon", "lat")
proj4string(df_cats) <- CRS("+proj=longlat +ellps=GRS80 +no_defs")
shapefile(df_cats, "output_data/04_swamp_sub_sites_points.shp", overwrite=T)


## subset sites with less than 50% concrete

df_cats_c <- subset(df_cats, concrete <= 50)



## bio nodes 

new_nodes_sp <- shapefile("input_data/spatial/New_Nodes_Bio.shp")
new_nodes_sp

proj4string(df_cats) <- proj4string(new_nodes_sp)

df_cats<-spTransform(df_cats,CRS(proj4string(new_nodes_sp)))
                     
                     
                     
                     
                     

