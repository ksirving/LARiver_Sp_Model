### flow-ecology - hybrid species modelling
## Katie Irving

## consolidate data

# setwd("/Users/katieirving/Documents/git/flow_eco_mech")

library(tidyverse)
library(magrittr)
library(glue)

# various data sets from literature. fish info, e.g. abundance, with environmental variables in various formats.
# purpose here is to consolidate and standardize
# ideally need a dataset with sites/abundances/presences ~ env variables

# upload data from same source to combine
#  Thompson 2010

# coarse substrate and discharge info vs abundance. 3 x sites 
thomp_dish <- read.table("input_data/bio/Thompson_2010_dis_v_abundance.txt", header=T)
thomp_dish

thomp_sub <- read.table("input_data/bio/Thompson_2010_sub_v_abundancev2.txt", header=T)
thomp_sub


# merge by site and year - abundance should be the same per site and year?? check they are! 
# will need error adjustments for data thief


dis_sub <- merge(thomp_sub, thomp_dish, by=c("site", "year"), all=T)
dis_sub
 # 2 x b 2004 - check figure!

# thomp_dish$s_symbol <- 
 sites <- thomp_dish$site
  
  for(x in 1: length (sites)) {
    
    if(thomp_dish$site[x] == "a") {
      thomp_dish$s_symbol[x] <- paste("circle") 
      
    } else if  (thomp_dish$site[x] == "b") {
      thomp_dish$s_symbol[x] <- paste("square") 
      
    } else {
      thomp_dish$s_symbol[x] <- paste("diamond")
    }
    }
  
 thomp_dish
  years
years <- thomp_dish$year
 
 for(x in 1: length (years)) {
   
   if(thomp_dish$year[x] == "2003") {
     thomp_dish$y_symbol[x] <- paste("solid") 
     
   } else if  (thomp_dish$year[x] == "2004") {
     thomp_dish$y_symbol[x] <- paste("curved_diag") 
     
   } else if (thomp_dish$year[x] == "2005") { 
     thomp_dish$y_symbol[x] <- paste("open")
   
   } else if (thomp_dish$year[x] == "2006") {
     thomp_dish$y_symbol[x] <- paste("horizontal")  
     
   } else if (thomp_dish$year[x] == "2007") {
     thomp_dish$y_symbol[x] <- paste("vertical") 
     
   } else {
     thomp_dish$y_symbol[x] <- paste("diagonal") 
   }
     
     }
 
 
 thomp_dish # row 9 - b 2004 incorrect. change to 2008 (diagonal)
 thomp_dish[9,4] <- 2008
thomp_dish # correct

#  run if else again to change 

years <- thomp_dish$year

for(x in 1: length (years)) {
  
  if(thomp_dish$year[x] == "2003") {
    thomp_dish$y_symbol[x] <- paste("solid") 
    
  } else if  (thomp_dish$year[x] == "2004") {
    thomp_dish$y_symbol[x] <- paste("curved_diag") 
    
  } else if (thomp_dish$year[x] == "2005") { 
    thomp_dish$y_symbol[x] <- paste("open")
    
  } else if (thomp_dish$year[x] == "2006") {
    thomp_dish$y_symbol[x] <- paste("horizontal")  
    
  } else if (thomp_dish$year[x] == "2007") {
    thomp_dish$y_symbol[x] <- paste("vertical") 
    
  } else {
    thomp_dish$y_symbol[x] <- paste("diagonal") 
  }
  
}

thomp_dish

dis_sub <- merge(thomp_sub, thomp_dish, by=c("site", "year"), all=T)
dis_sub

## sites and years match - remove columns
names(dis_sub)
dis_sub <- dis_sub[, c(1,2,3,4, 7, 8)]
dis_sub

# abundance is sq root transformed
# inverse by squaring values

dis_sub$ab_x_sq <- dis_sub$abundance.x^2
dis_sub$ab_y_sq <- dis_sub$abundance.y^2
#  round up
dis_sub$ab_x_sq <- round(dis_sub$ab_x_sq, 2)
dis_sub$ab_y_sq <- round(dis_sub$ab_y_sq, 2)

# match abundances - should be more or less the same
r_ab_x <- round(dis_sub$ab_x_sq)
r_ab_y <-round(dis_sub$ab_y_sq)
#  use rounded figure and mean - most are only 1 or so off
dis_sub$ab_mean <-  round((r_ab_x + r_ab_y)/2, 0) 
dis_sub
 #  clean up/remove unwanted columns

dis_sub <- dis_sub[,c(1,2,4,6,9)]
dis_sub  

write.csv(dis_sub, "output_data/bio/00_thompson_clean.csv")  

dis_sub <- read.csv("output_data/bio/00_thompson_clean.csv")
dis_sub
# upload environmental varaiables from table
env_vars <- read.csv("input_data/bio/Thompson_env_data.csv")
env_vars <- env_vars[-16,] # remove extra row
env_vars$Site <- as.character(env_vars$Site)
env_vars$X <- NULL
env_vars$Year <- as.numeric(as.character(env_vars$Year))
# replace small letters with capitals to match dfs
dis_sub_site <- dis_sub$site

dis_sub_site %<>%
  gsub("a","A", .) %>%
    gsub("b", "B", .) %>%
      gsub("c", "C", .)


dis_sub$site <- dis_sub_site
colnames(dis_sub) [2:3] <- c("Site", "Year")
dis_sub$Year <- as.numeric(as.character(dis_sub$Year))

str(dis_sub)
str(env_vars)
# merge datasets 
all_data <- merge(dis_sub, env_vars, by=c("Year", "Site"), all=T)
all_data$X <- NULL
all_data$method <- paste("electro3")
all_data$scale_m <- paste("100")
write.csv(all_data, "output_data/bio/00_Thompson_all_data_clean.csv")
#################
# upload and clean SMEA data 

#  2002 - 2008 (missing 2006)
#  thompson & SMEA - survey method?? habitat utilization = observations

## upload SMEA data
#  adult

dep_ad <- read.csv("input_data/bio/SMEA_depth_2003_2004_adult.csv", header=T)
sub_ad <- read.csv("input_data/bio/SMEA_substrate_2003_2004_adult.csv", header=T)
vel_ad <- read.csv("input_data/bio/SMEA_velocity_2003_2004_adult.csv", header =T)


sub_ad
smea_perc <- sub_ad[,c(1,5,8)]
write.csv(smea_perc, "output_data/bio/00_smea_total_percentage.csv")

# transform percentage into abundance
#  seapare years

 dep_ad
dep_ad_03 <- dep_ad[, 1:5]
dep_ad_04 <- dep_ad[,c(1,6:8)]
dep_ad_03
# install.packages("glue")
library(glue)

#  2003
write.csv(dep_ad_03, "output_data/bio/00_SMEA_depth_adult_2003.csv")
write.csv(dep_ad_04, "output_data/bio/00_SMEA_depth_adult_2004.csv")

dep_ad_03$M_Blvd_ab <- round(dep_ad_03$M_Blvd/100*dep_ad_03[16,2])
dep_ad_03$Highway_60_ab <- round(dep_ad_03$Highway_60/100*dep_ad_03[16,3])
dep_ad_03$Riverside_Dr_ab <- round(dep_ad_03$Riverside_Dr/100*dep_ad_03[16,4])
dep_ad_03$all_sites_ab <- round(dep_ad_03$all_sites/100*dep_ad_03[16,5])

#  change autocorrected date to deoth category and character
dep_ad_03$Depth <- as.character(dep_ad_03$Depth)
dep_ad_03[2,1] <- paste("06-10")
dep_ad_03[3,1] <- paste("11-15")
dep_ad_03

#  2004
dep_ad_04

colnames(dep_ad_04) [2:3] <- c("july_Highway60", "Aug_Highway60_Mission")
dep_ad_04$july_Highway60_ab <- round(dep_ad_04$july_Highway60/100*dep_ad_04[16,2])
dep_ad_04$Aug_Highway60_Mission_ab <- round(dep_ad_04$Aug_Highway60_Mission/100*dep_ad_04[16,3])
dep_ad_04$total <- round(dep_ad_04$total/100*dep_ad_04[16,3])

dep_ad_04$Depth <- as.character(dep_ad_04$Depth)
dep_ad_04[2,1] <- paste("06-10")
dep_ad_04[3,1] <- paste("11-15")

# save 
write.csv(dep_ad_03, "output_data/bio/00_SMEA_adult_depth_2003_abundance.csv")
write.csv(dep_ad_04, "output_data/bio/00_SMEA_adult_depth_2004_abundance.csv")


####################
# transform percentage into abundance
#  seaparate years
#  substrate


sub_ad_03 <- sub_ad[, 1:5]
sub_ad_04 <- sub_ad[,c(1,6:8)]
sub_ad_03

write.csv(sub_ad_03, "output_data/bio/00_SMEA_substrate_adult_2003.csv")
write.csv(sub_ad_04, "output_data/bio/00_SMEA_substrate_adult_2004.csv")

# 2003
sub_ad_03$M_Blvd_ab <- round(sub_ad_03$M_Blvd/100*sub_ad_03[10,2])
sub_ad_03$Highway_60_ab <- round(sub_ad_03$Highway_60/100*sub_ad_03[10,3])
sub_ad_03$Riverside_Dr_ab <- round(sub_ad_03$Riverside_Dr/100*sub_ad_03[10,4])
sub_ad_03$all_sites_ab <- round(sub_ad_03$all_sites/100*sub_ad_03[10,5])
sub_ad_03

#  2004
sub_ad_04

colnames(sub_ad_04) [2:3] <- c("july_Highway60", "Aug_Highway60_Mission")
sub_ad_04$july_Highway60_ab <- round(sub_ad_04$july_Highway60/100*sub_ad_04[10,2])
sub_ad_04$Aug_Highway60_Mission_ab <- round(sub_ad_04$Aug_Highway60_Mission/100*sub_ad_04[10,3])
sub_ad_04$total <- round(sub_ad_04$total/100*sub_ad_04[10,3])

# save 
write.csv(sub_ad_03, "output_data/bio/00_SMEA_adult_substrate_2003_abundance.csv")
write.csv(sub_ad_04, "output_data/bio/00_SMEA_adult_substrate_2004_abundance.csv")

########
## velocity

vel_ad_03 <- vel_ad[, 1:5]
vel_ad_04 <- vel_ad[,c(1,6:8)]
vel_ad_03

write.csv(vel_ad_03, "output_data/bio/00_SMEA_velocity_adult_2003.csv")
write.csv(vel_ad_04, "output_data/bio/00_SMEA_velocity_adult_2004.csv")

# 2003
vel_ad_03$M_Blvd_ab <- round(vel_ad_03$M_Blvd/100*vel_ad_03[7,2])
vel_ad_03$Highway_60_ab <- round(vel_ad_03$Highway_60/100*vel_ad_03[7,3])
vel_ad_03$Riverside_Dr_ab <- round(vel_ad_03$Riverside_Dr/100*vel_ad_03[7,4])
vel_ad_03$all_sites_ab <- round(vel_ad_03$all_sites/100*vel_ad_03[7,5])
vel_ad_03

#  2004
vel_ad_04

colnames(vel_ad_04) [2:3] <- c("july_Highway60", "Aug_Highway60_Mission")
vel_ad_04$july_Highway60_ab <- round(vel_ad_04$july_Highway60/100*vel_ad_04[7,2])
vel_ad_04$Aug_Highway60_Mission_ab <- round(vel_ad_04$Aug_Highway60_Mission/100*vel_ad_04[7,3])
vel_ad_04$total <- round(vel_ad_04$total/100*vel_ad_04[7,3])

# save 
write.csv(vel_ad_03, "output_data/bio/00_SMEA_adult_velocity_2003_abundance.csv")
write.csv(vel_ad_04, "output_data/bio/00_SMEA_adult_velocity_2004_abundance.csv")


##################
##### juvenile

dep_juv <- read.csv("input_data/bio/SMEA_depth_2003_2004_juvenile.csv", header=T)
sub_juv <- read.csv("input_data/bio/SMEA_substrate_2003_2004_juvenile.csv", header=T)
vel_juv <- read.csv("input_data/bio/SMEA_velocity_2003_2004_juvenile.csv", header =T)


dep_juv

# transform percentage into abundance
#  seapare years


dep_juv_03 <- dep_juv[, 1:5]
dep_juv_04 <- dep_juv[,c(1,6:8)]

# install.packages("glue")


#  2003
write.csv(dep_juv_03, "output_data/bio/00_SMEA_depth_juvenile_2003.csv")
write.csv(dep_juv_04, "output_data/bio/00_SMEA_depth_juvenile_2004.csv")

dep_juv_03$M_Blvd_ab <- round(dep_juv_03$M_Blvd/100*dep_juv_03[15,2])
dep_juv_03$Highway_60_ab <- round(dep_juv_03$Highway_60/100*dep_juv_03[15,3])
dep_juv_03$Riverside_Dr_ab <- round(dep_juv_03$Riverside_Dr/100*dep_juv_03[15,4])
dep_juv_03$all_sites_ab <- round(dep_juv_03$all_sites/100*dep_juv_03[15,5])

#  change autocorrected date to depth category and character
dep_juv_03$Depth <- as.character(dep_juv_03$Depth)
dep_juv_03[2,1] <- paste("6-10")
dep_juv_03[3,1] <- paste("11-15")
dep_juv_03

#  2004
dep_juv_04

colnames(dep_juv_04) [2:3] <- c("july_Highway60", "Aug_Highway60_Mission")
dep_juv_04$july_Highway60_ab <- round(dep_juv_04$july_Highway60/100*dep_juv_04[15,2])
dep_juv_04$Aug_Highway60_Mission_ab <- round(dep_juv_04$Aug_Highway60_Mission/100*dep_juv_04[15,3])

dep_juv_04$total_ab <- round(dep_juv_04$total/100*dep_juv_04[15,4])

dep_juv_04$Depth <- as.character(dep_juv_04$Depth)
dep_juv_04[2,1] <- paste("6-10")
dep_juv_04[3,1] <- paste("11-15")

# save 
write.csv(dep_juv_03, "output_data/bio/00_SMEA_juvenile_depth_2003_abundance.csv")
write.csv(dep_juv_04, "output_data/bio/00_SMEA_juvenile_depth_2004_abundance.csv")


####################
# transform percentage into abundance
#  seaparate years
#  substrate


sub_juv_03 <- sub_juv[, 1:5]
sub_juv_04 <- sub_juv[,c(1,6:8)]
sub_juv_03

write.csv(sub_juv_03, "output_data/bio/00_SMEA_substrate_juvenile_2003.csv")
write.csv(sub_juv_04, "output_data/bio/00_SMEA_substrate_juvenile_2004.csv")

# 2003
sub_juv_03$M_Blvd_ab <- round(sub_juv_03$M_Blvd/100*sub_juv_03[10,2])
sub_juv_03$Highway_60_ab <- round(sub_juv_03$Highway_60/100*sub_juv_03[10,3])
sub_juv_03$Riverside_Dr_ab <- round(sub_juv_03$Riverside_Dr/100*sub_juv_03[10,4])
sub_juv_03$all_sites_ab <- round(sub_juv_03$all_sites/100*sub_juv_03[10,5])
sub_juv_03

#  2004
sub_juv_04

colnames(sub_juv_04) [2:3] <- c("july_Highway60", "Aug_Highway60_Mission")
sub_juv_04$july_Highway60_ab <- round(sub_juv_04$july_Highway60/100*sub_juv_04[10,2])
sub_juv_04$Aug_Highway60_Mission_ab <- round(sub_juv_04$Aug_Highway60_Mission/100*sub_juv_04[10,3])
sub_juv_04$total <- round(sub_juv_04$total/100*sub_juv_04[10,3])

# save 
write.csv(sub_juv_03, "output_data/bio/00_SMEA_juvenile_substrate_2003_abundance.csv")
write.csv(sub_juv_04, "output_data/bio/00_SMEA_juvenile_substrate_2004_abundance.csv")

########
## velocity

vel_juv_03 <- vel_juv[, 1:5]
vel_juv_04 <- vel_juv[,c(1,6:8)]
vel_juv_03

write.csv(vel_juv_03, "output_data/bio/00_SMEA_velocity_juvenile_2003.csv")
write.csv(vel_juv_04, "output_data/bio/00_SMEA_velocity_juvenile_2004.csv")

# 2003
vel_juv_03$M_Blvd_ab <- round(vel_juv_03$M_Blvd/100*vel_juv_03[7,2])
vel_juv_03$Highway_60_ab <- round(vel_juv_03$Highway_60/100*vel_juv_03[7,3])
vel_juv_03$Riverside_Dr_ab <- round(vel_juv_03$Riverside_Dr/100*vel_juv_03[7,4])
vel_juv_03$all_sites_ab <- round(vel_juv_03$all_sites/100*vel_juv_03[7,5])
vel_juv_03

#  2004
vel_juv_04

colnames(vel_juv_04) [2:3] <- c("july_Highway60", "Aug_Highway60_Mission")
vel_juv_04$july_Highway60_ab <- round(vel_juv_04$july_Highway60/100*vel_juv_04[7,2])
vel_juv_04$Aug_Highway60_Mission_ab <- round(vel_juv_04$Aug_Highway60_Mission/100*vel_juv_04[7,3])
vel_juv_04$total <- round(vel_juv_04$total/100*vel_juv_04[7,3])

# save 
write.csv(vel_juv_03, "output_data/bio/00_SMEA_juvenile_velocity_2003_abundance.csv")
write.csv(vel_juv_04, "output_data/bio/00_SMEA_juvenile_velocity_2004_abundance.csv")

# all will need last row removed - abundance.

#  SAS data
#  upload - cobble and sand data only
cobble <- read.csv("input_data/bio/SAS_2014_abundance_v_cobble_correct_percent.csv")
cobble
sand <- read.csv("input_data/bio/SAS_2014_abundance_v_sandsilt_percent.csv")
sand
#  round abundance to nearest whole number - data thief error
cobble$Abundance_r <- round(cobble$abundance)
sand$Abundance_r <- round(sand$Abundance)

#  they should match - they do!!
cobble$Abundance_r %in% sand$Abundance_r
?order
cobble <- cobble[order(cobble$Abundance_r,decreasing=T),]
sand <- sand[order(sand$Abundance_r,decreasing=T),] 
sand
cobble

substrate <- cbind(sand,cobble)
substrate

#  remove old abundance columns
substrate <- substrate[,-c(1,5,6)]
# data combined but can't be 100% sure that cobble v sand are correct in zero abundance sites

write.csv(substrate, "output_data/bio/00_SAS_2014_abundance_substrate.csv")

# Brown et al 2005

#  upload 
brown <- read.csv("input_data/bio/Brown_etal_2005_env_vars.csv", header=T)
brown
#  select relevant columns

brown <- brown[,c(1,2,7,9,10,11, 12)]
colnames(brown) <- c("Site", "Discharge", "Depth", "Substrate", "Spec_cond", "Temp","P/A")

#  add year and fish method

brown$Year <- paste("2000")
brown$Method <-paste("electro1")

brown$Substrate_cat  

#  covert substrate values to category


sites <- brown$Site
sites
x=1

for(x in 1: length (sites)) {

if(brown$Substrate[x] <= 1.5) {
  brown$Substrate_cat [x] <- paste("Concrete") 
  
} else if (brown$Substrate[x] >= 1.6 && brown$Substrate[x] <= 2.5) {
  brown$Substrate_cat [x] <- paste("Silt_mud") 
  
} else if (brown$Substrate[x] >= 2.6 && brown$Substrate[x] <= 3.5) {
  brown$Substrate_cat [x] <- paste("Sand") 
  
} else if (brown$Substrate[x] >= 3.6 && brown$Substrate[x] <= 4.5) {
  brown$Substrate_cat [x] <- paste("Fine_Sediment")   
  
} else if (brown$Substrate[x] >= 4.6 && brown$Substrate[x] <= 5.5) {
  brown$Substrate_cat [x] <- paste("Coarse_Sediment") 
  
} else if (brown$Substrate[x] >= 5.6 && brown$Substrate[x] <= 6.5) {
  brown$Substrate_cat [x] <- paste("Very_Coarse_Sediment") 
  
} else if (brown$Substrate[x] >= 6.6 && brown$Substrate[x] <= 7.5) {
  brown$Substrate_cat [x] <- paste("Small_Cobble") 
 
  
} else {
  brown$Substrate_cat [x] <- paste("Large_Cobble") 
}

}

brown
write.csv(brown, "output_data/bio/00_Brown_2000_abundance_env_vars.csv")  

# SAWA report 2014

## upload

abund <- read.csv("input_data/bio/SAWA_2014_sas_abundance.csv")
colnames(abund)[2] <- "abundance"
abund
env_vars <- read.csv("input_data/bio/SAWA_2014_env_vars.csv")
env_vars
env_vars <- env_vars[,c(1,2,3,7)]
colnames(env_vars) <- c("Site", "Section", "Temp", "pH")


hab_vars <- read.csv("input_data/bio/SAWA_2014_hab_vars.csv")
hab_vars
hab_vars <- hab_vars[,c(1,2,3,4,5,6,9:13)]
colnames(hab_vars) <- c("Site", "Section", "Channel_width_m", "Max_depth_cm", 
                        "depth_edge_east_cm", "depth_edge_west_cm", "mud_silt", 
                        "sand", "gravel", "cobble", "boulder" )

# merge env and hab data
env_hab_vars <- merge(hab_vars, env_vars, by=c("Site", "Section"))
env_hab_vars

#  merge with abundance

abun_env_hab <- merge(env_hab_vars, abund, by="Site", all=T)

abun_env_hab
# substrate in different format - will need to standardise to combine. 
# don't know what the repeated measurements mean

write.csv(abun_env_hab, "output_data/bio/00_SAWA_2014_env_hab_abundance.csv")

# envicraft - only for temp range

envicr <- read.csv("input_data/bio/Enicraft_survey_and_reloc_2010_appendix.csv")

envicr <- envicr[-c(1,12:19),] 
rownames(envicr) <- envicr$Date

envicr <- as.data.frame(t(envicr))
envicr <- envicr[-1,]
envicr

write.csv(envicr, "output_data/bio/00_Envicraft_2010_Temp_abundance.csv")

# ######## Saiki - distribution data

depth <- read.csv("input_data/bio/Saiki_et_al_2000_Depth_dist.csv")
depth

substrate <- read.csv("input_data/bio/Saiki_et_al_2000_substrate_dist.csv")
substrate # only sand present at MWD

velo <- read.csv("input_data/bio/Saiki_et_al_2000_velocity_dist.csv")
velo  

sum(depth$SGR_N) # 532
sum(depth$MWD_N) # 157

sum(substrate$SGR_N) # 531
sum(substrate$MWD_N) # 158 # same in paper - don't know why numbers different - error?

sum(velo$SGR_N) # 532
sum(velo$MWD_N) # 157

## save
write.csv(depth, "output_data/bio/00_Saiki_2000_depth_dist.csv")
write.csv(substrate, "output_data/bio/00_Saiki_2000_substrate_dist.csv")
write.csv(velo, "output_data/bio/00_Saiki_2000_velocity_dist.csv")

# SAR 2015 

# upload data

micro_avail <- read.csv("input_data/bio/SAR 2015 Microhabitat Availability.csv")
micro_use <- read.csv("input_data/bio/SAR 2015 Microhabitat Use.csv")
fish_data <- read.csv("input_data/bio/SAR 2015 Reach Fish Data.csv")

colnames(micro_avail)[8:13] <- c("Transect", "ch_width_m", "Depth_cm", "Velocity_0.6_ms","Velocity_0.2_ms", "Velocity_0.8_ms")
micro_avail <- micro_avail[,-c(4, 19,20)]
names(micro_avail)

names(micro_use) [14:20] <- c("length_mm", "Distance_fish_abv_bottom_cm", "Velocity_at_fish_ms", "Depth_cm", "Velocity_0.6_ms", "Velocity_0.2_ms", "Velocity_0.8_ms")
head(micro_use)
micro_use$coord_code <- paste(micro_use$Latitude, "_", micro_use$Longitude, sep="")
unique(micro_use$coord_code) # 11 sites in total
dim(micro_use)

#  make sure species name is homogenous

micro_use$Species <- gsub("Santa Ana sucker", "Santa Ana Sucker", micro_use$Species)
unique(micro_use$Species)

#  look at sucker sites

micro_suck <- subset(micro_use, Species=="Santa Ana Sucker")
dim(micro_suck) # 136

unique(micro_suck$coord_code) # present at all sites

#  combine datasets??

names(micro_avail) # habitat info
names(micro_use) # species, abundance and habitat
names(fish_data) # species info e.g. FL etc

# size classes - TL=5.9122 + 1.1092*SL
# standard length juvenile = 21-30mm

head(fish_data)

#  add life stage
str(fish_data)
fish_data$Fork.length <- as.numeric(as.character(fish_data$Fork.length))
fish_data$Total.length <- as.numeric(as.character(fish_data$Total.length))

sum(is.na(fish_data$Fork.length)) # 714
sum(is.na(fish_data$Total.length)) # 2115

na_rows <- which(is.na(fish_data$Fork.length))
fish_data[na_rows,] # total length taken not fork length

# remove fork length NAs and run if/else

fish_dataFL <- fish_data[-na_rows, ]
fish_dataTL <- fish_data[na_rows, ]

fish <- row.names(fish_dataFL)
fish


for(x in 1: length (fish)) {
  
  if(fish_dataFL$Fork.length[x] <= 20) {
    fish_dataFL$life_stage [x] <- paste("Larvae") 
    
  # } else if (fish_data$Fork.length[x] = NA) {
  #   fish_data$life_stage  [x] <- paste("NA") 
    
  } else if (fish_dataFL$Fork.length[x] >= 21 && fish_dataFL$Fork.length[x] <= 30) {
    fish_dataFL$life_stage  [x] <- paste("Juvenile") 

  } else {
    fish_dataFL$life_stage [x] <- paste("Adult") 
  }
  
}

head(fish_dataFL)
unique(fish_dataFL$life_stage)

# run for TL 
#  first estimate sl from TL
#  TL=5.9122 + 1.1092*SL

# SL = (TL-5.9122)/1.1092


x
fish_dataTL$standard.length <- (fish_dataTL$Total.length - 5.9122)/1.1092

sum(is.na(fish_dataTL$standard.length)) #19
na_rows <- which(is.na(fish_dataTL$standard.length))

fish_dataTL <- fish_dataTL[-na_rows,] # remove as no sas here
fish <- row.names(fish_dataTL)
fish

for(x in 1: length (fish)) {
  
  if(fish_dataTL$standard.length[x] <= 20) {
    fish_dataTL$life_stage [x] <- paste("Larvae") 
    
    # } else if (fish_data$Fork.length[x] = NA) {
    #   fish_data$life_stage  [x] <- paste("NA") 
    
  } else if (fish_dataTL$standard.length[x] >= 21 && fish_dataTL$standard.length[x] <= 30) {
    fish_dataTL$life_stage  [x] <- paste("Juvenile") 
    
  } else {
    fish_dataTL$life_stage [x] <- paste("Adult") 
  }
  
}

head(fish_dataFL)
unique(fish_dataTL$life_stage)

fish_dataFL$standard.length <- 0

#  combine datasets

fish_data_all <- rbind(fish_dataFL, fish_dataTL)
unique(fish_data_all$Common.name)
# subset to only SAS to merge length data with env data

fish_data_sas <- subset(fish_data_all, Common.name=="Santa Ana Sucker")
unique(fish_data_sas$life_stage) # only adult


head(micro_avail)
head(micro_suck)
head(fish_data_sas)

#  as all adults, only micro_suck needed

names(micro_suck)

depth <- micro_suck[,c(1:3,5,6,7,12,13,17,27)]
head(depth)
substrate <- micro_suck[,c(1:3,5,6,7,12,13,23,27)]
velocity <- micro_suck[,c(1:3,5,6,7,12,13,16,18:20,27)]

# save 

write.csv(depth, "output_data/bio/00_Wulff_2015_depth_abundance.csv")
write.csv(substrate, "output_data/bio/00_Wulff_2015_substrate_abundance.csv")
write.csv(velocity, "output_data/bio/00_Wulff_2015_velocity_abundance.csv")

#  SAR 2016 

# upload data

micro_avail <- read.csv("input_data/bio/SAR 2016 Microhabitat Availability Data.csv")
micro_use <- read.csv("input_data/bio/SAR 2016 Microhabitat Use Data.csv")
fish_data <- read.csv("input_data/bio/SAR 2016 Reach Fish Data_v3_2.05.2019.csv")
reach_data <- read.csv("input_data/bio/SAR 2016 Reach Habitat Data.csv")

head(micro_avail)

colnames(micro_avail)[7:9] <- c("ch_width_m", "Depth_cm", "Velocity_0.6_ms")
# micro_avail <- micro_avail[,-c(4, 19,20)]
names(micro_avail)

names(micro_use)
names(micro_use) [14:20] <- c("length_mm", "Distance_fish_abv_bottom_cm", "Velocity_at_fish_ms", "Depth_cm", "Velocity_0.6_ms", "Velocity_0.2_ms", "Velocity_0.8_ms")
head(micro_use)
micro_use$coord_code_s <- paste(micro_use$Starting.Latitude, "_", micro_use$Starting.Longitude, sep="")
micro_use$coord_code_e <- paste(micro_use$Ending.Latitude, "_", micro_use$Ending.Longitude, sep="")

unique(micro_use$coord_code_e) # 3 sites in total
dim(micro_use)

#  make sure species name is homogenous
micro_usex
micro_use$Species <- gsub("Santa Ana sucker", "Santa Ana Sucker", micro_use$Species)
unique(micro_use$Species)

#  look at sucker sites

micro_suck <- subset(micro_use, Species=="Santa Ana Sucker")
dim(micro_suck) # 78

unique(micro_suck$coord_code) # present at all sites

#  combine datasets??

names(micro_avail) # habitat info
names(micro_use) # species, abundance and habitat
names(fish_data) # species info e.g. FL etc

# size classes - TL=5.9122 + 1.1092*SL
# standard length juvenile = 21-30mm

head(fish_data)

colnames(fish_data)[10:11] <- c("Total.length", "Fork.length")

#  add life stage
str(fish_data)
fish_data$Fork.length <- as.numeric(as.character(fish_data$Fork.length))
fish_data$Total.length <- as.numeric(as.character(fish_data$Total.length))

sum(is.na(fish_data$Fork.length)) # 1375
sum(is.na(fish_data$Total.length)) # 1907

na_rows <- which(is.na(fish_data$Fork.length))
fish_data[na_rows,] # total length taken not fork length

# remove fork length NAs and run if/else

fish_dataFL <- fish_data[-na_rows, ]
fish_dataTL <- fish_data[na_rows, ]

fish <- row.names(fish_dataFL)
fish


for(x in 1: length (fish)) {
  
  if(fish_dataFL$Fork.length[x] <= 20) {
    fish_dataFL$life_stage [x] <- paste("Larvae") 
    
    # } else if (fish_data$Fork.length[x] = NA) {
    #   fish_data$life_stage  [x] <- paste("NA") 
    
  } else if (fish_dataFL$Fork.length[x] >= 21 && fish_dataFL$Fork.length[x] <= 30) {
    fish_dataFL$life_stage  [x] <- paste("Juvenile") 
    
  } else {
    fish_dataFL$life_stage [x] <- paste("Adult") 
  }
  
}

head(fish_dataFL)
unique(fish_dataFL$life_stage)

# run for TL 
#  first estimate sl from TL
#  TL=5.9122 + 1.1092*SL

# SL = (TL-5.9122)/1.1092


fish_dataTL$standard.length <- (fish_dataTL$Total.length - 5.9122)/1.1092

sum(is.na(fish_dataTL$standard.length)) #19
na_rows <- which(is.na(fish_dataTL$standard.length))

fish_dataTL[na_rows,]

fish_dataTL <- fish_dataTL[-na_rows,] # remove as no sas here
fish <- row.names(fish_dataTL)
fish

for(x in 1: length (fish)) {
  
  if(fish_dataTL$standard.length[x] <= 20) {
    fish_dataTL$life_stage [x] <- paste("Larvae") 
    
    # } else if (fish_data$Fork.length[x] = NA) {
    #   fish_data$life_stage  [x] <- paste("NA") 
    
  } else if (fish_dataTL$standard.length[x] >= 21 && fish_dataTL$standard.length[x] <= 30) {
    fish_dataTL$life_stage  [x] <- paste("Juvenile") 
    
  } else {
    fish_dataTL$life_stage [x] <- paste("Adult") 
  }
  
}

head(fish_dataFL)
unique(fish_dataTL$life_stage)

fish_dataFL$standard.length <- 0

#  combine datasets

fish_data_all <- rbind(fish_dataFL, fish_dataTL)
unique(fish_data_all$Common.Name)
# subset to only SAS to merge length data with env data

fish_data_sas <- subset(fish_data_all, Common.Name=="Santa Ana Sucker")
unique(fish_data_sas$life_stage) # only adult


head(micro_avail)
head(micro_suck)
head(fish_data_sas)
head(reach_data)

micro_suck$site_code <- paste(micro_suck$Location, "_", micro_suck$Section, sep="")
unique(micro_suck$site_code) # 2

micro_avail$site_code <- paste(micro_avail$Location, "_", micro_avail$Section, sep="")
unique(micro_avail$site_code) # 2
micro_avail$coord_code <- paste(micro_avail$Latitude, "_", micro_avail$Longitude, sep="")
unique(micro_avail$coord_code)

reach_data$site_code <- paste(reach_data$Location, "_", reach_data$Section, sep="")
unique(reach_data$site_code) # 7 sites
reach_data$coord_code <- paste(reach_data$Latitude, "_", reach_data$Longitude, sep="")
unique(reach_data$coord_code)
#  as all adults, only micro_suck needed

names(micro_suck)

depth <- micro_suck[,c(1:4,5,6,7,12,13,17,27, 29)]
head(depth)
substrate <- micro_suck[,c(1:4,5,6,7,12,13,21,27, 29)]
velocity <- micro_suck[,c(1:4,5,6,7,12,13,16,18:20,27, 29)]
head(velocity)
head(substrate)
# save 

write.csv(depth, "output_data/bio/00_Wulff_2016_depth_abundance.csv")
write.csv(substrate, "output_data/bio/00_Wulff_2016_substrate_abundance.csv")
write.csv(velocity, "output_data/bio/00_Wulff_2016_velocity_abundance.csv")

# SAR 2017


# upload data
# micro_avail - not taken in 2017

micro_use <- read.csv("input_data/bio/SAR 2017 Microhabitat Use Data_AfterDMS.csv")
fish_data <- read.csv("input_data/bio/SAR 2017 Reach Fish Data_afterDMS.csv")
reach_data <- read.csv("input_data/bio/SAR 2017 Reach Habitat Data_afterDMS.csv")



names(micro_use)
names(micro_use) [10:16] <- c("length_mm", "Distance_fish_abv_bottom_cm", "Velocity_at_fish_ms", "Depth_cm", "Velocity_0.6_ms", "Velocity_0.2_ms", "Velocity_0.8_ms")
head(micro_use)
# micro_use$coord_code_s <- paste(micro_use$Starting.Latitude, "_", micro_use$Starting.Longitude, sep="")
# micro_use$coord_code_e <- paste(micro_use$Ending.Latitude, "_", micro_use$Ending.Longitude, sep="")
# no coordinates

unique(micro_use$coord_code_e) # 3 sites in total
dim(micro_use) # 47

#  make sure species name is homogenous

unique(micro_use$Species) # only SAS

#  look at sucker sites

micro_suck <- subset(micro_use, Species=="Santa Ana Sucker")
dim(micro_suck) # 17

unique(micro_suck$coord_code) # present at all sites

#  combine datasets??

# names(micro_avail) # habitat info
names(micro_use) # species, abundance and habitat
names(fish_data) # species info e.g. FL etc

# size classes - TL=5.9122 + 1.1092*SL
# standard length juvenile = 21-30mm

head(fish_data)

colnames(fish_data)[10:11] <- c("Total.length", "Fork.length")

#  add life stage
str(fish_data)
fish_data$Fork.length <- as.numeric(as.character(fish_data$Fork.length))
fish_data$Total.length <- as.numeric(as.character(fish_data$Total.length))

sum(is.na(fish_data$Fork.length)) # 121
sum(is.na(fish_data$Total.length)) # 3830

na_rows <- which(is.na(fish_data$Fork.length))
fish_data[na_rows,] # total length taken not fork length

# remove fork length NAs and run if/else

fish_dataFL <- fish_data[-na_rows, ]
fish_dataTL <- fish_data[na_rows, ]

fish <- row.names(fish_dataFL)
fish


for(x in 1: length (fish)) {
  
  if(fish_dataFL$Fork.length[x] <= 20) {
    fish_dataFL$life_stage [x] <- paste("Larvae") 
    
    # } else if (fish_data$Fork.length[x] = NA) {
    #   fish_data$life_stage  [x] <- paste("NA") 
    
  } else if (fish_dataFL$Fork.length[x] >= 21 && fish_dataFL$Fork.length[x] <= 30) {
    fish_dataFL$life_stage  [x] <- paste("Juvenile") 
    
  } else {
    fish_dataFL$life_stage [x] <- paste("Adult") 
  }
  
}

head(fish_dataFL)
unique(fish_dataFL$life_stage)

# run for TL 
#  first estimate sl from TL
#  TL=5.9122 + 1.1092*SL

# SL = (TL-5.9122)/1.1092


fish_dataTL$standard.length <- (fish_dataTL$Total.length - 5.9122)/1.1092

sum(is.na(fish_dataTL$standard.length)) #19
na_rows <- which(is.na(fish_dataTL$standard.length))

fish_dataTL[na_rows,]

fish_dataTL <- fish_dataTL[-na_rows,] # remove as no sas here
fish <- row.names(fish_dataTL)
fish

for(x in 1: length (fish)) {
  
  if(fish_dataTL$standard.length[x] <= 20) {
    fish_dataTL$life_stage [x] <- paste("Larvae") 
    
    # } else if (fish_data$Fork.length[x] = NA) {
    #   fish_data$life_stage  [x] <- paste("NA") 
    
  } else if (fish_dataTL$standard.length[x] >= 21 && fish_dataTL$standard.length[x] <= 30) {
    fish_dataTL$life_stage  [x] <- paste("Juvenile") 
    
  } else {
    fish_dataTL$life_stage [x] <- paste("Adult") 
  }
  
}

head(fish_dataFL)
unique(fish_dataTL$life_stage)

fish_dataFL$standard.length <- 0

#  combine datasets

fish_data_all <- rbind(fish_dataFL, fish_dataTL)
unique(fish_data_all$Common.Name)
# subset to only SAS to merge length data with env data

fish_data_sas <- subset(fish_data_all, Common.Name=="Santa Ana Sucker")
unique(fish_data_sas$life_stage) # only adult


head(micro_avail)
head(micro_suck)
head(fish_data_sas)
head(reach_data)

micro_suck$site_code <- paste(micro_suck$Location, "_", micro_suck$Section, "_", micro_suck$Reach, sep="")
unique(micro_suck$site_code) # 2

micro_avail$site_code <- paste(micro_avail$Location, "_", micro_avail$Section, sep="")
unique(micro_avail$site_code) # 2
micro_avail$coord_code <- paste(micro_avail$Latitude, "_", micro_avail$Longitude, sep="")
unique(micro_avail$coord_code)


reach_data$site_code <- paste(reach_data$Location, "_", reach_data$Section, sep="")
unique(reach_data$site_code) # 7 sites
reach_data$coord_code <- paste(reach_data$Latitude, "_", reach_data$Longitude, sep="")
unique(reach_data$coord_code)
#  as all adults, only micro_suck needed

names(micro_suck)

depth <- micro_suck[,c(1:3,8,9,10,13,23)]
head(depth)
substrate <- micro_suck[,c(1:3,8,9,10,17,23)]
velocity <- micro_suck[,c(1:3,8,9,10,12,14,15,16,23)]
head(velocity)
head(substrate)
# save 

write.csv(depth, "output_data/bio/00_Wulff_2017_depth_abundance.csv")
write.csv(substrate, "output_data/bio/00_Wulff_2017_substrate_abundance.csv")
write.csv(velocity, "output_data/bio/00_Wulff_2017_velocity_abundance.csv")



