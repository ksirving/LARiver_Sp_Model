## cleaning for chub and mosquitofish - Martha

# SAR 2015 
library(tidyverse)
# upload data

micro_avail <- read.csv("input_data/bio/SAR 2015 Microhabitat Availability.csv")
micro_use <- read.csv("input_data/bio/SAR 2015 Microhabitat Use.csv")
fish_data <- read.csv("input_data/bio/SAR 2015 Reach Fish Data.csv")

colnames(micro_avail)[8:13] <- c("Transect", "ch_width_m", "Depth_cm", "Velocity_0.6_ms","Velocity_0.2_ms", "Velocity_0.8_ms")
micro_avail <- micro_avail[,-c(4, 19,20)]
names(micro_avail)
## make coord code
micro_avail$coord_code <- paste(micro_avail$Latitude, "_", micro_avail$Longitude, sep="")
unique(micro_avail$coord_code) ## 17



names(micro_use) [14:20] <- c("length_mm", "Distance_fish_abv_bottom_cm", "Velocity_at_fish_ms", "Depth_cm", "Velocity_0.6_ms", "Velocity_0.2_ms", "Velocity_0.8_ms")
head(micro_use)
micro_use$coord_code <- paste(micro_use$Latitude, "_", micro_use$Longitude, sep="")
unique(micro_use$coord_code) # 11 sites in total
dim(micro_use)

unique(micro_avail$coord_code) %in% unique(micro_use$coord_code) 

#  make sure species name is homogenous
micro_usex
micro_use$Species <- gsub("Santa Ana sucker", "Santa Ana Sucker", micro_use$Species)
fish <- unique(micro_use$Species)[c(2,5)]
fish
#  look at sucker sites

micro_suck <- subset(micro_use, Species %in% fish)
dim(micro_suck) # 188  27

unique(micro_suck$coord_code) # present at all sites



#  as all adults, only micro_suck needed

names(micro_suck)

depth <- micro_suck[,c(1:3,5,6,7,12,13,17,18,27)]

names(depth)
# save 

write.csv(depth, "output_data/bio/00_Wulff_2015_Chub_Mos_depth_abundance.csv")

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

micro_avail$coord_code_s <- paste(micro_avail$Latitude, "_", micro_avail$Longitude, sep="")
micro_avail$coord_code_e <- paste(micro_avail$Latitude, "_", micro_avail$Longitude, sep="")

unique(micro_avail$coord_code_e) # 3 sites in total
dim(micro_avail)
names(micro_avail)


unique(micro_use$coord_code_e) # 3 sites in total
dim(micro_use)
names(micro_use)

# unique(micro_use$coord_code_s) # 3 sites in total
# dim(micro_use)
# names(micro_use)

#  look at arroyo sites

micro_suck <- subset(micro_use, Species=="Arroyo Chub")
dim(micro_suck) # 66

unique(micro_suck$coord_code_e) # present at all sites
names(micro_suck)

names(depth)
depth <- micro_suck[,c(1,2,7,4,5,12,13,17,18,27)]
head(depth)

# save 

write.csv(depth, "output_data/bio/00_Wulff_Arroyo_Mos_2016_depth_abundance.csv")

## upload data and merge

depth2015 <- read.csv("output_data/bio/00_Wulff_2015_Chub_Mos_depth_abundance.csv")
depth2016 <- read.csv("output_data/bio/00_Wulff_Arroyo_Mos_2016_depth_abundance.csv")

names(depth2015)
names(depth2016)
head(depth2015)
head(depth2016)

depth2015 <- depth2015 %>%
  select(Species:Velocity_0.6_ms) %>%
  rename(Count = Number)

depth2016 <- depth2016 %>%
  select(Species:Velocity_0.6_ms) 

depth <- rbind(depth2016, depth2015)

write.csv(depth, "output_data/bio/00_Wulff_ALL_Chub_Mos_depth_abundance.csv")

depth_chub <- depth %>% filter( Species == "Arroyo Chub") %>%
  rename(Abundance = Count, Depth = Depth_cm)

dim(depth_chub) ## 253
head(depth_chub)

write.csv(depth_chub, "output_data/bio/00_Wulff_Chub_depth_velocity_abundance.csv")

## make curve
all_depth <- depth_chub

depth_freq <- all_depth %>% 
  uncount(Abundance)
hist(depth_freq$Depth)
mean(depth_freq$Depth) ## 45.91012
dim(depth_freq) ## 1216
head(depth_freq)

## histogram with normal curve
x <-depth_freq$Depth
h<-hist(x, breaks=10, col="red", xlab="Depth (cm)",
        main="Arroyo Chub/Depth Histogram with Normal Curve")
xfit<-seq(min(x),max(x),length=130)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)
# plot(density(depth_freq$Depth))


## probability curve - histogram scaled and centered depth, then transformed back to raw depth


depth_freq$Scaled_Depth <-scale(depth_freq$Depth, scale=T, center=T)
scaled_x <- depth_freq$Scaled_Depth
h <- hist(scaled_x, plot=F)
xfit<-seq(min(scaled_x),max(scaled_x),length=120)
yfit<-dnorm(xfit,mean=mean(scaled_x),sd=sd(scaled_x))
## x axis with raw depth values
xfit_r <- seq(min(depth_freq$Depth), max(depth_freq$Depth), length=120)

png("figures/bio/Chub_depth_curve.png", width = 500, height = 600)

## plot curve with raw depth axis
plot(xfit_r, yfit, axes=FALSE, xlab='', ylab='', type='l', col='', main = "" )
axis(1, at=pretty(xfit_r))
par(new=TRUE)
#plot the line with no axes or labels
plot(xfit, yfit, axes=FALSE, xlab='Depth (cm)', ylab='Probability', type='l', col='red', main = "Arroyo Chub/Depth: Probability curve" )
#add these now with axis
par(new=TRUE)
axis(2, at=pretty(range(yfit)))

dev.off()

min(depth_freq$Depth) ## 0


# linear model exploration ------------------------------------------------

chub <- read.csv("output_data/bio/00_Wulff_Chub_depth_velocity_abundance.csv")

chubLM <- lm(Abundance~Depth, data = chub)
summary(chubLM)
plot(chub$Depth, chub$Abundance)

ggplot(data = chub, mapping = aes(x = Depth, y = Abundance))+
  geom_point(size = 4)+
  geom_smooth(method = "lm")+
  labs(y = "Abundance", x = "Depth (cm)")+
  theme_classic()+
  theme(axis.text = element_text(size = 20), axis.title = element_text(size = 20))

chubQLM <- lm(Abundance~poly(Depth,2), data = chub)
summary(chubQLM)


