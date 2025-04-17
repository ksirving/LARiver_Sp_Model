## typha models and figures
## coded by Jenny Rogers

library(tidyverse)
library(dplyr)
library(ggplot2)


# Velocity ----------------------------------------------------------------


load("typha_velocity.RData")  


ggplot(data = velocity, mapping = aes(x = vel_m_s, y = occurrence))+
  geom_point(alpha = 0.2, size = 3)+
  labs(x = "Velocity (m/s)", y = "Occurrence")+
  geom_smooth(method = "glm", method.args = list(family = "binomial"))+
  theme(text = element_text(size=20), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=.5))


summary(vel_ptch_mdl <- glm(occurrence ~ vel_m_s, data = velocity, family = "binomial"))
confint(vel_ptch_mdl)
save(vel_ptch_mdl, file = "vel_ptch_mdl.rda")


unique(velocity$source[!is.na(velocity$occurrence)])


# Depth -------------------------------------------------------------------


load("typha_depth.RData") ## depth
# write.csv(depth, "output_data/typha_depth.csv")
# depth <- read.csv("output_data/typha_depth.csv")
# save(depth, file="typha_depth.RData")
head(depth)

#patch occurrence
ggplot(data = depth, mapping = aes(x = depth_cm, y = occurrence))+
  geom_point(alpha = 0.2, size = 3)+
  labs(x = "Depth (cm)", y = "Occurrence")+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2))+
  theme(text = element_text(size=20), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=.5))





summary(dep_ptch_mdl <- lm(occurrence ~ depth_cm + I(depth_cm^2), data = depth))
save(dep_ptch_mdl, file = "dep_ptch_mdl.rda")


  # library(ggplot2)
  # ggplot(df, aes(x = stage, y = Repeatability, shape = Behaviour, colour=Age, linetype=Age)) + 
  #   geom_point() + 
  #   scale_colour_manual(values = c("#D55E00", "#0072B2", "black", "black", "#CC79A7", "black"), name = "Study", breaks=c("A","A2", "PE", "PE"=="PE2", "PW", "PE"=="PW1"),
  #                       labels=c(
  #                         (expression(atop("Taylor et al. 2012", italic("(366 adults)")))),
  #                         (expression(atop("Boon et al. 2008", italic("(71 adult females)")))), 
  #                         (expression(atop("This study", italic("(102 juveniles)")))),
  #                         "thisstudy",
  #                         (expression(atop("Kelley et al. 2015", italic("(16 yearlings)")))),
  #                         "thisstudy"))+
  #   theme(legend.key.size = unit(2.5, 'lines'))

#germination percent

# png(filename="figures/typha_germ_depth_2spp.png")

ggplot(data = depth[!is.na(depth$germination_perc),], mapping = aes(x = depth_cm, y = germination_perc, color = Species))+
  geom_point()+
  labs(x = "Depth (cm)", y = "Germination (%)")+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2))+
  theme(text = element_text(size=20), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=.5))

# dev.off()

ggplot(data = depth[!is.na(depth$germination_perc),], mapping = aes(x = depth_cm, y = germination_perc))+
  geom_point()+
  labs(x = "Depth (cm)", y = "Germination (%)")+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2))+
  theme(text = element_text(size=20), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=.5))




summary(dep_germ_mdl <- lm(germination_perc~depth_cm + I(depth_cm^2) + species, data = depth))
save(dep_germ_mdl, file = "dep_germ_mdl.rda")



 #seedling survial
  ggplot(data = depth[!is.na(depth$seedling_survial_perc),], mapping = aes(x = depth_cm, y = seedling_survial_perc, color = Species))+
  geom_point()+
  labs(x = "Depth (cm)", y = "Seedling survival (%)")+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2))+
  theme(text = element_text(size=20), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=.5))

  
ggplot(data = depth[!is.na(depth$seedling_survial_perc),], mapping = aes(x = depth_cm, y = seedling_survial_perc))+
  geom_point()+
  labs(x = "Depth (cm)", y = "Seedling survival (%)")+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2))+
  theme(text = element_text(size=20), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=.5))



summary(dep_sdlng_mdl <- lm(seedling_survial_perc~depth_cm + I(depth_cm^2) + species, data = depth))
save(dep_sdlng_mdl, file = "dep_sdlng_mdl.rda")



unique(depth$source[!is.na(depth$occurrence)])
unique(depth$source[!is.na(depth$germination_perc)])
unique(depth$source[!is.na(depth$seedling_survial_perc)])


# Temperature -------------------------------------------------------------



load("typha_temp.RData")

ggplot(data = temp, aes(x = temp_midRng_c, y = germination_perc))+
  geom_jitter(alpha = 0.2, size = 3)+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2))+
  theme(text = element_text(size=20), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=.5))+
  labs(x = "Temperature Mid-Range (C)", y = "Germination (%)")


ggplot(data = temp, aes(x = high_temperature_c, y = germination_perc))+
  geom_jitter(alpha = 0.2, size = 3)+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2))+
  theme(text = element_text(size=20), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=.5))+
  labs(x = "High Temperature (C)", y = "Germination (%)")

summary(tmp_germ_mdl <- (lm(germination_perc ~ temperature_range_c + high_temperature_c + I(high_temperature_c^2), data = temp)))
save(tmp_germ_mdl, file = "tmp_germ_mdl.rda")


ggplot(data = temp, aes(x = temperature_range_c, y = germination_perc))+
  geom_jitter(alpha = 0.2, size = 3)+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2))+
  theme(text = element_text(size=20), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=.5))+
  labs(x = "Temperature Range (C)", y = "Germination (%)")

unique(temp$source[!is.na(temp$germination_perc)])
