## flow recs figures

library(tidyr)
library(tidyverse)
library(ggplot2)

## save in directory
getwd()
out.dir <- "/Users/katieirving/Library/CloudStorage/OneDrive-SCCWRP/Documents - Katie’s MacBook Pro/git/LARiver_Sp_Model/figures/results"

## upload data

data <- read.csv("synthesis/willow_flow_recs_example.csv")
data

### growth
data_growth <- data %>%
  filter(LifeStage == "Growth") %>%
  pivot_longer(Medium:High, names_to = "ProbabilityThreshold", values_to = "Flow")
str(data_growth)
data_growth$Node <- as.factor(data_growth$Node)
data_growth

level_order <- c('LA20_2', 'LA14', 'GLEN', 'LA11', 'F57C', '11101250', 'F37B_Low') #this vector might be useful for other plots/analyses


p1 <- ggplot(data_growth, aes(x = factor(Node, level = level_order), y=Flow)) +
  geom_line(aes( group = ProbabilityThreshold, lty = ProbabilityThreshold)) +
  scale_linetype_manual(values= c("dotted", "solid"),
                        breaks=c("Medium", "High"))+
  # facet_wrap(~variable, scales="free_x", nrow=3, labeller=labeller(variable = labels)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.text.x = element_text(angle = 45, hjust=1)) +
  labs(title = "Flow Recommendations (Willow Growth)",
       y = "Flow (cfs)",
       x = "Node") #+ theme_bw(base_size = 15)

p1
file.name1 <- paste0(out.dir, "willow_growth_flo_recs.jpg")
ggsave(p1, filename=file.name1, dpi=300, height=5, width=6)

### adult

data_adult <- data %>%
  filter(LifeStage == "Adult") %>%
  pivot_longer(Medium:High, names_to = "ProbabilityThreshold", values_to = "Flow")
str(data_adult)
data_adult
data_growth$Node <- as.factor(data_growth$Node)


level_order <- c('LA20_2', 'LA14', 'GLEN', 'LA11', 'F57C', '11101250', 'F37B_Low') #this vector might be useful for other plots/analyses


p2 <- ggplot(data_adult, aes(x = factor(Node, level = level_order), y=Flow)) +
  geom_line(aes( group = ProbabilityThreshold, lty = ProbabilityThreshold)) +
  scale_linetype_manual(values= c("dotted", "solid"),
                        breaks=c("Medium", "High"))+
  # facet_wrap(~variable, scales="free_x", nrow=3, labeller=labeller(variable = labels)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.text.x = element_text(angle = 45, hjust=1),  legend.position = "none") +
  labs(title = "Flow Recommendations (Willow Adult)",
       y = "Flow (cfs)",
       x = "Node") #+ theme_bw(base_size = 15)



file.name2 <- paste0(out.dir, "willow_adult_flo_recs.jpg")
ggsave(p2, filename=file.name2, dpi=300, height=5, width=6)
