######################################################################################
### Benny Rice ### 2021 Jan 28 #######################################################
### Figure 1 for Baker et al review on global connectivity and infectious diseases ###
######################################################################################

library(tidyverse)
library(here)
library(reshape2)

######################################################################################
### Figure 1A: Map with historical epidemics ###
######################################################################################

### 1: Overland travel: Antonine, Cyprian, and Justinian plagues in 2nd, 3rd, and 6th centuries: Mediterranean
###    Late antiquity: 4th-6th centuries; Classical antiquity: 8th century BC - 6th century AD
### 2: Maritime travel: Columbus contact: Europe - Americas
### 3: Maritime travel: Trans-atlantic slave trade: Africa - Americas
### 4: Air travel: SARS: Asia - Global

# Refs for classical antiquity pandemics
# https://www.cambridge.org/core/journals/journal-of-economic-history/article/plague-and-lethal-epidemics-in-the-preindustrial-world/1D2D564AD8560ABACAF9D81A65F27CED
# https://www.sciencedirect.com/science/article/pii/S1473309913703232?casa_token=sbNL1Stx6PoAAAAA:coZNre9d3_OrnynZmAmaAiHEVoK_b_yb-LB7G_kJjFvvgNXKdWCFiNTNnUkgoD2hwibNGU7sCA
# https://www.cambridge.org/core/journals/journal-of-roman-archaeology/article/abs/impact-of-the-antonine-plague/B510B697DB444B274465A50381267380
# https://www.cambridge.org/core/journals/journal-of-roman-archaeology/article/abs/pandemics-and-passages-to-late-antiquity-rethinking-the-plague-of-c249270-described-by-cyprian/BF0A142F1864D05FDC0C3439D182B7B0

# Ref for Trans-atlantic slave trade and malaria:
# https://www.annualreviews.org/doi/abs/10.1146/annurev-genet-120215-035211?casa_token=B9GHXaSi0ZIAAAAA:I8lTQkLr6DGzd39CSCylKr-jDMQ8mfnS2jrv6CwtV38mre5tadEUwxpFsGeVfYp8iOv4qGRV1bHX

# Generating a map of the world, removing Antarctica (author is unaware of any epidemics in Antarctica)
map.world <- map_data('world') %>% filter(region != "Antarctica")

# Plotting map
ggplot(map.world, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill="#d7ccc8", colour = "white", size = 0.04) +
  coord_fixed(1.3) +
  theme(panel.background = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

# Adding arrows for movements in powerpoint (could do in R but would take more time)




######################################################################################
### Figure 1B: Post-1970s rapid growth in travel, trade, urbanization
######################################################################################

# 'more frequent transport of people and goods between larger, denser population centers'

### Reading in World Bank data

# 1. IS.AIR.PSGR Air transport, passengers carried (https://data.worldbank.org/indicator/IS.AIR.PSGR)
df.air <- read.csv(here("API_IS.AIR.PSGR_DS2_en_csv_v2_1993658.csv"), skip = 4)
# 2 


### Cleaning World Bank data, keeping data for regions only: WLD, EAS, ECS, MEA, SSF, LCN, NAC

df.air <- df.air %>% select(-c(Indicator.Name, X)) %>% 
  filter(Country.Code %in% c("WLD", "EAS", "ECS", "MEA", "SSF", "LCN", "NAC"))
air.col.names <- c("Country", "Code", "Indicator", 1960:2020)
names(df.air) <- air.col.names
df.air <- df.air %>% melt(id = c("Country", "Code", "Indicator"))




#Grab, clean worldbank data
# Show world above then split by region? 3 rows x 2 columns
# WLD World

# EAS East Asia & Pacific
# ECS Europe & Central Asia
# MEA Middle East & North Africa 
# SSF Sub-Saharan Africa
# LCN Latin America & Caribbean
# NAC North America

# Plot as % increase since 1970?







