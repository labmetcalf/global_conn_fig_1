######################################################################################
### Benny Rice ### 2021 Jan 28 #######################################################
### Figure 1 for Baker et al review on global connectivity and infectious diseases ###
######################################################################################

library(tidyverse)
library(here)
library(reshape2)
library(gridExtra)
library(scales)
library(directlabels)

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
  geom_polygon(fill="#efebe9", colour = "white", size = 0.2) +
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
# 2. SP.URB.TOTL Urban population (https://data.worldbank.org/indicator/SP.URB.TOTL)
df.urb <- read.csv(here("API_SP.URB.TOTL_DS2_en_csv_v2_1929299.csv"), skip = 4)
# 3. NE.EXP.GNFS.CD Exports of goods and services (current US$) (https://data.worldbank.org/indicator/NE.EXP.GNFS.CD)
df.exp <- read.csv(here("API_NE.EXP.GNFS.CD_DS2_en_csv_v2_1928255.csv"), skip = 4)



### Cleaning World Bank data, keeping data for regions: WLD, EAS, ECS, MEA, SSF, LCN, NAC

# WLD World

# EAS East Asia & Pacific
# ECS Europe & Central Asia
# MEA Middle East & North Africa 
# SSF Sub-Saharan Africa
# LCN Latin America & Caribbean
# NAC North America
df.air <- df.air %>% select(-c(Indicator.Name, X)) %>% 
  filter(Country.Code %in% c("WLD", "EAS", "ECS", "MEA", "SSF", "LCN", "NAC"))
new.col.names <- c("region", "code", "indicator", 1960:2020)
names(df.air) <- new.col.names
df.air <- df.air %>% melt(id = c("region", "code", "indicator"))

df.urb <- df.urb %>% select(-c(Indicator.Name, X)) %>% 
  filter(Country.Code %in% c("WLD", "EAS", "ECS", "MEA", "SSF", "LCN", "NAC"))
names(df.urb) <- new.col.names
df.urb <- df.urb %>% melt(id = c("region", "code", "indicator"))

df.exp <- df.exp %>% select(-c(Indicator.Name, X)) %>% 
  filter(Country.Code %in% c("WLD", "EAS", "ECS", "MEA", "SSF", "LCN", "NAC"))
names(df.exp) <- new.col.names
df.exp <- df.exp %>% melt(id = c("region", "code", "indicator"))

### Binding 3 variables together and prepping for plotting
df3 <- rbind(df.air, df.urb, df.exp) %>% 
  # Renaming columns
  rename(year = variable) %>% 
  # Converting year into a numerical value
  mutate(year = as.numeric(as.character(year))) %>%
  # Deleting data from before 1970 and for 2020 (incomplete data)
  filter(year > 1969) %>% filter(year != 2020) %>%
  #Deleting rows without data
  drop_na() %>%
  # Calculating yearly values as a % increase from 1970 or first year with data
  group_by(indicator, region) %>% mutate(perc_inc = value/min(value)*100-100) %>% ungroup()
  
### Plotting

# Color palette
region_color_pal <- c("#440154FF", "#ab47bc", "#2A788EFF", "#616161", "#7AD151FF", "#ffab00")


### Airtravel ### ### ### ### ### 
p1A <- df3 %>% filter(indicator == "IS.AIR.PSGR") %>% filter(code == "WLD") %>%
  mutate(value = value/1e9) %>%
  ggplot(aes(x = year, y = value)) +
  geom_line(colour = "#a1887f", size = 1) +
  ylab("Air travel passengers\n(billions)") +
  scale_x_continuous(limits = c(1970, 2025)) +
  geom_dl(aes(label = code), colour = "#a1887f", method = list(dl.trans(x = x + .1), "last.points", cex = 0.7)) +
  theme(panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none")

p1B <- df3 %>% filter(indicator == "IS.AIR.PSGR") %>% filter(code != "WLD") %>%
  mutate(value = value/1e9) %>%
  ggplot(aes(x = year, y = value)) +
  geom_line(aes(group = region, colour = region), size = 1) +
  scale_x_continuous(limits = c(1970, 2025)) +
  scale_color_manual(values = region_color_pal) +
  geom_dl(aes(label = code, colour = region), method = list(dl.trans(x = x + .1), 'last.bumpup', "last.points", cex = 0.7)) +
  theme(panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        legend.position = "none")

### Exports ### ### ### ### ### 

p2A <- df3 %>% filter(indicator == "NE.EXP.GNFS.CD") %>% filter(code == "WLD") %>%
  mutate(value = value/1e12) %>%
  ggplot(aes(x = year, y = value)) +
  geom_line(colour = "#a1887f", size = 1) +
  ylab("Exports of goods and services\n(trillion current US$)") +
  scale_x_continuous(limits = c(1970, 2025)) +
  geom_dl(aes(label = code), colour = "#a1887f", method = list(dl.trans(x = x + .1), "last.points", cex = 0.7)) +
  theme(panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none")

p2B <- df3 %>% filter(indicator == "NE.EXP.GNFS.CD") %>% filter(code != "WLD") %>%
  mutate(value = value/1e12) %>%
  ggplot(aes(x = year, y = value)) +
  geom_line(aes(group = region, colour = region), size = 1) +
  scale_x_continuous(limits = c(1970, 2025)) +
  scale_color_manual(values = region_color_pal) +
  geom_dl(aes(label = code, colour = region), method = list(dl.trans(x = x + .1), 'last.bumpup', "last.points", cex = 0.7)) +
  theme(panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        legend.position = "none")

### Urbanization ### ### ### ### ### 

p3A <- df3 %>% filter(indicator == "SP.URB.TOTL") %>% filter(code == "WLD") %>%
  mutate(value = value/1e9) %>%
  ggplot(aes(x = year, y = value)) +
  geom_line(colour = "#a1887f", size = 1) +
  ylab("Urban population\n(billions)") +
  scale_x_continuous(limits = c(1970, 2025)) +
  geom_dl(aes(label = code), colour = "#a1887f", method = list(dl.trans(x = x + .1), "last.points", cex = 0.7)) +
  theme(panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none")

p3B <- df3 %>% filter(indicator == "SP.URB.TOTL") %>% filter(code != "WLD") %>%
  mutate(value = value/1e9) %>%
  ggplot(aes(x = year, y = value)) +
  geom_line(aes(group = region, colour = region), size = 1) +
  scale_x_continuous(limits = c(1970, 2025)) +
  scale_color_manual(values = region_color_pal) +
  geom_dl(aes(label = code, colour = region), method = list(dl.trans(x = x + .1), 'last.bumpup', "last.points", cex = 0.7)) +
  theme(panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        legend.position = "none")


grid.arrange(p1A, p1B, p2A, p2B, p3A, p3B, nrow = 3)








