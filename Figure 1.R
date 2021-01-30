######################################################################################
### Benny Rice ### 2021 Jan 28 ###
### Figure 1 for Baker et al review on global connectivity and infectious diseases ###
######################################################################################

######################################################################################
### Figure 1A: Map with historical epidemics ###
######################################################################################

### 1: Land travel: Antonine, Cyprian, and Justinian plagues in 2nd, 3rd, and 6th centuries: Mediterranean
###    Late antiquity: 4th-6th centuries; Classical antiquity: 8th century BC - 6th century AD
### 2: Maritime travel: Columbus contact: Europe - Americas
### 3: Maritime travel: Trans-atlantic slave trade: Africa - Americas
### 4: Air travel: SARS: Asia - Global

### Mapping based off tutorial: https://www.r-graph-gallery.com/how-to-draw-connecting-routes-on-map-with-r-and-great-circles.html

library(tidyverse)
library(maps)

map.world <- map_data('world')

ggplot(map.world, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = group))

# remove antartica, remove background, make nice color map, country boundaries slight, 
# color coded arrows from manually entered coordinates, follow hartl map style, color legend: air travel, overland, maritime
# arrows: overland from eastern med to western med; africa to americas per hartl; europe to americas;
# SARS arrows: trickier; link stepwise across eurasia to get to americas


######################################################################################
### Figure 1B: Post-1970s rapid growth in travel, trade, urbanization
######################################################################################

# 'more frequent transport of people and goods between larger, denser population centers'

#Grab, clean worldbank data
#Split by region? 3 rows x 2 columns
# East Asia & Pacific; Europe & Central Asia; Latin America & Caribbean; Middle East & North Africa; Sub-Saharan Africa
# Needs US and Canada?
# Check for the 3 letter codes to grab these regions






