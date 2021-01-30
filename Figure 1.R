######################################################################################
### Benny Rice ### 2021 Jan 28 #######################################################
### Figure 1 for Baker et al review on global connectivity and infectious diseases ###
######################################################################################

library(tidyverse)

######################################################################################
### Figure 1A: Map with historical epidemics ###
######################################################################################

### 1: Overland travel: Antonine, Cyprian, and Justinian plagues in 2nd, 3rd, and 6th centuries: Mediterranean
###    Late antiquity: 4th-6th centuries; Classical antiquity: 8th century BC - 6th century AD
### 2: Maritime travel: Columbus contact: Europe - Americas
### 3: Maritime travel: Trans-atlantic slave trade: Africa - Americas
### 4: Air travel: SARS: Asia - Global

### Mapping based off tutorial: https://www.r-graph-gallery.com/how-to-draw-connecting-routes-on-map-with-r-and-great-circles.html

# Generating a map of the world, removing Antarctica (author is unaware of any epidemics in Antarctica)
map.world <- map_data('world') %>% filter(region != "Antarctica")

# Plotting map
ggplot(map.world, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill="#a1887f", colour = "white", size = 0.01) +
  coord_fixed(1.3) +
  theme(panel.background = element_blank())

# 



# remove antartica, remove background, make nice color map, country boundaries slight, 
# color coded arrows from manually entered coordinates, follow hartl map style, color legend: air travel, overland, maritime
# arrows: overland from eastern med to western med; africa to americas per hartl; europe to americas;
# SARS arrows: trickier; link stepwise across eurasia to get to americas

# Refs
# https://www.cambridge.org/core/journals/journal-of-economic-history/article/plague-and-lethal-epidemics-in-the-preindustrial-world/1D2D564AD8560ABACAF9D81A65F27CED
# https://www.sciencedirect.com/science/article/pii/S1473309913703232?casa_token=sbNL1Stx6PoAAAAA:coZNre9d3_OrnynZmAmaAiHEVoK_b_yb-LB7G_kJjFvvgNXKdWCFiNTNnUkgoD2hwibNGU7sCA
# https://www.cambridge.org/core/journals/journal-of-roman-archaeology/article/abs/impact-of-the-antonine-plague/B510B697DB444B274465A50381267380
# https://www.cambridge.org/core/journals/journal-of-roman-archaeology/article/abs/pandemics-and-passages-to-late-antiquity-rethinking-the-plague-of-c249270-described-by-cyprian/BF0A142F1864D05FDC0C3439D182B7B0



######################################################################################
### Figure 1B: Post-1970s rapid growth in travel, trade, urbanization
######################################################################################

# 'more frequent transport of people and goods between larger, denser population centers'

#Grab, clean worldbank data
#Split by region? 3 rows x 2 columns
# East Asia & Pacific; Europe & Central Asia; Latin America & Caribbean; Middle East & North Africa; Sub-Saharan Africa
# Needs US and Canada?
# Check for the 3 letter codes to grab these regions






