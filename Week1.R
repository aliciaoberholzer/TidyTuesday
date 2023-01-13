setwd("~/Documents/R/TidyTuesday/Week 1")

#load necessary packages
library(WDI)
library(ggplot2)
library(tidyverse)
library(ggthemes)
library(dplyr)
library(rnaturalearth)
library(sf)

#get country data from Natural Earth R package and return as sf object
world <- ne_countries(scale = "large", returnclass = 'sf')

#get clean cooking access data from World Bank R Package
WDIsearch('fuel')
dt = WDI(indicator = 'EG.CFT.ACCS.ZS', country = 'all', start = '2020', end = '2020')

#clean data for merging with country data
#remove World Bank regions so only countries are left
dt2 <- dt[-c(1:49),]

#merge clean cooking access data with country SF object
map <- merge(world, dt2, by.x = "iso_a3", by.y = "iso3c", all.x = 'TRUE')

#build the plot
cookmap <- ggplot() +
  geom_sf(
    data = map, 
    aes(fill = EG.CFT.ACCS.ZS, lwd = ""), 
    color = NA) +
  labs(
    title = "Perecentage of the population with access to clean cooking fuels and technologies (2020)",
    subtitle = "Data source: World Bank",
    caption = "#TidyTuesday Week 1, 2023 | @aliciaoberhol") +
  scale_fill_distiller(
    palette = 'RdPu', "% of population with\n access to clean cooking", 
    labels=c("0%", "25%", "50%", "75%", "100%"), 
    na.value = "#ECECEC",
    direction = -1) +
  scale_color_manual(                   #hack for displaying countries that have no data 
    labels = "NA",                      #this is also why I assigned a column of empty strings to line width (lwd) in geom_sf
    values=NA) +              
  guides(lwd=guide_legend("No data", override.aes=list(colour="#ECECEC"))) +
  coord_sf(crs = "+proj=robin +lon_0=0w") #World Robinson projection 

#format the plot
cookmap + 
  theme_void() +
theme(
    legend.justification = c(0,1),
    legend.position = c(0.04,0.7),
    legend.spacing.y = unit(0.5, 'cm'),
    legend.key.size = unit(1.2, 'cm'),
    legend.title = element_text(size=12), 
    legend.text = element_text(size=10),
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    plot.caption = element_text(size = 12, hjust = 0.5),
    text = element_text(family= "Courier"))

#save the plot & specify size & resolution
ggsave("test.png", bg = "#fbf7f5", width = 297, height = 175, units = 'mm', dpi = 300)