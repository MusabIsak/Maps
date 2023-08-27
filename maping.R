
#title: "Study Area Map"
#author: "Musab Isak"
#date: "2023-08-27"

#Load packages

library(tidyverse)             
library(ggplot2)                
library(dplyr)                 
library(sf)                     
library(rnaturalearth)         
library(rnaturalearthdata)     
library(raster)                
library(viridis)               
library(ggspatial)             


#download shape data
turkey <- st_as_sf(getData("GADM", level = 1, country = "TUR"))
turkey1 <- st_as_sf(getData("GADM", level = 2, country = "TUR"))
trapzon <- st_as_sf(subset(turkey1, NAME_1== "Trabzon"))
giresun <- st_as_sf(subset(turkey1, NAME_1== "Giresun"))
ordu <- st_as_sf(subset(turkey1, NAME_1== "Ordu"))
samsun <- st_as_sf(subset(turkey1, NAME_1== "Samsun"))

# Create a new data frame for the legend
legend_data <- data.frame(
  City = c("Trabzon", "Ordu", "Samsun", "Giresun"),
  Color = c("yellow", "green", "skyblue", "purple")
)

turkey_ggplot <- ggplot() +
  geom_sf(data = turkey, aes(fill = "Turkey")) +
  geom_sf(data = subset(turkey1, NAME_1 %in% c("Trabzon", "Ordu", "Samsun", "Giresun")), aes(fill = NAME_1)) +
  geom_rect(aes(xmin = 35, xmax = 41, ymin = 40, ymax = 41.8), color = "black", fill = NA) +
  scale_fill_manual(values = c("Turkey" = "pink", "Trabzon" = "yellow", "Ordu" = "green", "Samsun" = "skyblue", "Giresun" = "purple"),
                    guide = guide_legend(override.aes = list(fill = "white"))) +
  theme_minimal() +
  theme(plot.background = element_blank()) +
  guides(fill = guide_legend(title = "Cities", label.theme = element_text(color = "black", face = "bold"))) + labs(title = "STUDY AREA") + 

print(turkey_ggplot)




#Extracted Study area Map from Country Map

tr_alt <- getData( "alt", country = "TUR", mask = TRUE)
crop_alt_trab <- crop(tr_alt, extent(study_area))
mask_alt_trab <- mask(crop_alt_trab, study_area)
rasdf2 = as.data.frame(mask_alt_trab, xy = TRUE) %>% drop_na()

tr_alt = ggplot() + geom_tile(aes(x=x, y=y, fill = TUR_msk_alt), data = rasdf2) +
  geom_sf(fill = "transparent", data = study_area) + scale_fill_viridis_b(name = "alt(mm)", direction = -1) +
  labs(x= 'Lon', y= "Lat", title = "", subtitle = "", caption = '') +
  cowplot::theme_cowplot() + theme(panel.grid.major = element_line(color = "black"))
tr_alt

