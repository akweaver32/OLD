#install packages
install.packages("sf")

#library
library("sf") 
library("ggplot2")

#read in CA shapefile
CA_aoi <- st_read("ca-state-boundary/CA_State_TIGER2016.shp")
CA_counties_aoi <- st_read("CA_Counties/CA_Counties_TIGER2016.shp")

#plot CA
ggplot() +
  geom_sf(data = CA_aoi, size = 3, color = "black", fill = "cyan1") +
  coord_sf()

#plot CA counties
ggplot() +
  geom_sf(data = CA_counties_aoi, size = 3, color = "black", fill = "cyan1") +
  coord_sf()

