#BASIC SPATIAL PLOTTING

#install packges
install.packages("raster", "rgdal", "sp")
install.packages("rgdal")
install.packages("maps")

#libraries
library("sf") 
library("ggplot2")

#read in California shapefile using ST
CA_aoi <- st_read("shapefiles/ca-state-boundary/CA_State_TIGER2016.shp")
CA_counties_aoi <- st_read("shapefiles/CA_Counties/CA_Counties_TIGER2016.shp")
#read in using rgdal
CA_aoi <- readOGR(dsn = path.expand("shapefiles/ca-state-boundary"), layer = "CA_State_TIGER2016")
CA_counties_aoi <- st_read("shapefiles/CA_Counties/CA_Counties_TIGER2016.shp") #fix this later

#going through metadata
class(CA_aoi)
crs(CA_aoi)
CA_aoi@data

#plot CA for sf
ggplot() +
  geom_sf(data = CA_aoi, size = 3, color = "black", fill = "cyan1") +
  coord_sf()

#plot CA counties (not sure if this plots)
ggplot() +
  geom_sf(data = CA_counties_aoi, size = 3, color = "black", fill = "cyan1") +
  coord_sf()

#playing with maps package
map('county', 'california', fill = T, col = palette())

