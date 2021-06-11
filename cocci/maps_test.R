#BASIC SPATIAL PLOTTING

#install packges
install.packages("raster", "rgdal", "sp")
install.packages("rgdal")
install.packages("maps")
install.packages("RgoogleMaps")
install.packages("ggmap")
install.packages("mapview")
install.packages("mapdata")

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
ca_counties <- map('county', 'california', fill = T, col = palette())
class(ca_counties)
crs(ca_counties)

#creating test df for lat long
lat_long <- as_tibble(cocci_location[13:14])
ll_sf <- st_as_sf(lat_long, coords = c("X", "Y"), crs = 4326)
mapview(ll_sf) #interactive plot

#plotting with another method
map("county", "california", fill = T, add = T)
points(lat_long$X, lat_long$Y, pch = 4, col = "red", cex = 0.8)
