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
colnames(lat_long) <- c("long", "lat")
ll_sf <- st_as_sf(lat_long, coords = c("lat", "long"), crs = 4326)
mapview(ll_sf) #interactive plot
View(lat_long)

#write a function to convert lat long to ca counties
ll_to_county <- function(ll_df, 
                         map, 
                         name_col = "name") {
  ## Convert points data.frame to an sf POINTS object
  pts <- st_as_sf(ll_df, coords = c("long", "lat"), crs = 4326)
  
  ## Transform spatial data to some planar coordinate system
  ## (e.g. Web Mercator) as required for geometric operations
  county <- st_transform(map, crs = 3857)
  pts <- st_transform(pts, crs = 3857)
  
  ## Find names of county (if any) intersected by each point
  county_names <- county[[name_col]]
  ii <- as.integer(st_intersects(pts, county))
  county_names[ii]
  return(cbind(ll_df, county_names[ii]))
}

#debug function, works
cocci_counties <- ll_to_county(cocci_limited, CA_counties_aoi, "NAME") %>%   #creates a df with counties and coords
  rename("county" = "county_names[ii]")
  
ll_to_county(tiny_ll, CA_counties_aoi, "NAME")
tiny_ll <- lat_long[125,]


#create a df with coords and county
ll_county <- data.frame(lat_long, cocci_counties) %>%
  mutate(cocci_counties = as.factor(cocci_counties)) %>%
  rename("lat" = "Y", "long" = "X")
str(ll_county)

#plotting with another method
map("county", "california", fill = T, add = T)
points(lat_long$X, lat_long$Y, pch = 4, col = "red", cex = 0.8)

cocci_limited[3:4]
