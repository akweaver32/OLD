#BASIC SPATIAL PLOTTING

#libraries
packages <- c("sp", "rgdal", "ggplot2", "plyr", "tidyverse", "rgeos",  "raster", "sf", "maps", "mapdata")
lapply(packages, library, character.only = TRUE)

#creating test df for lat long
lat_long <- as_tibble(cocci_limited[3:4])

ll_sf <- st_as_sf(lat_long, coords = c("lat", "long"), crs = 4326)
mapview(ll_sf) #interactive plot

#read in California shapefile using ST
CA_aoi <- st_read("shapefiles/ca-state-boundary/CA_State_TIGER2016.shp")
#counties this is what you are using rn
counties_df <- st_read("CA_Counties/CA_Counties_TIGER2016.shp")
counties_df <- st_transform(counties_df,crs = 3488) 
#read in izzy's hex grid
CA_hex <- st_read("shapefiles/CA_hex_grid/CA_hex_grid.shp") #unknown CRS
#read in CA census tract
CA_census <- st_read("shapefiles/ca_census_tract/tl_2019_06_tract.shp") #CRS = NAD83

#save as rds object
saveRDS(counties_df, "ca_counties.rds")

#load california county map
california <- map_data("state") %>%
  subset(region == "california")
ca_counties <- map_data("county") %>%
  subset(region == "california")

##############################################################################################################

#plot CA counties
ca_base <- ggplot(data = california, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "gray")

#plot with cocci cases
all_cocci_cases_county_map <- ca_base + 
  geom_polygon(data = ca_counties, aes(x = long, y = lat, group = group), fill = NA, color = "white") +
  geom_polygon(color = "black", fill = NA) + # get the state border back on top
  geom_point(lat_long, mapping = aes(x = long, y = lat), color = "red", size = 0.1, inherit.aes = FALSE) #add cocci cases

all_cocci_cases_county_map

##############################################################################################################

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
  rename("county" = "county_names[ii]") %>%
  mutate_at(vars(county, incarcerated), factor)

##############################################################################################################

#write a function to get census tract for coordinates
ll_to_census <- function(ll_df, map, id_col = "name"){
  ## Convert points data.frame to an sf POINTS object
  pts <- st_as_sf(ll_df, coords = c("long", "lat"), crs = 4269)
  
  ## Transform spatial data to some planar coordinate system
  ## (e.g. Web Mercator) as required for geometric operations
  census <- st_transform(map, crs = 3857)
  pts <- st_transform(pts, crs = 3857)
  
  ## Find names of county (if any) intersected by each point
  census_id <- census[[id_col]]
  ii <- as.integer(st_intersects(pts, census))
  census_id[ii]
  return(cbind(ll_df, census_id[ii]))
}

#debug function, doesn't work
cocci_census <- ll_to_census(cocci_limited, CA_census, "NAME")

#try using tigris package, doesn't work
cocci_limited$census_code <- mclapply(cocci_limited[3:4], 1,call_geolocator_latlon(row['lat'], row['long']))

##############################################################################################################



