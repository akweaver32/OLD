#BASIC SPATIAL PLOTTING

#libraries
packages_maps <- c("sp", "rgdal", "ggplot2", "plyr", "tidyverse", "rgeos",  "raster", "sf", "maps", "mapdata")
lapply(packages_maps, library, character.only = TRUE)

#creating test df for lat long
lat_long <- as_tibble(cocci_limited[3:4])

#read in California shapefile using ST
CA_aoi <- st_read("shapefiles/ca-state-boundary/CA_State_TIGER2016.shp")
#counties this is what you are using rn
counties_sf <- st_read("shapefiles/CA_Counties/CA_Counties_TIGER2016.shp")
counties_sf <- st_transform(counties_sf,crs = 3488) 
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

#read in california spatial data
States <- raster::getData("GADM", country = "United States", level = 1)
CA <- States[States$NAME_1 == "California",]
CA_extent <- extent(CA)


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

#plotting with sf objects, works + added buffers of 26K based on avg USA commute time
ggplot() +
  geom_sf(counties_sf, mapping = aes(geometry = geometry)) +
  geom_sf(buffer_26k, mapping = aes(geometry = geometry), color = "blue") +
  geom_sf(ll_sf, mapping = aes(geometry = geometry), color = "red", size = 0.1) +
  coord_sf(crs = 3488)

ggsave("buffers.pdf")

ggplot() + #10k buffers
  geom_sf(counties_sf, mapping = aes(geometry = geometry)) +
  geom_sf(buffer_10k, mapping = aes(geometry = geometry), color = "blue") +
  geom_sf(ll_sf, mapping = aes(geometry = geometry), color = "red", size = 0.1) +
  coord_sf(crs = 3488)

ggsave("buffers_10k.png")

ggplot() + #10k buffers
  geom_sf(counties_sf, mapping = aes(geometry = geometry)) +
  geom_sf(dust_cases, mapping = aes(geometry = geometry), color = "blue") +
  geom_sf(dust_points, mapping = aes(geometry = geometry), color = "red", size = 0.1) +
  coord_sf(crs = 3488)

ggsave("dust_buffers_10k.png")

##############################################################################################################
#create plots subtracting cases where dust data not available



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

#debug function, doesn't work, might be able to use something in the tigris package
cocci_census <- ll_to_census(cocci_limited, CA_census, "NAME")

##############################################################################################################


