#load packages for savio
#libraries
packages <- c("sp", "rgdal", "ggplot2", "plyr", "tidyverse", "rgeos",  "raster", "sf", "maps", "mapdata")
lapply(packages, library, character.only = TRUE)

##############################################################################################################

#creating dataset for cc analysis


#project cocci points
ll_df <- st_as_sf(lat_long, coords = c("long", "lat"), crs = 4326) %>%
  st_transform(crs = 3488)

#create buffer
buffer_1km <- st_buffer(ll_df, 1)

class(buffer_1km)
all_cocci_cases_county_map + geom_sf(buffer_1km, aes())

#make sure CA counties also has same CRS
compareCRS(buffer, counties_df)




