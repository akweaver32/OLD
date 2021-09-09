#load packages for savio
#libraries
packages_raster <- c("sp", "rgdal", "tidyverse", "rgeos",  "raster", "sf", "maps", "mapdata", "velox", "foreach", "doParallel", "maptools", "ncdf4", "exactextractr")
lapply(packages_raster, library, character.only = TRUE)

##############################################################################################################
#read in buffer extract object
buffer_extract <- readRDS("buffer_10k.rds") #this is an sf object and the area you are interested in
st_crs(buffer_extract) <- 3488
buffer_extract$GEOID <- paste0(1:nrow(buffer_extract))
buffer_extent <- extent(buffer_extract) #i think this is 10k but not sure, making a new one

#exact extract test
sand <- raster('MosaicRasterAllSand.tif')
sand <- crop(sand, buffer_extent)
crs(sand) <- crs("+proj=aea +lat_0=0 +lon_0=-120 +lat_1=34 +lat_2=40.5 +x_0=0 +y_0=-4000000 +ellps=GRS80 +units=m +no_defs")

vals <- exact_extract(sand, buffer_extract, 'mean', force_df = TRUE)
vals$GEOID <- buffer_extract$GEOID

write.csv(vals, 'percent_sand_10k.csv')