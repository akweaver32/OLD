#prism extraction of climate data

#libraries for savio
packages_prism <- c("prism", "raster", "velox", "exactextractr", "tidyverse")
lapply(packages_prism, library, character.only = TRUE)

#set prism download folder
#prism_set_dl_dir("environmental data")
options(prism.path = "/global/scratch/users/amanda_weaver/environmental data/temp")

#test get daily vals
get_prism_dailys("tmean", minDate = "2000-01-01", maxDate = "2017-12-31", keepZip = FALSE)

data_tmean <- as.data.frame(prism_archive_ls()) #creates a list of all the rasters in the prism.path
index_tmean <- 1:nrow(data_tmean)
RS_tmean <- pd_stack(data_tmean[index_tmean,]) #creates a raster stack of all rasters in 'data' from the prism.path folder

# import shapefiles for each buffer size
buffer_10k <- readRDS("buffer_10k.rds") #transfer this file via GUI into savio (and other files!)

# project both files into longlat 
buffer_10k <- st_transform(buffer_10k, "+proj=longlat +datum=NAD83 +no_defs")

# check that extent formats match
ext <- extent(RS_ppt)

#check that crs matches 
#crs(RS_test_ppt)
#crs(all_buffers)

#create the velox raster
vx_ppt <- velox(RS_ppt, extent=ext, crs=crs(all_buffers))


