#drop non dust data cases

#load packages for savio
#libraries
packages_dust <- c("sp", "rgdal", "tidyverse", "rgeos",  "raster", "sf", "maps", "mapdata", "maptools", "exactextractr", "stars", "velox", "gdalUtilities")
lapply(packages_dust, library, character.only = TRUE)

#read in dust mask raster
dust_mask <- raster("shapefiles/NLCD_2016_CA_clipped_non_ag.tif")
dust_mask_ag <- raster("shapefiles/NLCD_2016_CA_clipped_ag.tif")

#merge ag and non ag tif into composite raster
dust_merge <- c("shapefiles/NLCD_2016_CA_clipped_non_ag.tif", "shapefiles/NLCD_2016_CA_clipped_ag.tif")
gdalbuildvrt(gdalfile = dust_merge, 
             output.vrt = "dem.vrt")
gdal_translate(src_dataset = "dem.vrt", 
               dst_dataset = "dem.tif")

#combined ag and non ag files
dust_mask_CA <- raster("dem.tif")

#read in buffers
buffers_4326_all <- readRDS("objects for savio/buffers_4326_all.rds") #crs = +proj=longlat +datum=WGS84 +no_defs 

#change this for different tif (ag vs not ag)
ex_5k <- exact_extract(dust_mask_CA, buffers_4326_all$`5k`, 'count')
ex_10k <- exact_extract(dust_mask_CA, buffers_4326_all$`10k`, 'count') #done up to this line
ex_26k <- exact_extract(dust_mask_CA, buffers_4326_all$`26k`, 'count')

#create approx percent coverage and create categories for coverage
drop_df <- as_tibble(ex_5k, ex_10k, ex_26k) %>%
  mutate(coverage_5k = ex_5k/max(ex_5k),
         coverage_10k = ex_10k/max(ex_10k),
         coverage_26k = ex_26k/max(ex_26k)) %>%
  mutate(category_5k = cut(coverage_5k, breaks=c(-Inf,0.25, 0.5, 0.75, Inf), 
                           labels=c("low","middle_l","middle_h", "high")),
         category_10k = cut(coverage_10k, breaks=c(-Inf,0.25, 0.5, 0.75, Inf), 
                            labels=c("low","middle_l","middle_h", "high")),
         category_26k = cut(coverage_26k, breaks=c(-Inf,0.25, 0.5, 0.75, Inf), 
                            labels=c("low","middle_l","middle_h", "high")))

#create dust cases with buffer, coverage, and drop categories  
names(ll_sf) <- "points"

dust_cases <- cbind(cases_buffer_all, drop_df, ll_sf) %>%
  mutate(year = lubridate::year(date))

#go to basic_figures.R to create dust map with cases that will need to be dropped (ag dropped and not dropped)
  


  
  
  
  

