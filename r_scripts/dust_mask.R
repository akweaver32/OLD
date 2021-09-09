#drop non dust data cases

#load packages for savio
#libraries
packages_dust <- c("sp", "rgdal", "tidyverse", "rgeos",  "raster", "sf", "maps", "mapdata", "maptools", "exactextractr")
lapply(packages_dust, library, character.only = TRUE)

dust_mask <- raster("NLCD_2016_CA_clipped_non_ag.tif")
crs(dust_mask) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
values(dust_mask)[values(dust_mask) > 0] = 1

buffer_10k <- readRDS("buffer_10k.rds")#crs = +proj=longlat +datum=WGS84 +no_defs 
st_crs(buffer_10k) <- 4326
buffer_10k$GEOID <- paste0(1:nrow(buffer_10k)) #add ID if needed

ex <- extract(dust_mask, buffer_10k, fun=function(x, ...) length((x==1))/length(x), df = TRUE)

saveRDS(ex, "step1.rds")

drop <- cbind.data.frame(ex, buffer_10k$GEOID)

saveRDS(drio, "step2.rds")



