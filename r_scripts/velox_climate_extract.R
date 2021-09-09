#Extracting future climate Velox

library(raster)
library(dplyr)
library(rgdal)
library(sp)
library(maptools)
library(velox)
library(foreach)
library(doParallel)

#Registering cores for parallelization
numCores <- detectCores()
numCores
registerDoParallel(numCores)

#Extracting precipitation
future_ppt <- raster::stack('CanESM2_RCP45_ppt.tif')
future_ppt
future_ppt <- raster::dropLayer(future_ppt, c(1:5479, 12420:34698)) #selecting days of interest
future_ppt
grid <- readOGR('spatial_units/basin_grid.shp')
grid <- spTransform(grid, crs(future_ppt))
all_ppt <- foreach (i = 1:6940) %dopar% { #looping through each raster and parallelizing
  temp <- future_ppt[[i]] #subsetting each raster from stack
  crop <- crop(temp, grid) #cropping to grid extent
  mask <- mask(crop, grid) #masking to grid
  vlr <- velox(mask) #converting to velox object
  dat <- as.data.frame(vlr$extract(grid, fun = base::mean)) #extracting ppt values for each hex and averaging
  names(dat)[1] <- 'ppt' #Changing name of ppt column so that each raster binds together
  dat %>% mutate(FID = grid$FID) #adding Hexagon ID number
}
write.csv(all_ppt, 'CanESM2_RCP45_ppt_hex.csv') #writing out file

#Extracting tmin
future_tmin <- raster::stack('CanESM2_RCP45_tmin.tif')
future_tmin
future_tmin <- raster::dropLayer(future_tmin, c(1:5479, 12420:34698)) #selecting days of interest
future_tmin
all_tmin <- foreach (i = 1:6940, .combine=rbind) %dopar% {
  temp <- future_tmin[[i]]
  crop <- crop(temp, grid)
  mask <- mask(crop, grid)
  vlr <- velox(mask)
  dat <- as.data.frame(vlr$extract(grid, fun = base::mean))
  names(dat)[1] <- 'tmin'
  dat %>% mutate(FID = grid$FID)
}
write.csv(all_tmin, 'CanESM2_RCP45_tmin_hex.csv')

#Extracting tmax
future_tmax <- raster::stack('CanESM2_RCP45_tmax.tif') 
future_tmax
future_tmax <- raster::dropLayer(future_tmax, c(1:5479, 12420:34698)) #selecting days of interest
future_tmax

all_tmax <- foreach (i = 1:6940, .combine=rbind) %dopar% { #looping through each raster and parallelizing
  temp <- future_tmax[[i]]
  crop <- crop(temp, grid)
  mask <- mask(crop, grid)
  vlr <- velox(mask)
  dat <- as.data.frame(vlr$extract(grid, fun = base::mean))
  names(dat)[1] <- 'tmax'
  dat %>% mutate(FID = grid$FID)
}
write.csv(all_tmax, 'CanESM2_RCP45_tmax_hex.csv')

CanESM2_RCP45_data <- cbind(all_ppt, all_tmin)
CanESM2_RCP45_data <- cbind(CanESM2_RCP45_data, all_tmax)

write.csv(CanESM2_RCP45_data, 'CanESM2_RCP45_data.csv')

stopImplicitCluster()

