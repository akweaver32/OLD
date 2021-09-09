#wind NetCDFs 
#load packages for savio
#libraries
packages_wind <- c("sp", "rgdal", "tidyverse", "rgeos",  "raster", "sf", "ncdf4", "reshape2", "lubridate", "stars", "ncmeta", "lwgeom")
lapply(packages_wind, library, character.only = TRUE)

#read in multiple netCDF files for wind data
#wind_speed <- read_rds("wind_speed.rds")
filenames = list.files('environmental data/gridMET', pattern='*.nc',full.names=TRUE)

n <- 19
prefix <- "wind_"
suffix <- seq(from = 2000, to = 2018)
obj_names <- paste(prefix, suffix, sep = "")

case_control <- read_rds("case_control.rds") #crs = 3488


#for loop
for (f in 1:length(filenames)) {
  obj_names[[f]] <- read_ncdf(filenames[[f]]) #try to assign orginal crs to that 
  obj_names[[f]] <- st_transform(obj_names[[f]], crs = 3488)
  
#aggregate workflow

##step 1: translate lagged_cc_date to time index to from wind_speed cube

case_control$row_ind = 1:nrow(case_control)

case_control$date_ind = 
difftime(as.POSIXct(case_control$lagged_cc_date),
         st_dimensions(obj_names[[f]])$day$offset,units = 'days') + 1

##step 2: subset to rows that correspond to the date range of the wind_speed cube

case_control2 = subset(case_control, date_ind <= st_dimensions(obj_names[[f]])$day$to & 
                                    date_ind >= st_dimensions(obj_names[[f]])$day$from)

##step 3: aggregate mean wind speed for unique geometries

#convert to character for faster indexing
case_control2$geom_char = as.character(case_control2$geometry)

geom_inds = tapply(1:nrow(case_control2), case_control2$geom_char, function(x) x)

geoms = names(geom_inds)

rows = sapply(geom_inds, '[',1)

by_ = st_as_sf(case_control2[rows,])

temp = aggregate(obj_names[[f]], by_, FUN = mean)

##step 4: retrieve mean wind speeds on relevant dates for each geometry and bring back into case_control dataset

case_control2$mean_wind_speed = NA
case_control$mean_wind_speed = NA

for(i in 1:length(geoms)){
	
	rows_ = geom_inds[[i]]
	
	date_inds = as.numeric(case_control2$date_ind[rows_])
	
	case_control2$mean_wind_speed[rows_] = as.numeric(temp[,i,date_inds]$wind_speed)		
}

case_control$mean_wind_speed[case_control2$row_ind] = case_control2$mean_wind_speed

#rm(obj_names[f])

gc()

}

##step 5: save results

saveRDS(case_control, "cc_wind.rds")



