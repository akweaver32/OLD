### This code calculates the mean annual precipitation experienced between 2010-2019
### within varying buffer sizes (5,10,15km) surrounding CARB monitor sites in Cocci endemic counties 

#It downloads daily rainfall, mean/min/max temperature, average dew point data using the prism package in R
##https://prism.oregonstate.edu/documents/PRISM_datasets.pdf
# and then does a raster extract

################ IMPORT PACKAGES ##############
#install.packages("prism")
library(prism) #use to download and create raster stack of PRISM environmental data
library(lubridate) #epiweek()
library(dplyr) 
library(tidyr)
library(raster) #crs()
library(rgdal) #reads in shapefile readOGR()
library(devtools)
#install_github("hunzikp/velox", force = TRUE)
library(velox) #raster extract
#####################################################################################################

### 1. Extract total weekly precipitation from PRISM data

################ DOWNLOAD PPT RASTERS ##############
# create new folder to put downloads in
options(prism.path = "/global/scratch/users/erikalee/ppt")
# download daily ppt data 2010-2019
get_prism_dailys(type  ="ppt", minDate=as.Date("2010-01-01"), maxDate =as.Date("2019-12-31"), keepZip = FALSE)
#ls_prism_data(name=TRUE)


################ CREATE PPT RASTER STACK ##############
# create stack of rasters for extraction
data_ppt <- as.data.frame(prism_archive_ls()) #creates a list of all the rasters in the prism.path
index_ppt <- 1:nrow(data_ppt)
RS_ppt <- pd_stack(data_ppt[index_ppt,])#creates a raster stack of all rasters in 'data' from the prism.path folder
#RS_ppt <- pd_stack(prism_archive_subset( "ppt","daily", minDate = as.Date("2010-01-01"),maxDate = as.Date("2019-12-31")))#creates a raster stack of a subset of rasters in 'data' from the prism.path folder 



################ EXTRACT PPT DATA ##############

# import shapefiles for each buffer size
all_buffers <- readOGR("all_buffers.shp") #transfer this file via GUI into savio (and other files!)

# project both files into longlat 
all_buffers <- spTransform(all_buffers, CRS("+proj=longlat +datum=NAD83 +no_defs"))

# check that extent formats match
ext <- extent(RS_ppt)

#check that crs matches 
#crs(RS_test_ppt)
#crs(all_buffers)

#create the velox raster
vx_ppt <- velox(RS_ppt, extent=ext, crs=crs(all_buffers))

#perform the extraction using velox package (FASTER)
vals_ppt <- vx_ppt$extract(all_buffers, fun= function(x) mean(x, na.rm = TRUE), small = TRUE)
vals_ppt <- data.frame(vals_ppt)

write.csv(vals_ppt, "vals_ppt.csv")

################ ORGANIZE DATAFRAME ##############

dates_ppt <- names(RS_ppt)

colnames(vals_ppt) <- dates_ppt 

ID <- all_buffers@data$ID #make list of IDs (site code and buffer size) from buffer data

vals_ppt <- mutate(vals_ppt, buff_ID = ID)

data_long_ppt <- vals_ppt %>% gather(date, ppt, 1:(ncol(vals_ppt)-1)) %>%
  mutate(year=factor(substr(date,24,27)),
         month=factor(substr(date,28,29)),
         day=factor(substr(date,30,31))) %>% 
  dplyr::select(-date) %>%
  mutate(date = as.Date(paste(year, month, day, sep="-"))) %>%
  mutate(epiweek = epiweek(date), epiyear = epiyear(date))

data_sum_ppt <- data_long_ppt %>% 
  group_by(buff_ID, epiyear, epiweek) %>%
  summarise(ppt_total = sum(ppt)) #taking the sum of ppt to get total ppt per week 

write.csv(data_sum_ppt, "PRISM_sum_ppt.csv")

#####################################################################################################