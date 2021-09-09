#wind NetCDFs 

#load packages for savio
#libraries
packages_wind <- c("sp", "rgdal", "tidyverse", "rgeos",  "raster", "sf", "ncdf4", "reshape2", "lubridate", "stars", "ncmeta", "AOI", "climateR", "exactextractr", "zoo")
lapply(packages_wind, library, character.only = TRUE)

#read in case control object
case_control <- readRDS("objects for savio/case_control.rds")
st_crs(case_control$geometry) <- 4326 #originally in 3488
case_control$place <- as.character(case_control$geometry)

#read in buffer rds
buffer_10k <- readRDS("objects for savio/buffer_10k.rds") #crs 4326
st_crs(buffer_10k) <- 4326
buffer_10k$GEOID <- paste0(1:nrow(buffer_10k)) #add ID
buffer_10k$place <- as.character(buffer_10k$geometry)

#get gridMET wind data
buffer_wind <- getGridMET(AOI = buffer_10k,
                          param = "wind_vel",
                          startDate = "2010-12-22",
                          endDate = "2010-12-25") %>%
                                stack() 

#extract mean wind speed for each buffer for data range specified above
buffer_extract <- cbind(buffer_10k$GEOID, exact_extract(buffer_wind, buffer_10k, "mean"))

#clean up the dataframe and bind it to the case crossover data
buffer_clean <- buffer_extract %>%
  pivot_longer(cols = starts_with("mean.X"), names_to = "date", values_to = "wind_speed") %>%
  mutate(date = str_sub(date,7)) %>%
  mutate(date = ymd(date)) %>%
  mutate(place = as.character(geometry)) %>%
  group_by(place, date) %>%
  mutate(roll_mean = rollmean(wind_speed, k = 3, fill = NA)) %>%
  ungroup() %>%
  rename(GEOID = `buffer_10k$GEOID`) %>%
  left_join(buffer_10k, by = "GEOID")

case_control$CID <- as.character(case_control$CID)

#bind to case control by geometry and time
cc_wind <- case_control %>%
  inner_join(buffer_clean, by = c("place", "lagged_cc_date" = "date", "CID" = "GEOID")) %>%
  select(-c("place", "geometry.y")) %>%
  rename("geometry" = "geometry.x")

test <- cc_wind %>%
  group_by(geometry, lagged_cc_date) %>%
  mutate(roll_mean = rollmean(wind_speed, k = 3, fill = NA)) %>%
  ungroup()

#save the dataframe
saveRDS(cc_wind, "cc_wind_test.rds")

