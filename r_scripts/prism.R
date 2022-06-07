#prism extraction of climate data
#works, testing on savio

#function to convert celcius to far
c_to_f <- function(x) {
  far <- x*(9/5) + 32
  return(far)
}

#libraries for savio
packages_prism <- c("prism", "raster", "velox", "exactextractr", "tidyverse", "lubridate", "rgdal", "sf", "doParallel", "foreach")
lapply(packages_prism, library, character.only = TRUE)

#set prism download folder
prism_set_dl_dir("environmental data")
options(prism.path = "/global/scratch/users/amanda_weaver/environmental data") #for savio
options(prism.path = "environmental data") #for my desktop

#set time range
minDate = "2000-01-01"
maxDate = "2020-12-31"

#get dates for column names
dates_tmean <- seq(ymd(minDate), ymd(maxDate), by = 'days')

#test get daily vals
#get_prism_dailys("tmean", minDate = minDate, maxDate = maxDate, keepZip = FALSE)

data_tmean <- as.data.frame(prism_archive_ls()) #creates a list of all the rasters in the prism.path
index_tmean <- 1:nrow(data_tmean) #create index
RS_tmean <- pd_stack(data_tmean[index_tmean,]) #creates a raster stack of all rasters in 'data' from the prism.path folder

RS_tmean_2000 <- pd_stack(data_tmean[1:366,])
RS_tmean_2000 <- setZ(RS_tmean_2000, as.Date((dates_tmean[1:366])), name = "time")


#the big RS from from 2003 to 2020
RS_tmean <- readRDS("tmean.rds")

#tiny RS
tiny_RS <- raster::dropLayer(RS_tmean, c(11:6575))

# import buffer rds
buffer_10k_all <- readRDS("objects for savio/buffers_4326_all.rds") %>%
  dplyr::select("CID","10k") %>%
  st_as_sf() %>%
  st_transform("+proj=longlat +datum=NAD83 +no_defs")  #set crs to match PRISM

#tiny buffers
tiny_buffer <- buffer_10k_all %>%
  slice_head(n = 5) 

#create number of cores for parallelization
numCores <- detectCores()
numCores
registerDoParallel(numCores)

# check that extent formats match
ext <- sf::as_Spatial(buffer_10k_all)

#extraction to dataframe
begin <- Sys.time()
tmean_2000 <- foreach (i = 1:366, .combine=rbind) %dopar% { #looping through each raster and parallelizing
  temp <- RS_tmean_2000[[i]]
  crop <- crop(temp, ext)
  mask <- mask(crop, ext)
  vlr <- velox(mask)
  dat <- as.data.frame(vlr$extract(ext, fun = base::mean))
  names(dat)[1] <- 'tmean'
  dat %>% mutate(CID = buffer_10k_all$CID)
}
end <- Sys.time()
print(begin - end)

#each = number of buffers run
tmean_2000$date <- rep(dates_tmean[1:366], each = 74535)

#write CSV
write.csv(tmean_2000, 'tmean_buffer_2000_20_10k.csv')

#stop cluster
stopImplicitCluster()


