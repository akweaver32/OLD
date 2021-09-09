
setwd("~/Desktop/earthquakes/")
options(stringsAsFactors = F)
library(parallel)
library(dplyr)
library(sf)
library(raster)

cases <- read.csv("CACasesWExposure.csv") # has lat/lon, onset date
cases <- cases[,c("X","Y","Est_DtOnset","Age")]
names(cases) <-  c("longitude","latitude","date","Age")
cases$CID <- paste0("ID", 1:nrow(cases))

hazardLength <- 60 # Median time between exposure and case being reported

##### Create matrix of time periods ######################################################
cases$EstOnsetDt <- as.Date(cases$EstOnsetDt, origin = "1960-01-01") 

# For each case, find the same date 1-4 years prior to serve as acontrol period
# Note it's not exactly one year prior,  doesn't accommodate leap years 
getControlPeriods <- function(i) {
  cs <- cases[i,]
  dates <- cs$EstOnsetDt - (1:4 * 365)
  ndates <- length(dates)
  dates <- dates[dates != cs$EstOnsetDt]
  return(data.frame("CID" = rep(cs$CID, ndates), 
                    "date" = dates, 
                    "case" = 0))
}

# Make a list of dataframes for each case
cl <- makeCluster(3)
clusterExport(cl, varlist = c("getControl", "cases"))
tmp <- parLapply(cl, 1:nrow(cases), getControl) # 30s to run
stopCluster(cl)

# Bind all these dataframes together
begin <- Sys.time()
controls <- do.call(rbind, tmp)
end <- Sys.time()
print(end - begin) # 7 mins to run

# Now add case periods
casemat <- rbind(cases %>% select(CID, date) %>% mutate(case = 1), controls)
rownames(casemat) <- c()
#write.csv(casemat,"ExposureMat.csv")

##### Get exposure data  ####################################################

casemat <- read.csv("ExposureMat.csv") # has bare essential case data to save memory
casemat$date <- as.Date(casemat$date) # by default R reads in dates as strings

# Convert dataframes to sf objects so you can use spatial functions
# IMPORTANT: I project the lon/lat coords so I can consider distance between points in meters
# if unprojected, distances are in degrees which are uninterpretable
quakes <- read.csv("quakes/earthquakes0018.csv") %>% select(latitude,longitude, date, qid)
quakes <- st_as_sf(quakes, coords = c("longitude","latitude"), crs = 4326) %>% st_transform(.,crs = 3488) 
# epsg:3488 is good for CA

cases <- read.csv("CACasesWExposure.csv") # contains detailed case data
cases <- st_as_sf(coords = c("longitude","latitude"), data = cases, crs = 4326) %>% st_transform(.,crs = 3488) 


begin <- Sys.time()
cl <- makeCluster(3)
# you need to tell the cluster about any variables you need
clusterExport(cl, varlist = c("casemat", "hazardLength", "quakes")) 


# Determine if the case point overlaps with the pga raster
# ie does the case feel any effects of the quake
inRange <- function(caserow, r) {
  ext <- extent(r) # xmin, xmax, ymin, ymax
  goodLat <- (caserow$latitude >= ext[3]) && (caserow$latitude <= ext[4])
  goodLon <- (caserow$longitude >= ext[1]) && (caserow$longitude <= ext[2])
  return(goodLon && goodLat)
}

# For each case-period, find quakes that occurred during hazard period
# Retrieve the shakemap raster for that quake and extract the pga experienced by the case
getExposure <- function(i, sm) {
  pt <- casemat[i,]
  ptLoc <- cases[cases$CID == pt$CID, ] # retrieve location data from cases df
  mindate <- pt$date - hazardLength
  recentQs <- quakes[(quakes$date >= mindate)&(quakes$date < pt$date),] 
  if (nrow(recentQs) == 0) { # no earthquakes at that time
    return(0) 
  } else {
    distances <- pointDistance(recentQs, ptLoc, lonlat = F)
    indexofnearest <- which.min(distances)
    qid <- recentQs[indexofnearest, 'qid']
    if (qid %in% c("-999", "-900", "-909")) { 
      inzone <- as.numeric(qid) # exposed but NA data
    } else {
      files <- list.files(paste0("smrasters/", qid))
      f <- f[!grepl("xml", f)] # find the tif/flt file we can extract from
      if (length(f) == 0) { 
        inzone <- NA
      } else {
        r <- raster(paste0("smrasters/", qid, "/", f), crs = "+proj=longlat +datum=WGS84")
        if (inRange(pt, r)) { # the pga rasters have limited range
          ptmat <- data.frame(x = ptLoc$longitude, y = ptLoc$latitude)
          inzone <- extract(r, ptmat, method = "simple", fun = base::max) 
          # If you want a buffer around the point use argument buffer=25 for 25 m
          # I think the function is irrelevant since the simple method is to just return the values in the cell
        } else {
          inzone <- 0
        }
      }
    }
  }
  return( inzone )
}

pga <- parSapply(cl, 1:nrow(casemat), getExposure, shape = "pga") # took 39 mins
casemat$pga <- pga
stopCluster(cl)
end <- Sys.time()
print(end - begin)
#write.csv(casemat, "ExposureMat.csv")


##### clogit #############################################################################

casemat <- read.csv("ExposureMat.csv")
casemat$date <- as.Date(casemat$date)
casemat[casemat$pga %in% c(-999, -909), "pga"] <- NA
casemat$pgaBinary <- as.integer(casemat$pga >= 5)
nona <- casemat[!is.na(casemat$pga),]



library(Epi)
summ <- data.frame()
clOut <- Epi::clogistic(case ~ pga, strata = CID, data = nona)
data.frame(coef = round(clOut$coefficients, 2), n = clOut$n)


##### cpoisson #########################################################

library(gnm)
cpOut1 <- gnm(case ~ pga, data=casemat[clOut1$informative, ], family=poisson, eliminate=factor(year))
summary(cpOut1) 

