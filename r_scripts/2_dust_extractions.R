#environmental extractions work flow
#Amanda Weaver (derived from Nicole Keeney)

## Script name: dust_analysis.R
##
## Purpose of script: Compute the fraction dust concentration per land use type across coccidiomycosis endemic regions in California
## Tasks remaining: do extractions with increased dust area catchment

# ------------------ Dependencies ------------------

packages_extraction <- c("tidyverse", "ncdf4", "raster", "sf", "lubridate", 
                         "exactextractr", "plyr", "dplyr")
lapply(packages_extraction, library, character.only = TRUE)
source("r_scripts/dust_utils.R") # Helper functions


# ------------------ Define filepaths  ------------------

# Set locations to data and check that paths exits
#WUSTL_FOLDER <- paste("data/SOIL", as.character(year), sep="/") %>% check_path # Path to WUSTL data 
WUSTL_FOLDER <- paste("environmental data/SOIL") #for on your computer

#CENSUS_PATH <- "data/CDPH_ct" %>% check_path # Path to counties shapefile
CENSUS_PATH <- "shapefiles/ca_census_tract/CDPH_ct" %>% check_path # Path to counties shapefile for testing on your computer

#SHAPEFILE_PATH <- "data/CA_Counties" %>% check_path # Path to counties shapefile
SHAPEFILE_PATH <- "shapefiles/CA_Counties" %>% check_path # Path to counties shapefile for testing on your computer

#geoIDs in the split counties
#endemic_counties_X <- c("WKern", "EKern", "WFresno", "WTulare", "San Luis Obispo", "Ventura", "Kings", "Monterey", 
                        #"San Joaquin", "NLA", "Merced", "Stanislaus", "Santa Barbara", "WMadera")

#geo_split <- read_csv("GEOIDs_in_Divided_Counties.csv") %>%
  #filter(county1 %in% endemic_counties_X)


# ------------------ Read in census raster & CA counties shapefile ------------------

#doing all splint counties but w/o the split
#Read in cropscape raster & shapefile 
counties <- read_centralValley(SHAPEFILE_PATH) # Read in shapefile of Central Valley counties of interest
census_tracts <- read_census(CENSUS_PATH) # Read in census tract for extraction
  
  # ------------------ Read in WUSTL raster ------------------
  
#create storage vectors
  year <- 2000 #need to do for 2000-2017
  months <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
  names <- paste0("wustl_", months, "_", year) 
  results <- paste0("wustl_results_", months, "_", year)
 
#read in all 20XX monthly rasters  
  for (i in 1:12) {
    assign(names[i], read_wustl(WUSTL_FOLDER=WUSTL_FOLDER, 
                           year=year, 
                           month=months[i], 
                           geom=counties))
    }
  
#need to take the rasters and make them not characters, into a list    
rasters <- c(wustl_01_2000, wustl_02_2000, 
                wustl_03_2000, wustl_04_2000, 
                wustl_05_2000, wustl_06_2000, 
                wustl_07_2000, wustl_08_2000, 
                wustl_09_2000, wustl_10_2000, 
                wustl_11_2000, wustl_12_2000) 

#give them names
names(rasters) <- names

# perform analysis --------------------------------------------------------

#perform analysis on each
  for (i in 1:12) {
    assign(results[i], mean_dust_wustl_polys(wustl_raster = rasters[[i]],
                                                census_raster = census_tracts))
  }

#check that the values make sense
test <- wustl_results_04_2013 %>%
  left_join(census_tracts, by = "GEOID")

ggplot() +
  geom_sf(test, mapping = aes(geometry = geometry, fill = dust_mean), size = 0.05) + 
  geom_sf(counties, mapping = aes(geometry = geometry), fill = NA, color = "red") +
  coord_sf() + 
  scale_fill_viridis_c()

wustl_04_2013 %>%
  as.data.frame(xy = TRUE) %>%
  ggplot() + 
  geom_raster(aes(x = x, y = y, fill = SOIL)) + 
  geom_sf(counties, mapping = aes(geometry = geometry), fill = NA, color = "red") +
  coord_sf() + 
  scale_fill_viridis_c()

       
#bind them together into one year .csv file
MD2017 <- rbind.data.frame(wustl_results_01_2017, wustl_results_02_2017, 
                                   wustl_results_03_2017, wustl_results_04_2017, 
                                   wustl_results_05_2017, wustl_results_06_2017, 
                                   wustl_results_07_2017, wustl_results_08_2017, 
                                   wustl_results_09_2017, wustl_results_10_2017, 
                                   wustl_results_11_2017, wustl_results_12_2017)


#save the .csv file
write_csv(MD2017, "MD_CT_2017.csv")


# create whole brick ------------------------------------------------------



#bind them all together into one and remove the counties outside of our endemic analysis
dust.files = list.files(pattern = "MD_CT_")
mean_dust_all <- do.call(rbind,lapply(dust.files,read.csv))

#check the county codes for each and remove the non-endemic coutnies
mean_dust_all$GEOID <- as.character(mean_dust_all$GEOID)

mean_dust_endemic <- mean_dust_all %>%
  left_join(county_codes, by = "GEOID") %>%
  dplyr::select(-County) %>%
  mutate(county = str_remove(county, " County"))

#save this as .csv
write_csv(mean_dust_endemic, "MD_CT_endemic_00_17.csv")


# create animations -------------------------------------------------------

dust.files = list.files(path = "environmental data/SOIL", pattern = "GWR", full.names = TRUE)

#for endemic counties
all_rasters <- stack(dust.files) %>%
  crop(counties) %>% 
  raster::mask(counties)

all_names <- as.character(seq(lubridate::ymd("2000-01-01"), lubridate::ymd("2017-12-01"), by = "months")) %>%
  str_sub(end = -4)

fun <- function() {
  plot(counties$geometry, fill = NA, add=T)
}

raster::animate(all_rasters, n = 1, main = all_names, addfun = fun)

animation::saveGIF(raster::animate(all_rasters, n = 1, pause = 0.05, 
                                   main = all_names, addfun = fun), "dust_all.gif")

#all CA
CA <- read_sf("shapefiles/CA_Counties") %>%
  st_transform(4326)

#read in dust rasters
all_rasters <- stack(dust.files) %>%
  crop(CA) %>% 
  raster::mask(CA)

all_names <- as.character(seq(lubridate::ymd("2000-01-01"), lubridate::ymd("2017-12-01"), by = "months")) %>%
  str_sub(end = -4)

fun <- function() {
  plot(CA$geometry, fill = NA, add=T)
}

raster::animate(all_rasters, n = 1, main = all_names, addfun = fun)

animation::saveGIF(raster::animate(all_rasters, n = 1, pause = 0.05, 
                                   main = all_names, addfun = fun), "dust_all_CA.gif")



