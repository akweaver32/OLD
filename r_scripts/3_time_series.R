#time series analysis - Poisson
#census tracts creating the data frame for analysis in time_series_models.R

packages_time <- c("tidyverse", "ncdf4", "stars", "tigris", "dlnm", "splines", "rgeos", "rgdal", "data.table", "glmmTMB")
lapply(packages_time, library, character.only = TRUE)

#get number of cases per census tract per month
cases_census_monthly <- readRDS("extracted data/Cases_CT_Month_Nov2021.rds") %>%
  mutate(GEOID = substring(GEOID, 2)) 

#endemic counties
endemic_counties <- c("Kern", "Fresno", "Tulare", "San Luis Obispo", "Ventura", "Kings", "Monterey", 
                      "San Joaquin", "Los Angeles", "Merced",  "Stanislaus", "Santa Barbara","Madera")
#endemic counties from split Jen counties
endemic_counties_X <- c("WKern", "EKern", "WFresno", "WTulare", "San Luis Obispo", "Ventura", "Kings", "Monterey", 
                      "San Joaquin", "NLA", "Merced", "Stanislaus", "Santa Barbara", "WMadera")

#get the GEOIDs from split counties
split_GEOID <- read_csv("GEOIDS_in_Divided_Counties.csv") %>%
  dplyr::filter(county1 %in% endemic_counties_X) %>% #just the split counties
  mutate(GEOID = substring(GEOID, 2))

corrected_GEOID <- c(split_GEOID$GEOID, "6029005508", "6079012302", "6053011101", "6019008402", "6077004902", "6029006100", "6029000101", "6029004301", "6107003100", "6107000900", "6077003803")

#read in California census tracts
CT <- readOGR("shapefiles/ca_census_tract/CDPH_ct", "cb_2017_06_tract_500k")

#assign correct spatial information
proj4string(CT) <- CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')
CT_proj <- spTransform(CT, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

#get sizes of CT  (in m^2)
CT_sf <- st_as_sf(CT_proj) 
CT_sf <- CT_sf %>%
  mutate(area = st_area(geometry), area_km = as.vector(area/1000000)) %>%
  mutate(GEOID = substring(GEOID, 2), GEOID = as.numeric(GEOID)) %>%
  dplyr::select(GEOID, area_km)

#CT_proj has the county codes for census tracts
data("fips_codes")
fips_codes <- fips_codes %>%
  filter(state == "CA")

county_codes <- cbind.data.frame(CT_proj@data$COUNTYFP, CT_proj@data$GEOID) %>%
  `colnames<-`(c("County", "GEOID")) %>%
  left_join(fips_codes, by = c("County" = "county_code")) %>%
  dplyr::select(-c("state", "state_code", "state_name")) %>%
  mutate(GEOID = substring(GEOID, 2)) %>%
  mutate(county_num = str_remove(County, "^0+")) %>%
  mutate(county = str_remove(county, " County")) %>%
  mutate(county_num = as.numeric(county_num))

#keep endemic counties
cases_census_monthly_endemic <- cases_census_monthly %>%
  left_join(county_codes, by = "GEOID") %>%
  dplyr::select(-County) %>%
  mutate(county = str_remove(county, " County")) %>%
  filter(county %in% endemic_counties) %>% #limit to the full counties (will drop the splits latter)
  filter(OnsetYear <= 2017) #limit to the years we have dust data

#read in dust data
mean_dust_endemic <- read_csv("extracted data/WashU dust means/MD_CT_endemic_00_17.csv") %>%
  mutate(GEOID = as.character(GEOID))

#bind to the dust data
cases_dust_endemic <- cases_census_monthly_endemic %>%
  left_join(mean_dust_endemic, by = c("GEOID", "OnsetMonth" = "month", #join dust data for that month
                                      "OnsetYear" = "year",
                                      "county" = "county",
                                      "county_num")) %>%
  mutate(time = lubridate::make_date(OnsetYear, OnsetMonth)) #create a combined metric for grouping for proper lags

#read in census yearly population data to et mean population 
census_pop <- read_csv("extracted data/demographic info/CA_CT_Census_Info_2000_2017.csv")%>%
  dplyr::select(YEAR, GEO.id2, TotalPopulation) %>%
  dplyr::rename("GEOID" = "GEO.id2", "year" = "YEAR") %>%
  dplyr::mutate(GEOID = as.character(GEOID)) %>%
  dplyr::group_by(GEOID) %>%
  dplyr::summarise(mean_pop = mean(TotalPopulation, na.rm = TRUE))

total_pop <- read_csv("extracted data/demographic info/CA_CT_Census_Info_2000_2017.csv")%>%
  dplyr::select(YEAR, GEO.id2, TotalPopulation) %>%
  dplyr::rename("GEOID" = "GEO.id2", "year" = "YEAR") %>%
  dplyr::mutate(GEOID = as.character(GEOID))

#bind to the dust data
dust_data <- cases_dust_endemic %>%
  dplyr::filter(GEOID != "6111990100") %>% #remove two island census tracts with no dust data
  dplyr::filter(GEOID != "6037990300") %>%
  dplyr::filter(GEOID %in% corrected_GEOID) %>% #filter out the split counties (277776 entries)
  left_join(census_pop, by = "GEOID") %>% #mean population over available years of data
  left_join(total_pop, by = c("GEOID", "OnsetYear" = "year")) %>%
  left_join(CT_sf, by = "GEOID") %>%
  group_by(GEOID) %>% #for proper creation of dust lags
  arrange(time) %>%
  mutate(dust_season = case_when(between(OnsetMonth, 5, 7) ~ 'Spring', # get the season of dust exposures (based on dust lag 2)
                                 between(OnsetMonth, 8, 10) ~ 'Summer',
                                 (OnsetMonth %in% c(1, 11, 12)) ~ 'Fall',
                                 TRUE ~ 'Winter'),
         dust_lag1 = dplyr::lag(dust_mean, n = 1, default = NA),
         dust_lag2 = dplyr::lag(dust_mean, n = 2, default = NA), #add in the manual dust lags
         dust_lag3 = dplyr::lag(dust_mean, n = 3, default = NA),
         dust_lag4 = dplyr::lag(dust_mean, n = 4, default = NA),
         centroid = st_centroid(geometry), #get the centroid of the census tract
                long = st_coordinates(centroid)[,1], #get lat long of census tracts for smoothing
                lat = st_coordinates(centroid)[,2]) 

#fill in Total population information
dust_data_pop <- dust_data %>%
  mutate(TotalPopulation = ifelse(is.na(TotalPopulation), mean_pop, TotalPopulation),
         pop_dense = TotalPopulation/area_km,
         GEOID = as.numeric(GEOID)) 
  
#adjust total population so model doesn't throw errors
dust_data_pop$mean_pop[dust_data_pop$mean_pop < 100] <- 1000
dust_data_pop$TotalPopulation[dust_data_pop$TotalPopulation < 100] <- 1000

#make dust season a factor and reorder
dust_data_pop$dust_season <- factor(dust_data_pop$dust_season, levels = c("Winter", "Fall", "Spring", "Summer"))
levels(dust_data_pop$dust_season)

#clean up environment 
rm(cases_census_monthly, cases_census_monthly_endemic, cases_dust_endemic, census_pop, mean_dust_endemic, total_pop)

# control for winter precip -----------------------------------------------
PRISM <- read_csv("extracted data/environmental data/Monthly Summary of Precip and Temp PRISM data_CA Census Tract.csv")

#smaller version of county_codes
county_codes_tiny <- county_codes %>%
  select(county, county_num) %>%
  unique()

#make prism data smaller
prism_monthly <- PRISM %>%
  dplyr::filter(year > 1998 & year < 2019) %>%
  dplyr::select(c(GEO_ID, COUNTY, year, TotalRain, AvgRain, AvgTmean)) %>%
  rename("GEOID" = "GEO_ID") %>%
  dplyr::mutate(GEOID = substring(GEOID, 11)) %>%
  filter(COUNTY %in% c(29, 19, 107, 79, 111, 31, 53, 77, 37, 47, 99, 83, 39)) %>%
  mutate(GEOID = as.numeric(GEOID)) %>%
  group_by(GEOID, year) %>%
  dplyr::mutate(month = seq(1,12), COUNTY = as.numeric(COUNTY)) %>%
  left_join(county_codes_tiny, by = c("COUNTY" = "county_num")) 

#get previous winter precipitation (need to have epi_year) 
prism_winter_precip <- prism_monthly %>%
  dplyr::filter(month %in% c(1, 2, 12)) %>% #winter rain defined as dec jan feb
  dplyr::filter(county %in% endemic_counties) %>%
  mutate(time = lubridate::make_date(year, month))

#can also get temperature out of this too
prism_summer_temp <- prism_monthly %>%
  dplyr::filter(month %in% c(6,7,8)) %>% #summer temp defined as jun jul aug
  dplyr::filter(county %in% endemic_counties) %>%
  mutate(time = lubridate::make_date(year, month))

#create epi-year
for (i in 1:nrow(prism_winter_precip)) {
  if (prism_winter_precip$time[i] < as.Date("1999-11-01")) {
    prism_winter_precip$epi_year[i] = "pre1999"
  } else if (prism_winter_precip$time[i] > as.Date("1999-11-01") & prism_winter_precip$time[i] < as.Date("2000-03-01")) {
    prism_winter_precip$epi_year[i] = "1999/2000"
  } else if (prism_winter_precip$time[i] > as.Date("2000-11-01") & prism_winter_precip$time[i] < as.Date("2001-03-01")) {
    prism_winter_precip$epi_year[i] = "2000/2001"
  } else if (prism_winter_precip$time[i] > as.Date("2001-11-01") & prism_winter_precip$time[i] < as.Date("2002-03-01")) {
    prism_winter_precip$epi_year[i] = "2001/2002"
  } else if (prism_winter_precip$time[i] > as.Date("2002-11-01") & prism_winter_precip$time[i] < as.Date("2003-03-01")) {
    prism_winter_precip$epi_year[i] = "2002/2003"
  } else if (prism_winter_precip$time[i] > as.Date("2003-11-01") & prism_winter_precip$time[i] < as.Date("2004-03-01")) {
    prism_winter_precip$epi_year[i] = "2003/2004"
  } else if (prism_winter_precip$time[i] > as.Date("2004-11-01") & prism_winter_precip$time[i] < as.Date("2005-03-01")) {
    prism_winter_precip$epi_year[i] = "2004/2005"
  } else if (prism_winter_precip$time[i] > as.Date("2005-11-01") & prism_winter_precip$time[i] < as.Date("2006-03-01")) {
    prism_winter_precip$epi_year[i] = "2005/2006"
  } else if (prism_winter_precip$time[i] > as.Date("2006-11-01") & prism_winter_precip$time[i] < as.Date("2007-03-01")) {
    prism_winter_precip$epi_year[i] = "2006/2007"
  } else if (prism_winter_precip$time[i] > as.Date("2007-11-01") & prism_winter_precip$time[i] < as.Date("2008-03-01")) {
    prism_winter_precip$epi_year[i] = "2007/2008"
  } else if (prism_winter_precip$time[i] > as.Date("2008-11-01") & prism_winter_precip$time[i] < as.Date("2009-03-01")) {
    prism_winter_precip$epi_year[i] = "2008/2009"
  } else if (prism_winter_precip$time[i] > as.Date("2009-11-01") & prism_winter_precip$time[i] < as.Date("2010-03-01")) {
    prism_winter_precip$epi_year[i] = "2009/2010"
  } else if (prism_winter_precip$time[i] > as.Date("2010-11-01") & prism_winter_precip$time[i] < as.Date("2011-03-01")) {
    prism_winter_precip$epi_year[i] = "2010/2011"
  } else if (prism_winter_precip$time[i] > as.Date("2011-11-01") & prism_winter_precip$time[i] < as.Date("2012-03-01")) {
    prism_winter_precip$epi_year[i] = "2011/2012"
  } else if (prism_winter_precip$time[i] > as.Date("2012-11-01") & prism_winter_precip$time[i] < as.Date("2013-03-01")) {
    prism_winter_precip$epi_year[i] = "2012/2013"
  } else if (prism_winter_precip$time[i] > as.Date("2013-11-01") & prism_winter_precip$time[i] < as.Date("2014-03-01")) {
    prism_winter_precip$epi_year[i] = "2013/2014"
  } else if (prism_winter_precip$time[i] > as.Date("2014-11-01") & prism_winter_precip$time[i] < as.Date("2015-03-01")) {
    prism_winter_precip$epi_year[i] = "2014/2015"
  } else if (prism_winter_precip$time[i] > as.Date("2015-11-01") & prism_winter_precip$time[i] < as.Date("2016-03-01")) {
    prism_winter_precip$epi_year[i] = "2015/2016"
  } else if (prism_winter_precip$time[i] > as.Date("2016-11-01") & prism_winter_precip$time[i] < as.Date("2017-03-01")) {
    prism_winter_precip$epi_year[i] = "2016/2017"
  } else if (prism_winter_precip$time[i] > as.Date("2017-11-01") & prism_winter_precip$time[i] < as.Date("2018-03-01")) {
    prism_winter_precip$epi_year[i] = "2017/2018"
    }
  }

#summarize previous total winter precipitation 
winter_precip <- prism_winter_precip %>%
  group_by(GEOID, epi_year) %>%
  summarise(winter_total = sum(TotalRain))

#and summer mean temperature
summer_temp <- prism_summer_temp %>%
  group_by(GEOID, year) %>%
  summarise(summer_mean = mean(AvgTmean)) 

#clean up environment
rm(county_codes, county_codes_tiny, CT, CT_proj, CT_sf, dust_data, fips_codes, PRISM, split_GEOID, test_tiny, corrected_GEOID, prism_monthly)

#make epi-year for dust data  
for (i in 1:nrow(dust_data_pop)) {
  if (dust_data_pop$time[i] < as.Date("1999-11-01")) {
    dust_data_pop$epi_year[i] = "pre1999"
  } else if (dust_data_pop$time[i] < as.Date("2000-12-01") & dust_data_pop$time[i] >= as.Date("2000-02-29")) {
    dust_data_pop$epi_year[i] = "1999/2000"
  } else if (dust_data_pop$time[i] < as.Date("2001-12-01") & dust_data_pop$time[i]>= as.Date("2001-02-28")) {
    dust_data_pop$epi_year[i] = "2000/2001"
  } else if (dust_data_pop$time[i] < as.Date("2002-12-01") & dust_data_pop$time[i]>= as.Date("2002-02-28")) {
    dust_data_pop$epi_year[i] = "2001/2002"
  } else if (dust_data_pop$time[i] < as.Date("2003-12-01") & dust_data_pop$time[i]>= as.Date("2003-02-28")) {
    dust_data_pop$epi_year[i] = "2002/2003"
  } else if (dust_data_pop$time[i] < as.Date("2004-12-01") & dust_data_pop$time[i]>= as.Date("2004-02-29")) {
    dust_data_pop$epi_year[i] = "2003/2004"
  } else if (dust_data_pop$time[i] < as.Date("2005-12-01") & dust_data_pop$time[i]>= as.Date("2005-02-28")) {
    dust_data_pop$epi_year[i] = "2004/2005"
  } else if (dust_data_pop$time[i] < as.Date("2006-12-01") & dust_data_pop$time[i]>= as.Date("2006-02-28")) {
    dust_data_pop$epi_year[i] = "2005/2006"
  } else if (dust_data_pop$time[i] < as.Date("2007-12-01") & dust_data_pop$time[i]>= as.Date("2007-02-28")) {
    dust_data_pop$epi_year[i] = "2006/2007"
  } else if (dust_data_pop$time[i] < as.Date("2008-12-01") & dust_data_pop$time[i]>= as.Date("2008-02-29")) {
    dust_data_pop$epi_year[i] = "2007/2008"
  } else if (dust_data_pop$time[i] < as.Date("2009-12-01") & dust_data_pop$time[i]>= as.Date("2009-02-28")) {
    dust_data_pop$epi_year[i] = "2008/2009"
  } else if (dust_data_pop$time[i] < as.Date("2010-12-01") & dust_data_pop$time[i]>= as.Date("2010-02-28")) {
    dust_data_pop$epi_year[i] = "2009/2010"
  } else if (dust_data_pop$time[i] < as.Date("2011-12-01") & dust_data_pop$time[i]>= as.Date("2011-02-28")) {
    dust_data_pop$epi_year[i] = "2010/2011"
  } else if (dust_data_pop$time[i] < as.Date("2012-12-01") & dust_data_pop$time[i]>= as.Date("2012-02-29")) {
    dust_data_pop$epi_year[i] = "2011/2012"
  } else if (dust_data_pop$time[i] < as.Date("2013-12-01") & dust_data_pop$time[i]>= as.Date("2013-02-28")) {
    dust_data_pop$epi_year[i] = "2012/2013"
  } else if (dust_data_pop$time[i] < as.Date("2014-12-01") & dust_data_pop$time[i]>= as.Date("2014-02-28")) {
    dust_data_pop$epi_year[i] = "2013/2014"
  } else if (dust_data_pop$time[i] < as.Date("2015-12-01") & dust_data_pop$time[i]>= as.Date("2015-02-28")) {
    dust_data_pop$epi_year[i] = "2014/2015"
  } else if (dust_data_pop$time[i] < as.Date("2016-12-01") & dust_data_pop$time[i]>= as.Date("2016-02-29")) {
    dust_data_pop$epi_year[i] = "2015/2016"
  } else if (dust_data_pop$time[i] < as.Date("2017-12-01") & dust_data_pop$time[i]>= as.Date("2017-02-28")) {
    dust_data_pop$epi_year[i] = "2016/2017"
  } else if (dust_data_pop$time[i] < as.Date("2018-12-01") & dust_data_pop$time[i]>= as.Date("2018-02-28")) {
    dust_data_pop$epi_year[i] = "2017/2018"
  } else {
      dust_data_pop$epi_year[i] = "unassigned" 
  }
}
    
#bind summer and winter climate data to the dust data
dust_data_full <- dust_data_pop %>%
  left_join(summer_temp, by = c("OnsetYear" = "year", "GEOID")) %>%
  left_join(winter_precip, by = c("epi_year", "GEOID"))

table(dust_data_full$epi_year) #25% unassigned

# Vulnerable group prop census -------------------------------------------------------
#add in racial/ethnic groups

#create expanded data frame with all years
GEOID <- split_GEOID$GEOID
df <- cbind.data.frame(rep(2000:2017, each = 1288), rep(GEOID, 18)) 
colnames(df) <- c("year", "GEOID")

#get demographic data
census_RE <- read_csv("extracted data/demographic info/CA_CT_Census_Info_2000_2017.csv")%>%
  dplyr::select(YEAR, GEO.id2, TotalPopulation, PercentBlack, PercentFilipino, PercentHispanic) %>%
  filter(GEO.id2 %in% split_GEOID$GEOID) %>%
  dplyr::rename("GEOID" = "GEO.id2", "year" = "YEAR") %>%
  dplyr::mutate(GEOID = as.numeric(GEOID),
                PercentBlack = as.numeric(PercentBlack),
                PercentFilipino = as.numeric(PercentFilipino),
                PercentHispanic = as.numeric(PercentHispanic)) %>%
  dplyr::group_by(GEOID) %>%
  dplyr::mutate(meanP_Black = mean(PercentBlack, na.rm = TRUE,), #create means to be expanded into data
                meanP_Filipino = mean(PercentFilipino, na.rm = TRUE),
                meanP_Hispanic = mean(PercentHispanic, na.rm = TRUE)) %>%
  mutate_all(~ifelse(is.nan(.), NA, .))

#fill in expanded frame
df2 <- df %>%
  left_join(census_RE, by = c("year", "GEOID")) %>%
  left_join(select(census_RE, -c(year, TotalPopulation, PercentBlack, PercentFilipino, PercentHispanic)), by = "GEOID") %>%
  mutate(PercentBlack = ifelse(is.na(PercentBlack), meanP_Black.y, PercentBlack),
         PercentFilipino = ifelse(is.na(PercentFilipino), meanP_Filipino.y, PercentFilipino), 
         PercentHispanic = ifelse(is.na(PercentHispanic), meanP_Hispanic.y, PercentHispanic)) %>%
  select(-c(meanP_Black.y, meanP_Filipino.y, meanP_Hispanic.y)) %>%
  rename("meanP_Black" = "meanP_Black.x",
         "meanP_Filipino" = "meanP_Filipino.x",
         "meanP_Hispanic" = "meanP_Hispanic.x") %>%
  group_by(year, GEOID) %>%
  unique()

#bind to the dust data
dust_data_full_v2 <- dust_data_full %>%
  left_join(select(df2, -c(TotalPopulation, meanP_Black, meanP_Filipino, meanP_Hispanic)), 
            by = c("GEOID", "OnsetYear" = "year"))

View(dust_data_full_v2)

# save final dataset ------------------------------------------------------

#save as .csv file (contains basic information)
write_csv(dust_data_full_v2, "base_dust_data_CLEAN_FINAL.csv")


