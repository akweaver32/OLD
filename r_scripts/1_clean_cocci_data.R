#CLEANING CASE DATA
#updated with data, feb 2021
#cases = 74662
#cases_endemic = 56609
#run functions.R before this script
#runs in one chunk well


# cleaning cocci data -----------------------------------------------------

#PACKAGES
packages_clean <- c("lubridate", "ggplot2", "tidyverse", "finalfit", "zoo")
lapply(packages_clean, library, character.only = TRUE)

packages_maps <- c("sp", "rgdal", "ggplot2", "plyr", "tidyverse", "rgeos",  "raster", "sf", "maps", "mapdata", "tidycensus")
lapply(packages_maps, library, character.only = TRUE)

#CLEANING 
cocci <- read_csv("cocci_nov2021.csv") %>% #start out with 100741 cocci cases
  as_tibble()

#keep only those with good locations
cocci_location <- cocci %>% 
  filter(Region == "California") %>% #remove not in california (n = 96861)
  filter(X != 0 & Y != 0) %>% #remove cases w/ no geographic location (n = 96861)
  filter(GoodAddress != "Bad") %>% #remove cases w/ bad address (n = 93880)
  filter(GoodCity != 0 & GoodZip != 0) #remove cases w/ both bad city and zip (n = 93613)

#some cases have NA for county, fixed below
#create tiny df to work with NAs
cocci_subNA <- cocci_location %>%
  filter(is.na(Subregion)) %>%
  dplyr::rename("long" = "X", "lat" = "Y")

#download appropriate spatial data
ca_county <- st_read("shapefiles/CA_Counties/CA_Counties_TIGER2016.shp")

#cycle through all 
for (i in (1:nrow(cocci_subNA))) {
  if (is.na(cocci_subNA$Subregion[i])) {
    cocci_subNA$Subregion[i] <- lonlat_to_county(cocci_subNA, ca_county, name_col = "NAME")[i,19]
  }
}

#make cocci sub NA small
cocci_subNA_small <- cocci_subNA %>%
  dplyr::select("...1", "Subregion")

#check that all Subregion NAs have been resolved
table(cocci_subNA$Subregion, useNA = "always")

#join back into cocci_location (with all counties information)
cocci_location2 <- cocci_location %>%
  dplyr::rename("long" = "X", "lat" = "Y") %>%
  left_join(cocci_subNA_small, by = "...1") %>%
  mutate(Subregion = coalesce(Subregion.x, Subregion.y)) %>%
  dplyr::select(-c(Subregion.x, Subregion.y))

#remove incarcerated and clean the data futher
cocci_limited <- cocci_location2 %>%
  filter(incarcerated_anywhere == 0) %>% #remove people who are in prison (n = 86278)
  dplyr::select(-(10:11), -(13:15)) %>% #creating a limited dataset
  dplyr::rename("CID" = "...1")

#rename cocci-limited to cases and fix CID column (n = 86278)
cases <- cocci_limited %>%
  mutate(CID = paste0("case_", 1:nrow(cocci_limited)))


# cases/population --------------------------------------------------------

#get population for rough idea of which counties are most affected
county_case_count <- cases %>%
  group_by(Subregion) %>%
  dplyr::summarise(n_cases = n())

#tidy census
census_api_key("2cccb5cae3ad60ee4c828b8eed0daddf1201566e")
county_census_2020 <- get_decennial(state = "California", geography = "county", variables = "P1_001N", year = 2020)

#calculate incidence over whole time period
county_pop_cases <- county_census_2020 %>%
  mutate("NAME" = str_remove(NAME, " County, California")) %>%
  dplyr::rename("Subregion" = "NAME") %>%
  left_join(county_case_count, by = "Subregion") %>%
  mutate(incidence = n_cases/value, inc_1000 = (n_cases/value)*1000)

#create cases only in endemic counties
endemic_counties <- c("Kern", "Kings", "San Luis Obispo", "Tulare", "Fresno", "Madera", "Merced", 
                      "Monterey", "Ventura", "San Joaquin", "Santa Barbara", "Stanislaus", 
                      "Riverside", "Mariposa")

#create limited dataset with endemic counties (n = 63681,) have to change cocci limited to cases pre running this in maps_test.R
cases_endemic <- cases %>%
  filter(Subregion %in% endemic_counties)

# reporting delays exploration --------------------------------------------

date_vars <- c("year_est_DtOnset", "Est_DtOnset", "DtOnset", "DtDiagnosis", 
               "DtCreate", "DtReceived", "DtLabCollect", 
               "DtDeath")

#how many cases with X date
cases %>%
  ff_glimpse(date_vars)

cases %>%
  missing_plot()

cases_endemic %>%
  missing_plot()

#explore the lags for each month in the record between relevent dates for correction
delays <- cases %>%
  dplyr::select(Est_DtOnset, DtOnset, DtDiagnosis, DtCreate, DtReceived, DtLabCollect, DtDeath) %>%
  group_by(year = floor_date(as_date(Est_DtOnset), "year")) %>%
  dplyr::summarise(med_diag = -median(difftime(DtOnset, DtDiagnosis, unit = "days"), na.rm = TRUE),
                   med_create = -median(difftime(DtOnset, DtCreate, unit = "days"), na.rm = TRUE),
                   med_recieved = -median(difftime(DtOnset, DtReceived, unit = "days"), na.rm = TRUE),
                   med_lab = -median(difftime(DtOnset, DtLabCollect, unit = "days"), na.rm = TRUE),
                   med_death = -median(difftime(DtOnset, DtDeath, unit = "days"), na.rm = TRUE),
                   count = n()) %>%
  mutate(smooth_diag = rollmean(med_diag, k = 3, fill = NA),
         smooth_create = rollmean(med_create, k = 3, fill = NA),
         smooth_recieved = rollmean(med_recieved, k = 3, fill = NA),
         smooth_lab = rollmean(med_lab, k = 3, fill = NA),
         smooth_death = rollmean(med_death, k = 3, fill = NA))

#plot the smooths see if there is a pattern

ggplot(delays) +
  geom_smooth(aes(x = month_year, y = smooth_diag), se = FALSE, col = "red") +
  geom_smooth(aes(x = month_year, y = smooth_create), se = FALSE, col = "purple") +
  geom_smooth(aes(x = month_year, y = smooth_death), se = FALSE) +
  labs(x="Month-Year", y="Delay (days)")

#weird ones
ggplot(delays) +
  geom_smooth(aes(x = month_year, y = smooth_recieved), se = FALSE, col = "red") +
  geom_smooth(aes(x = month_year, y = smooth_lab), se = FALSE) +
  labs(x="Month-Year", y="Delay (days)")
 
#use the smoothed delayed to correct the exposure data then add 14 days for the estimated exposure day
case_adjust <- cases %>%
  mutate(DtAdjusted = NA, month_year = floor_date(as_date(Est_DtOnset), "month")) %>%
  left_join(delays, by = "month_year") 

#dt DEATH (inserts death date - month specific reporting delay - 14 days) 
for (i in 1:nrow(case_adjust)) {
  if(!is.na(case_adjust$DtDeath[i]) & (case_adjust$DtDeath[i] == case_adjust$Est_DtOnset[i]))  {
    case_adjust$DtAdjusted[i] <- as.Date(case_adjust$DtDeath[i] - case_adjust$smooth_death[i] - 14)
  }
}

#dt CREATE (inserts create date - month specific reporting delay - 14 days)
for (i in 1:nrow(case_adjust)) {
  if(!is.na(case_adjust$DtCreate[i]) & (case_adjust$DtCreate[i] == case_adjust$Est_DtOnset[i]))  {
    case_adjust$DtAdjusted[i] <- as.Date(case_adjust$DtCreate[i] - case_adjust$smooth_create[i] - 14)
  }
}

#dt RECEIVED (inserts recieved date - month specific reporting delay - 14 days) don't use
for (i in 1:nrow(case_adjust)) {
  if(!is.na(case_adjust$DtReceived[i]) & (case_adjust$DtReceived[i] == case_adjust$Est_DtOnset[i]))  {
    case_adjust$DtAdjusted[i] <- as.Date(case_adjust$DtReceived[i] - case_adjust$smooth_recieved[i] - 14)
  }
}

#dt LAB COLLECT (inserts lab collect date - month specific reporting delay - 14 days) don't use
for (i in 1:nrow(case_adjust)) {
  if(!is.na(case_adjust$DtLabCollect[i]) & (case_adjust$DtLabCollect[i] == case_adjust$Est_DtOnset[i]))  {
    case_adjust$DtAdjusted[i] <- as.Date(case_adjust$DtLabCollect[i] - case_adjust$smooth_lab[i] - 14)
  }
}

#dt DIAGNOSIS (inserts diagnosis date - month specific reporting delay - 14 days)
for (i in 1:nrow(case_adjust)) {
  if(!is.na(case_adjust$DtDiagnosis[i]) & (case_adjust$DtDiagnosis[i] == case_adjust$Est_DtOnset[i]))  {
    case_adjust$DtAdjusted[i] <- as.Date(case_adjust$DtDiagnosis[i] - case_adjust$smooth_diag[i] - 14)
  }
}

# dt ONSET (inserts the onset date - 14 days, fully adjusted) - last to override others
for (i in 1:nrow(case_adjust)) {
  if (!is.na(case_adjust$DtOnset[i])) {
    case_adjust$DtAdjusted[i] <- as.Date(case_adjust$DtOnset[i] - 14)
  }
}

#check, still some NAs left (from not having a smooth to adjust with?)
count(is.na(case_adjust$DtAdjusted)) #(n = 36449)

#check if NAs are spatially distributed
check_noNA <- case_adjust %>%
  filter(!is.na(DtAdjusted)) %>%
  group_by(Subregion) %>%
  dplyr::summarise(count = n())

check <- case_adjust %>%
  group_by(Subregion) %>%
  dplyr::summarise(count_total = n()) %>%
  left_join(check_noNA, by = "Subregion") %>%
  mutate(NAs = count_total - count)



#convert the DtAdjusted column back to date format, drop delays columns
cocci_adjusted <- case_adjust %>%
  mutate(DtAdjusted = as.Date(DtAdjusted), diff = as.numeric(DtAdjusted - Est_DtOnset)) %>%
  dplyr::select(-c(15:26)) 

cocci_adjusted_endemic <- cocci_adjusted %>%
  filter(Subregion %in% endemic_counties)

count(is.na(cocci_adjusted_endemic$DtAdjusted))

test <- case_adjust %>%
  group_by(month_year) %>%
  dplyr::summarise( count = n())

# final products ----------------------------------------------------------






