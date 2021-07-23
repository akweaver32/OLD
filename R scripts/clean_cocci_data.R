#CLEANING CASE DATA

#PACKAGES
library("lubridate")
library("tidyverse")

#CLEANING
cocci <- read.csv("GPSpoints_incarcerated.csv") #read in csv of raw data
cocci[2:8] <- lapply(cocci[2:8], as.Date, origin="1960-01-01") #convert dates
cocci_location <- cocci %>% 
  filter(X != 0 & Y != 0) %>% #remove cases w/ no geographic location 
  filter(GoodAddress != "Bad" & GoodCity != 0 & GoodZip != 0) #remove with poor quality location --> coord (one record?)

#how many diagnosed cases with date? 44369
diagnose <- cocci_location %>%
  select(DtDiagnosis) %>%
  summarise(count = sum(!is.na(DtDiagnosis)))

#creating a limited dataset
cocci_limited <- cocci_location %>%
  dplyr::select(-1, -(3:8), -(10:12)) %>%
  rename(long = X, lat = Y) %>%
  filter(long > -130 & lat < 45)  #remove three point out of califoria

#rename columns
names(cocci_limited) <- c("date", "incarcerated", "long", "lat", "age")

#add ID column
cocci_limited$CID <- paste0(1:nrow(cocci_limited))

#reorder columns to put ID first
col_order <- c("CID", "date", "lat", "long", "age", "incarcerated")
cocci_limited <- cocci_limited[, col_order]

#rename cocci-limited to cases (case dataset used in cc_adapt.R)
cases <- cocci_limited

