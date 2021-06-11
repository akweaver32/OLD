#CLEANING CASE DATA

#PACKAGES
library("lubridate")
library("tidyverse")

#CLEANING
cocci <- read.csv("GPSpoints_incarcerated.csv") #read in csv of raw data
cocci[2:8] <- lapply(cocci[2:8], as.Date, origin="1960-01-01") #convert dates
