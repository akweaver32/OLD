#Dust Exposure Assessment (AIM 1)
#Amanda K. Gomez-Weaver 
#looking at the census demographic data we have so far
#also need to run read_wustl function from 2.5_dust_utils.R

#libraries
packages_census <- c("lubridate", "ggplot2", "tidyverse", "tidycensus", "rgeos", "sf", "exactextractr", "raster", "ncdf4")
lapply(packages_census, library, character.only = TRUE)


# read in census data -----------------------------------------------------

#read in the demographic data
#max number of times that CT shows up is 10
#years available 2000 and 2010-2017
census <- read_csv("extracted data/CA_CT_Census_Info_2000_2017.csv") %>%
  dplyr::select(YEAR, GEO.id, GEO.id2, Geography, TotalPopulation, TotPopMOE, MedianAge, MedianAgeMOE, #select the variables you are interested in
         White, PercentWhite, Black, PercentBlack, Asian, PercentAsian,
         Hispanic, PercentHispanic, NonHispanic, PercentNonHispanic)

#look at the available census variables
c_vars00 <- load_variables(2000, "sf1", cache = TRUE)


# tidycensus data ---------------------------------------------------------

#decenial census data
census_api_key("2cccb5cae3ad60ee4c828b8eed0daddf1201566e")

#Total number of people in each group, AIAN = American Indian, Alaska Natice, NHPI = Native Hawaii, Pacific Islander
demo_2000 <- get_decennial(state = "California", geography = "tract", variables = c(Total = "P007001", White = "P007002", Black = "P007003", 
                                                                                    AIAN = "P007004", Asian = "P007005", NHPI = "P007006", 
                                                                                    Other = "P007007", Hispanic = "P008010"),
                           geometry = TRUE,
                           year = 2000) %>%
  st_transform(4326)

var00 <- load_variables(2010, "sf1", cache = TRUE)

#load in the income data from census, see below for categories
# Less than $10,000, $10,000 to $14,999, $15,000 to $19,999, $20,000 to $24,999, $25,000 to $29,999, $30,000 to $34,999
# $35,000 to $39,999, $40,000 to $44,999, $45,000 to $49,999, $50,000 to $59,999, $60,000 to $74,999, $75,000 to $99,999
#$100,000 to $124,999, $125,000 to $149,999, $150,000 to $199,999, $200,000 or more

income_2000 <- read_csv("nhgis0001_csv/nhgis0001_ds151_2000_tract.csv") %>%
  filter(STATE == "California") %>%
  select(GISJOIN, YEAR, 31:47) 

income_2000$GEOID <- demo_2000_wide$GEOID

#pivot so you don't have to do so many extractions
demo_2000_wide <- demo_2000 %>%
  as.data.frame() %>%
  mutate(value = as.numeric(value), variable = as.factor(variable)) %>%
  tidyr::pivot_wider( names_from = variable, values_from = value)

#2010
demo_2010 <- get_decennial(state = "California", geography = "tract", variables = c(Total = "P007001", White = "P007002", 
                                                                                    Black = "P007003", 
                                                                                    AIAN = "P007004", Asian = "P007005", 
                                                                                    NHPI = "P007006", 
                                                                                    Other = "P007007", Hispanic = "P008010"),
                           geometry = TRUE,
                           year = 2010) %>%
  st_transform(4326)

demo_2010_wide <- demo_2010 %>%
  as.data.frame() %>%
  mutate(value = as.numeric(value), variable = as.factor(variable)) %>%
  tidyr::pivot_wider( names_from = variable, values_from = value)


# dust extractions for 2000 and 2010 --------------------------------------

#adapted function
mean_dust_wustl_polys <- function(wustl_raster, census_raster){ 
  # COMPUTE MEAN DUST CONCENTRATON FOR A SPECIFIC MONTH BY CENSUS TRACT
  
  # Get coverage area by polygon 
  mean_dust_extracted <- exactextractr::exact_extract(wustl_raster, # Raster data
                                                      census_raster$geometry, # Polygons to extract raster to 
                                                      'mean', # Get mean for census tract
                                                      progress = TRUE, # Show progress bar
                                                      force_df = TRUE) 
  # bind to census tract information                                                     
  dust_census_results <- cbind(mean_dust_extracted, census_raster$GEOID, 
                               month = months[i], year = as.factor(year)) %>%
    dplyr::rename("dust_mean" = "mean")# Bind list of data frames
  
  return(dust_census_results)
}


#create storage vectors
year <- 2010 #need to do for 2000 and 2010
months <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
names <- paste0("wustl_", months) 
results <- paste0("wustl_results_", months)

#read in california shape
CA <- read_sf("shapefiles/ca-state-boundary") %>%
  st_transform(4326)

#alternative CA census tracts for 2010 and bind to the 2010 demographic data
CA_2010 <- read_sf("shapefiles/tl_2010_06_tract10") %>%
  st_transform(4326)

demo_2010_wide <- demo_2010_wide %>%
  dplyr::select(-geometry) %>%
  left_join(CA_2010, by = c("GEOID" = "GEOID10"))
 
#where your WashU data is
WUSTL_FOLDER <- paste("environmental data/SOIL") #for on your computer

#read in all 20XX monthly rasters  
for (i in 1:12) {
  assign(names[i], read_wustl(WUSTL_FOLDER=WUSTL_FOLDER, 
                              year=year, 
                              month=months[i], 
                              geom=CA))
}

#need to take the rasters and make them not characters, into a list    
rasters <- c(wustl_01, wustl_02, 
             wustl_03, wustl_04, 
             wustl_05, wustl_06, 
             wustl_07, wustl_08, 
             wustl_09, wustl_10, 
             wustl_11, wustl_12) 

#give them names
names(rasters) <- names

#perform analysis on each census year
for (i in 1:12) {
  assign(results[i], mean_dust_wustl_polys(wustl_raster = rasters[[i]],
                                      census_raster = demo_2010_wide))
}

census_2010_results <- rbind.data.frame(wustl_results_01, wustl_results_02, 
                                   wustl_results_03, wustl_results_04, 
                                   wustl_results_05, wustl_results_06, 
                                   wustl_results_07, wustl_results_08, 
                                   wustl_results_09, wustl_results_10, 
                                   wustl_results_11, wustl_results_12)

saveRDS(census_2010_results, "2010.rds")

census_2000_results <- read_rds("2000.rds")

#bind them together into one year .csv file (CHANGE THIS PART)
decennial_results <- rbind.data.frame(census_2000_results, census_2010_results)

#save the .csv file
write_csv(decennial_results, "dec_dust_0010.csv")

# population weighted analysis --------------------------------------------

decennial_results <- read_csv("dec_dust_0010.csv")

#add year columns to demographic data
demo_2000_wide$year <- as.factor(2000)
demo_2010_wide$year <- as.factor(2010)

#revert demo 2010 to smaller form
demo_2010_wide <- demo_2010_wide %>%
  dplyr::select(-c("STATEFP10",  "COUNTYFP10", "TRACTCE10",  "NAME10", "NAMELSAD10", "MTFCC10", "FUNCSTAT10", "ALAND10",   
                   "AWATER10", "INTPTLAT10", "INTPTLON10")) %>%
   relocate(geometry, .after = GEOID)

#combine the demographic data
demographics <- rbind.data.frame(demo_2000_wide, demo_2010_wide)

#get annual dust exposures in census tracts
annual_dust <- decennial_results %>%
  rename("GEOID" = "census_raster$GEOID") %>%
  group_by(GEOID, year) %>%
  summarise(annual_dust = mean(dust_mean)) %>%
  left_join(demographics, by = c("year", "GEOID"))

#annual dust for income
annual_dust_income <- decennial_results %>%
  rename("GEOID" = "census_raster$GEOID") %>%
  filter(year == "2000") %>%
  group_by(GEOID) %>%
  summarise(annual_dust = mean(dust_mean)) %>%
  left_join(income_2000, by = "GEOID") %>%
  arrange(annual_dust)
  
#do each year
weighed_pop_percentiles_2000 <- annual_dust %>%
  filter(year == "2000") %>% #change the year here
  arrange(annual_dust) 

weighed_pop_percentiles_2010 <- annual_dust %>%
  filter(year == "2010") %>% #change the year here
  arrange(annual_dust) 

#cumulative sum for income
annual_dust_income$cGMX001 <- cumsum(annual_dust_income$GMX001)
annual_dust_income$cGMX002 <- cumsum(annual_dust_income$GMX002)
annual_dust_income$cGMX003 <- cumsum(annual_dust_income$GMX003)
annual_dust_income$cGMX004 <- cumsum(annual_dust_income$GMX004)
annual_dust_income$cGMX005 <- cumsum(annual_dust_income$GMX005)
annual_dust_income$cGMX006 <- cumsum(annual_dust_income$GMX006)
annual_dust_income$cGMX007 <- cumsum(annual_dust_income$GMX007)
annual_dust_income$cGMX008 <- cumsum(annual_dust_income$GMX008)
annual_dust_income$cGMX009 <- cumsum(annual_dust_income$GMX009)
annual_dust_income$cGMX010 <- cumsum(annual_dust_income$GMX010)
annual_dust_income$cGMX011 <- cumsum(annual_dust_income$GMX011)
annual_dust_income$cGMX012 <- cumsum(annual_dust_income$GMX012)
annual_dust_income$cGMX013 <- cumsum(annual_dust_income$GMX013)
annual_dust_income$cGMX014 <- cumsum(annual_dust_income$GMX014)
annual_dust_income$cGMX015 <- cumsum(annual_dust_income$GMX015)
annual_dust_income$cGMX016 <- cumsum(annual_dust_income$GMX016)

#get the cumulative sum 2000
weighed_pop_percentiles_2000$cum_total <- cumsum(weighed_pop_percentiles_2000$Total)
weighed_pop_percentiles_2000$cum_White <- cumsum(weighed_pop_percentiles_2000$White)
weighed_pop_percentiles_2000$cum_black <- cumsum(weighed_pop_percentiles_2000$Black)
weighed_pop_percentiles_2000$cum_AIAN <- cumsum(weighed_pop_percentiles_2000$AIAN)
weighed_pop_percentiles_2000$cum_Asian <- cumsum(weighed_pop_percentiles_2000$Asian)
weighed_pop_percentiles_2000$cum_NHPI <- cumsum(weighed_pop_percentiles_2000$NHPI)
weighed_pop_percentiles_2000$cum_other <- cumsum(weighed_pop_percentiles_2000$Other)
weighed_pop_percentiles_2000$cum_hispanic <- cumsum(weighed_pop_percentiles_2000$Hispanic)

#get the cumulative sum 2010
weighed_pop_percentiles_2010$cum_total <- cumsum(weighed_pop_percentiles_2010$Total)
weighed_pop_percentiles_2010$cum_White <- cumsum(weighed_pop_percentiles_2010$White)
weighed_pop_percentiles_2010$cum_black <- cumsum(weighed_pop_percentiles_2010$Black)
weighed_pop_percentiles_2010$cum_AIAN <- cumsum(weighed_pop_percentiles_2010$AIAN)
weighed_pop_percentiles_2010$cum_Asian <- cumsum(weighed_pop_percentiles_2010$Asian)
weighed_pop_percentiles_2010$cum_NHPI <- cumsum(weighed_pop_percentiles_2010$NHPI)
weighed_pop_percentiles_2010$cum_other <- cumsum(weighed_pop_percentiles_2010$Other)
weighed_pop_percentiles_2010$cum_hispanic <- cumsum(weighed_pop_percentiles_2010$Hispanic)

#get the percentage of the totol
df <- weighed_pop_percentiles_2000 %>% 
  mutate(total_percentile = cum_total/33871648*100,
         White_percentile = cum_White/20170059 *100,
         Black_percentile = cum_black/2263882*100,
         #p_AIAN = cum_AIAN/333346*100,
         Asian_percentile = cum_Asian/3697513*100,
         #p_NHPI = cum_NHPI/116961*100,
         Other_percentile = cum_other/5682241*100,
         Hispanic_percentile = cum_hispanic/10966556*100)

#for 2010
df <- weighed_pop_percentiles_2010 %>% 
  mutate(total_percentile = cum_total/39226788*100,
         White_percentile = cum_White/24297483 *100,
         Black_percentile = cum_black/15763625*100,
         #p_AIAN = cum_AIAN/333346*100,
         Asian_percentile = cum_Asian/383957*100,
         #p_NHPI = cum_NHPI/116961*100,
         Other_percentile = cum_other/233405*100,
         Hispanic_percentile = cum_hispanic/1673501*100) #check the number of hispanic people

#for income 2000
df <- annual_dust_income %>% 
  mutate(n1_percentile = cGMX001/967089*100,
         n2_percentile = cGMX002/648780 *100,
         n3_percentile = cGMX003/645181*100,
         n4_percentile = cGMX004/673065*100,
         n5_percentile = cGMX005/653245*100,
         n6_percentile = cGMX006/661840*100,
         n7_percentile = cGMX007/619875*100,
         n8_percentile = cGMX008/595943*100,
         n9_percentile = cGMX009/530143*100,
         n10_percentile = cGMX010/984798*100,
         n11_percentile = cGMX011/1218075*100,
         n12_percentile = cGMX012/1326569*100,
         n13_percentile = cGMX013/780489*100,
         n14_percentile = cGMX014/412129*100,
         n15_percentile = cGMX015/385248*100,
         n16_percentile = cGMX016/409551*100) 

#create percentiles matrix (income)
percentiles <- matrix(c(df[which.min(abs(1 - df$n1_percentile)),2],
                        df[which.min(abs(1 - df$n2_percentile)),2],
                        df[which.min(abs(1 - df$n3_percentile)),2],
                        df[which.min(abs(1 - df$n4_percentile)),2],
                        df[which.min(abs(1 - df$n5_percentile)),2],
                        df[which.min(abs(1 - df$n6_percentile)),2],
                        df[which.min(abs(1 - df$n7_percentile)),2],
                        df[which.min(abs(1 - df$n8_percentile)),2],
                        df[which.min(abs(1 - df$n9_percentile)),2],
                        df[which.min(abs(1 - df$n10_percentile)),2],
                        df[which.min(abs(1 - df$n11_percentile)),2],
                        df[which.min(abs(1 - df$n12_percentile)),2],
                        df[which.min(abs(1 - df$n13_percentile)),2],
                        df[which.min(abs(1 - df$n14_percentile)),2],
                        df[which.min(abs(1 - df$n15_percentile)),2],
                        df[which.min(abs(1 - df$n16_percentile)),2],
                        
                        df[which.min(abs(10 - df$n1_percentile)),2],
                        df[which.min(abs(10 - df$n2_percentile)),2],
                        df[which.min(abs(10 - df$n3_percentile)),2],
                        df[which.min(abs(10 - df$n4_percentile)),2],
                        df[which.min(abs(10 - df$n5_percentile)),2],
                        df[which.min(abs(10 - df$n6_percentile)),2],
                        df[which.min(abs(10 - df$n7_percentile)),2],
                        df[which.min(abs(10 - df$n8_percentile)),2],
                        df[which.min(abs(10 - df$n9_percentile)),2],
                        df[which.min(abs(10 - df$n10_percentile)),2],
                        df[which.min(abs(10 - df$n11_percentile)),2],
                        df[which.min(abs(10 - df$n12_percentile)),2],
                        df[which.min(abs(10 - df$n13_percentile)),2],
                        df[which.min(abs(10 - df$n14_percentile)),2],
                        df[which.min(abs(10 - df$n15_percentile)),2],
                        df[which.min(abs(10 - df$n16_percentile)),2],
                      
                       
                        df[which.min(abs(25 - df$n1_percentile)),2],
                        df[which.min(abs(25 - df$n2_percentile)),2],
                        df[which.min(abs(25 - df$n3_percentile)),2],
                        df[which.min(abs(25 - df$n4_percentile)),2],
                        df[which.min(abs(25 - df$n5_percentile)),2],
                        df[which.min(abs(25 - df$n6_percentile)),2],
                        df[which.min(abs(25 - df$n7_percentile)),2],
                        df[which.min(abs(25 - df$n8_percentile)),2],
                        df[which.min(abs(25 - df$n9_percentile)),2],
                        df[which.min(abs(25 - df$n10_percentile)),2],
                        df[which.min(abs(25 - df$n11_percentile)),2],
                        df[which.min(abs(25 - df$n12_percentile)),2],
                        df[which.min(abs(25 - df$n13_percentile)),2],
                        df[which.min(abs(25 - df$n14_percentile)),2],
                        df[which.min(abs(25 - df$n15_percentile)),2],
                        df[which.min(abs(25 - df$n16_percentile)),2],
                        
                        df[which.min(abs(50 - df$n1_percentile)),2],
                        df[which.min(abs(50 - df$n2_percentile)),2],
                        df[which.min(abs(50 - df$n3_percentile)),2],
                        df[which.min(abs(50 - df$n4_percentile)),2],
                        df[which.min(abs(50 - df$n5_percentile)),2],
                        df[which.min(abs(50 - df$n6_percentile)),2],
                        df[which.min(abs(50 - df$n7_percentile)),2],
                        df[which.min(abs(50 - df$n8_percentile)),2],
                        df[which.min(abs(50 - df$n9_percentile)),2],
                        df[which.min(abs(50 - df$n10_percentile)),2],
                        df[which.min(abs(50 - df$n11_percentile)),2],
                        df[which.min(abs(50 - df$n12_percentile)),2],
                        df[which.min(abs(50 - df$n13_percentile)),2],
                        df[which.min(abs(50 - df$n14_percentile)),2],
                        df[which.min(abs(50 - df$n15_percentile)),2],
                        df[which.min(abs(50 - df$n16_percentile)),2],
                        
                        df[which.min(abs(75 - df$n1_percentile)),2],
                        df[which.min(abs(75 - df$n2_percentile)),2],
                        df[which.min(abs(75 - df$n3_percentile)),2],
                        df[which.min(abs(75 - df$n4_percentile)),2],
                        df[which.min(abs(75 - df$n5_percentile)),2],
                        df[which.min(abs(75 - df$n6_percentile)),2],
                        df[which.min(abs(75 - df$n7_percentile)),2],
                        df[which.min(abs(75 - df$n8_percentile)),2],
                        df[which.min(abs(75 - df$n9_percentile)),2],
                        df[which.min(abs(75 - df$n10_percentile)),2],
                        df[which.min(abs(75 - df$n11_percentile)),2],
                        df[which.min(abs(75 - df$n12_percentile)),2],
                        df[which.min(abs(75 - df$n13_percentile)),2],
                        df[which.min(abs(75 - df$n14_percentile)),2],
                        df[which.min(abs(75 - df$n15_percentile)),2],
                        df[which.min(abs(75 - df$n16_percentile)),2],
                        
                        df[which.min(abs(90 - df$n1_percentile)),2],
                        df[which.min(abs(90 - df$n2_percentile)),2],
                        df[which.min(abs(90 - df$n3_percentile)),2],
                        df[which.min(abs(90 - df$n4_percentile)),2],
                        df[which.min(abs(90 - df$n5_percentile)),2],
                        df[which.min(abs(90 - df$n6_percentile)),2],
                        df[which.min(abs(90 - df$n7_percentile)),2],
                        df[which.min(abs(90 - df$n8_percentile)),2],
                        df[which.min(abs(90 - df$n9_percentile)),2],
                        df[which.min(abs(90 - df$n10_percentile)),2],
                        df[which.min(abs(90 - df$n11_percentile)),2],
                        df[which.min(abs(90 - df$n12_percentile)),2],
                        df[which.min(abs(90 - df$n13_percentile)),2],
                        df[which.min(abs(90 - df$n14_percentile)),2],
                        df[which.min(abs(90 - df$n15_percentile)),2],
                        df[which.min(abs(90 - df$n16_percentile)),2],
                          
                        df[which.min(abs(99 - df$n1_percentile)),2],
                        df[which.min(abs(99 - df$n2_percentile)),2],
                        df[which.min(abs(99 - df$n3_percentile)),2],
                        df[which.min(abs(99 - df$n4_percentile)),2],
                        df[which.min(abs(99 - df$n5_percentile)),2],
                        df[which.min(abs(99 - df$n6_percentile)),2],
                        df[which.min(abs(99 - df$n7_percentile)),2],
                        df[which.min(abs(99 - df$n8_percentile)),2],
                        df[which.min(abs(99 - df$n9_percentile)),2],
                        df[which.min(abs(99 - df$n10_percentile)),2],
                        df[which.min(abs(99 - df$n11_percentile)),2],
                        df[which.min(abs(99 - df$n12_percentile)),2],
                        df[which.min(abs(99 - df$n13_percentile)),2],
                        df[which.min(abs(99 - df$n14_percentile)),2],
                        df[which.min(abs(99 - df$n15_percentile)),2],
                        df[which.min(abs(99 - df$n16_percentile)),2]), byrow = T, ncol = 16) %>%
  as.data.frame()
colnames(percentiles) <- c("Less than $10,000", "$10,000 to $14,999", 
                           "$15,000 to $19,999", "$20,000 to $24,999", 
                           "$25,000 to $29,999", "$30,000 to $34,999",
                           "35,000 to $39,999", "$40,000 to $44,999", 
                           "$45,000 to $49,999", "$50,000 to $59,999", "$60,000 to $74,999", 
                           "$75,000 to $99,999", "$100,000 to $124,999", "$125,000 to $149,999", 
                           "$150,000 to $199,999", "$200,000 or more")
rownames(percentiles) <- c('1%','10%','25%','50%','75%','90%', '99%')


#create percentiles matrix (race ethnicity)
percentiles <- matrix(c(df[which.min(abs(1 - df$total_percentile)),3],
                        df[which.min(abs(1 - df$Asian_percentile)),3],
                        df[which.min(abs(1 - df$Black_percentile)),3],
                        df[which.min(abs(1 - df$Hispanic_percentile)),3],
                        df[which.min(abs(1 - df$White_percentile)),3],
                        df[which.min(abs(1 - df$Other_percentile)),3],
                        df[which.min(abs(10 - df$total_percentile)),3],
                        df[which.min(abs(10 - df$Asian_percentile)),3],
                        df[which.min(abs(10 - df$Black_percentile)),3],
                        df[which.min(abs(10 - df$Hispanic_percentile)),3],
                        df[which.min(abs(10 - df$White_percentile)),3],
                        df[which.min(abs(10 - df$Other_percentile)),3],
                        df[which.min(abs(25 - df$total_percentile)),3],
                        df[which.min(abs(25- df$Asian_percentile)),3],
                        df[which.min(abs(25- df$Black_percentile)),3],
                        df[which.min(abs(25- df$Hispanic_percentile)),3],
                        df[which.min(abs(25- df$White_percentile)),3],
                        df[which.min(abs(25- df$Other_percentile)),3],
                        df[which.min(abs(50 - df$total_percentile)),3],
                        df[which.min(abs(50 - df$Asian_percentile)),3],
                        df[which.min(abs(50 - df$Black_percentile)),3],
                        df[which.min(abs(50- df$Hispanic_percentile)),3],
                        df[which.min(abs(50- df$White_percentile)),3],
                        df[which.min(abs(50- df$Other_percentile)),3],
                        df[which.min(abs(75 - df$total_percentile)),3],
                        df[which.min(abs(75 - df$Asian_percentile)),3],
                        df[which.min(abs(75 - df$Black_percentile)),3],
                        df[which.min(abs(75 - df$Hispanic_percentile)),3],
                        df[which.min(abs(75 - df$White_percentile)),3],
                        df[which.min(abs(75 - df$Other_percentile)),3],
                        df[which.min(abs(90 - df$total_percentile)),3],
                        df[which.min(abs(90- df$Asian_percentile)),3],
                        df[which.min(abs(90- df$Black_percentile)),3],
                        df[which.min(abs(90- df$Hispanic_percentile)),3],
                        df[which.min(abs(90- df$White_percentile)),3],
                        df[which.min(abs(90- df$Other_percentile)),3],
                        df[which.min(abs(99 - df$total_percentile)),3],
                        df[which.min(abs(99 - df$Asian_percentile)),3],
                        df[which.min(abs(99 - df$Black_percentile)),3],
                        df[which.min(abs(99 - df$Hispanic_percentile)),3],
                        df[which.min(abs(99 - df$White_percentile)),3],
                        df[which.min(abs(99 - df$Other_percentile)),3]), byrow = T, ncol = 6) %>%
  as.data.frame()
colnames(percentiles) <- c("Total", "Asian", "Black", "Hispanic", "White", "Other")
rownames(percentiles) <- c('1%','10%','25%','50%','75%','90%', '99%')

#race ethnicity
box_plot_per <- percentiles %>%
  pivot_longer(Total:Other, names_to = "demographics", values_to = c("percentile")) %>%
  mutate(rank = rep(c('ex_low','low','b_quart','middle','u_quart','high', 'ex_high'), each = 6),
         demographics = as.factor(demographics)) %>%
  pivot_wider(names_from = rank, values_from = percentile) %>%
  mutate(ex_low = as.numeric(ex_low),
         low = as.numeric(low),
         b_quart = as.numeric(b_quart),
         middle = as.numeric(middle),
         u_quart = as.numeric(u_quart),
         high = as.numeric(high),
         ex_high = as.numeric(ex_high),
         demographics = fct_relevel(demographics, c("White", "Asian", "Total", "Black", "Hispanic", "Other"))) %>%
  as.data.frame()

#income
box_plot_per <- percentiles %>%
  pivot_longer(1:16, names_to = "income_cat", values_to = c("percentile")) %>%
  mutate(rank = rep(c('ex_low','low','b_quart','middle','u_quart','high', 'ex_high'), each = 16),
         income_cat = as.factor(income_cat)) %>%
  pivot_wider(names_from = rank, values_from = percentile) %>%
  mutate(ex_low = as.numeric(ex_low),
         low = as.numeric(low),
         b_quart = as.numeric(b_quart),
         middle = as.numeric(middle),
         u_quart = as.numeric(u_quart),
         high = as.numeric(high),
         ex_high = as.numeric(ex_high),
         income_cat = fct_relevel(income_cat, c("Less than $10,000", "$10,000 to $14,999", 
                                                "$15,000 to $19,999", "$20,000 to $24,999", 
                                                "$25,000 to $29,999", "$30,000 to $34,999",
                                                "35,000 to $39,999", "$40,000 to $44,999", 
                                                "$45,000 to $49,999", "$50,000 to $59,999", "$60,000 to $74,999", 
                                                "$75,000 to $99,999", "$100,000 to $124,999", "$125,000 to $149,999", 
                                                "$150,000 to $199,999", "$200,000 or more"))
         ) %>%
  as.data.frame()

ggplot(box_plot_per, aes(x = income_cat, 
                         ymin = ex_low, 
                         lower = b_quart, 
                         middle = middle, 
                         upper = u_quart, 
                         ymax = ex_high, 
                         fill = income_cat)) +
  geom_boxplot(stat = "identity") +
  #scale_fill_brewer(palette="Spectral") +
  labs(x = "Income Category", y = "Population-Weighted Dust Exposure", fill = "Income Category") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

#figure out how to add points to the box plot


# population weighted means and disparity measures ------------------------


# dust 2000  and 2010-------------------------------------------------------------

#popuation weighted annual average for 2000
annual_weighted <- annual_dust %>%
  filter(year == "2010") %>%
  mutate(n_total = Total*annual_dust,
         n_white = White*annual_dust,
         n_black = Black*annual_dust,
         n_hispanic = Hispanic*annual_dust,
         n_asian = Asian*annual_dust,
         n_AIAN = AIAN*annual_dust,
         n_NHPI = NHPI*annual_dust,
         n_other = Other*annual_dust)
 
total <- sum(annual_weighted$n_total, na.rm = TRUE)/sum(annual_weighted$Total)
white <- sum(annual_weighted$n_white, na.rm = TRUE)/sum(annual_weighted$White)
black <- sum(annual_weighted$n_black, na.rm = TRUE)/sum(annual_weighted$Black)
hispanic <- sum(annual_weighted$n_hispanic, na.rm = TRUE)/sum(annual_weighted$Hispanic)
asian <- sum(annual_weighted$n_asian, na.rm = TRUE)/sum(annual_weighted$Asian)
AIAN <- sum(annual_weighted$n_AIAN, na.rm = TRUE)/sum(annual_weighted$AIAN)
NHPI <- sum(annual_weighted$n_NHPI, na.rm = TRUE)/sum(annual_weighted$NHPI)
other <- sum(annual_weighted$n_other, na.rm = TRUE)/sum(annual_weighted$Other)


# income 2000 -------------------------------------------------------------


#for income
annual_weighted_income <- annual_dust_income %>%
  mutate(n_1 = GMX001*annual_dust,
         n_2 = GMX002*annual_dust,
         n_3 = GMX003*annual_dust,
         n_4 = GMX004*annual_dust,
         n_5 = GMX005*annual_dust,
         n_6 = GMX006*annual_dust,
         n_7 = GMX007*annual_dust,
         n_8 = GMX008*annual_dust,
         n_9 = GMX009*annual_dust,
         n_10 = GMX010*annual_dust,
         n_11 = GMX011*annual_dust,
         n_12 = GMX012*annual_dust,
         n_13 = GMX013*annual_dust,
         n_14 = GMX014*annual_dust,
         n_15 = GMX015*annual_dust,
         n_16 = GMX016*annual_dust)

p_1 <- sum(annual_weighted_income$n_1, na.rm = TRUE)/sum(annual_weighted_income$GMX001)
p_2 <- sum(annual_weighted_income$n_2, na.rm = TRUE)/sum(annual_weighted_income$GMX002)
p_3 <- sum(annual_weighted_income$n_3, na.rm = TRUE)/sum(annual_weighted_income$GMX003)
p_4 <- sum(annual_weighted_income$n_4, na.rm = TRUE)/sum(annual_weighted_income$GMX004)
p_5 <- sum(annual_weighted_income$n_5, na.rm = TRUE)/sum(annual_weighted_income$GMX005)
p_6 <- sum(annual_weighted_income$n_6, na.rm = TRUE)/sum(annual_weighted_income$GMX006)
p_7 <- sum(annual_weighted_income$n_7, na.rm = TRUE)/sum(annual_weighted_income$GMX007)
p_8 <- sum(annual_weighted_income$n_8, na.rm = TRUE)/sum(annual_weighted_income$GMX008)
p_9 <- sum(annual_weighted_income$n_9, na.rm = TRUE)/sum(annual_weighted_income$GMX009)
p_10 <- sum(annual_weighted_income$n_10, na.rm = TRUE)/sum(annual_weighted_income$GMX010)
p_11 <- sum(annual_weighted_income$n_11, na.rm = TRUE)/sum(annual_weighted_income$GMX011)
p_12 <- sum(annual_weighted_income$n_12, na.rm = TRUE)/sum(annual_weighted_income$GMX012)
p_13 <- sum(annual_weighted_income$n_13, na.rm = TRUE)/sum(annual_weighted_income$GMX013)
p_14 <- sum(annual_weighted_income$n_14, na.rm = TRUE)/sum(annual_weighted_income$GMX014)
p_15 <- sum(annual_weighted_income$n_15, na.rm = TRUE)/sum(annual_weighted_income$GMX015)
p_16 <- sum(annual_weighted_income$n_16, na.rm = TRUE)/sum(annual_weighted_income$GMX016)


#create df of population weighted means (race/eth)
pwms_2000 <- c(total, white, black, hispanic, asian, AIAN, NHPI, other)
pwms_2010 <- c(total, white, black, hispanic, asian, AIAN, NHPI, other)

pwms <- cbind.data.frame(pwms_2000, pwms_2010) %>%
  `rownames<-`(c(c("Total", "White", "Black", "Hispanic", "Asian", "AIAN", "NHPI", "Other")))

disparity_pwms <- pwms %>%
  mutate(abs_2000 = pwms_2000 - 0.7709053,
         rel_2000 = pwms_2000/0.7709053,
         abs_2010 = pwms_2010 - 0.6371809,
         rel_2010 = pwms_2010/0.6371809)

#create a nice table
stargazer::stargazer(disparity_pwms, type = "html", summary = FALSE, out = "disparity.htm")

#create df for income disparities
income_dis2000 <- c(p_1, p_2, p_3, p_4, p_5, p_6, p_7, p_8, p_9, 
                    p_10, p_11, p_12, p_13, p_14, p_15, p_16) %>%
  as.data.frame() %>%
  `row.names<-`(c("Less than $10,000", "$10,000 to $14,999", 
                  "$15,000 to $19,999", "$20,000 to $24,999", 
                  "$25,000 to $29,999", "$30,000 to $34,999",
                  "35,000 to $39,999", "$40,000 to $44,999", 
                  "$45,000 to $49,999", "$50,000 to $59,999", "$60,000 to $74,999", 
                  "$75,000 to $99,999", "$100,000 to $124,999", "$125,000 to $149,999", 
                  "$150,000 to $199,999", "$200,000 or more")) %>%
  `colnames<-`("PWM") %>%
  mutate(abs_2000 = PWM - 0.6528761,
         rel_2000 = PWM/0.6528761)

#create a nice table
stargazer::stargazer(income_dis2000, type = "html", summary = FALSE, out = "disparity_income.htm")



