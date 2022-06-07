#comparison between IMPROVE monitors and WashU dust product
#Amanda K. Weaver

#libraries
packages_IMPROVE <- c("tidyverse", "lubridate", "maps", "rgeos", "sf", "stars", "raster", "gganimate", "ncdf4", "readxl", "tidygeocoder", "vroom")
lapply(packages_IMPROVE, library, character.only = TRUE)

# reading in data ---------------------------------------------------------

#available monitors with downloaded data
available_monitors <- c("AGTI1", "BLIS1", "DEVA1", "DOME1", "FRES1", "JOSH1", "KAIS1", "LAVO1", "LTCC1", "OWVL1", "PINN1", "PORE1", "RAFA1", "REDW1", "RUBI1", "SAGA1", "SAGO1", "SEQU1", "TRIN1", "WRIG1", "YOSE1", "YOSEX")

#IMPROVE monitor locations
monitor_locations <- read_xlsx("IMPROVE/IMPROVE_Sites_Active_and_Inactive_Updated_February_2022.xlsx") %>%
  filter(ST == "CA") %>% #keep only the monitors in California
  st_as_sf(coords = c("Lon", "Lat"), crs = 4326) %>% #create geom points from lat long of monitors
  filter(SiteCode %in% available_monitors) %>% #remove monitors with no lat long
  slice(-23) %>% #remove the duplicate Yosemite monitor
  mutate(buffer_1k = st_buffer(geometry, 1000), 
         buffer_3k = st_buffer(geometry, 3000), #create the buffers for dust product extraction
         buffer_5k = st_buffer(geometry, 5000),
         buffer_10k = st_buffer(geometry, 10000))
  
#list all the files and download the data into R
IMPROVE_files <- list.files(path = "IMPROVE", pattern = ".txt", full.names = TRUE)
A <- read.delim(IMPROVE_files[1], header = TRUE, sep = "|")
B <- read.delim(IMPROVE_files[2], header = TRUE, sep = "|")
C <- read.delim(IMPROVE_files[3], header = TRUE, sep = "|")
#D <- read.delim(IMPROVE_files[4], header = TRUE, sep = "|") #DOLA has no observations
E <- read.delim(IMPROVE_files[5], header = TRUE, sep = "|")
G <- read.delim(IMPROVE_files[6], header = TRUE, sep = "|")
#H <- read.delim(IMPROVE_files[7], header = TRUE, sep = "|") #won't read in right, fixed below
I <- read.delim("IMPROVE/JOSH2.txt", header = TRUE, sep = "|") #No JOSH2
J <- read.delim(IMPROVE_files[9], header = TRUE, sep = "|")
#K <- read.delim(IMPROVE_files[10], header = TRUE, sep = "|") #No lat long
L <- read.delim(IMPROVE_files[11], header = TRUE, sep = "|")
M <- read.delim(IMPROVE_files[12], header = TRUE, sep = "|")
N <- read.delim(IMPROVE_files[13], header = TRUE, sep = "|")
O <- read.delim(IMPROVE_files[14], header = TRUE, sep = "|")
P <- read.delim(IMPROVE_files[15], header = TRUE, sep = "|")
Q <- read.delim(IMPROVE_files[16], header = TRUE, sep = "|")
R <- read.delim(IMPROVE_files[17], header = TRUE, sep = "|")
S <- read.delim(IMPROVE_files[18], header = TRUE, sep = "|")
U <- read.delim(IMPROVE_files[19], header = TRUE, sep = "|")
V <- read.delim(IMPROVE_files[20], header = TRUE, sep = "|")
W <- read.delim(IMPROVE_files[21], header = TRUE, sep = "|")
#X <- read.delim(IMPROVE_files[22], header = TRUE, sep = "|") #SOLA has no observations
Y <- read.delim(IMPROVE_files[23], header = TRUE, sep = "|")
Z <- read.delim(IMPROVE_files[24], header = TRUE, sep = "|")
Aa <- read.delim(IMPROVE_files[25], header = TRUE, sep = "|")
Bb <- read.delim(IMPROVE_files[26], header = TRUE, sep = "|")

#combine and remove
IMPROVE_all <- rbind(A, B, C, E, G, I, J, L, M, N, O, P, Q, R, S, U, V, W, Y, Z, Aa, Bb)
rm(A, B, C, E, G, I, J, L, M, N, O, P, Q, R, S, U, V, W, Y, Z, Aa, Bb)

#run linear regression of Iron and Soil
m1 <- lm(SOIL_CIRA ~ FE + SiteCode, data = dust_IMPROVE)
slope <- m1$coefficients[2]
slope <- 15


# calculating dust --------------------------------------------------------

#iron corrected dust
dust_IMPROVE <- IMPROVE_all %>%
  as_tibble() %>%
  pivot_wider(names_from = ParamCode, values_from = FactValue) %>%
  filter_at(vars(FE, SOIL_CIRA), any_vars(!is.na(.))) %>%
  mutate(FactDate = as.Date(FactDate, "%m/%d/%Y")) %>%
  arrange(FactDate) %>%
  group_by(FactDate, SiteCode) %>%
  mutate(FE = sum(FE, na.rm = T), SOIL_CIRA = sum(SOIL_CIRA, na.rm = T)) %>%
  dplyr::ungroup() %>%
  filter_at(vars(Unc, Mdl), any_vars(!is.na(.)))%>%
  mutate(DUST = slope*FE) %>% #do this separately then run the whole chunk (or just run 15)
  group_by(month = month(FactDate), SiteCode) %>%
  summarise(month_DUST_IMPROVE = mean(DUST))

#SOIL_CIRA
soil_IMPROVE <- IMPROVE_all %>%
  as_tibble() %>%
  pivot_wider(names_from = ParamCode, values_from = FactValue) %>%
  filter_at(vars(FE, SOIL_CIRA), any_vars(!is.na(.))) %>%
  mutate(FactDate = as.Date(FactDate, "%m/%d/%Y")) %>%
  arrange(FactDate) %>%
  group_by(FactDate, SiteCode) %>%
  mutate(FE = sum(FE, na.rm = T), SOIL_CIRA = sum(SOIL_CIRA, na.rm = T)) %>%
  dplyr::ungroup() %>%
  filter_at(vars(Unc, Mdl), any_vars(!is.na(.))) %>%
  group_by(month = month(FactDate), SiteCode) %>%
  summarise(month_soil = mean(SOIL_CIRA))

dust_TS <- IMPROVE_all %>%
  as_tibble() %>%
  pivot_wider(names_from = ParamCode, values_from = FactValue) %>%
  filter_at(vars(FE, SOIL_CIRA), any_vars(!is.na(.))) %>%
  mutate(FactDate = as.Date(FactDate, "%m/%d/%Y")) %>%
  arrange(FactDate) %>%
  group_by(FactDate, SiteCode) %>%
  mutate(FE = sum(FE, na.rm = T), SOIL_CIRA = sum(SOIL_CIRA, na.rm = T)) %>%
  dplyr::ungroup() %>%
  filter_at(vars(Unc, Mdl), any_vars(!is.na(.))) %>%
  mutate(DUST = slope*FE, time = floor_date(unit = "month", FactDate)) %>%
  group_by(time, SiteCode) %>%
  summarise(mean_dust = mean(DUST))

#california counties
CA_counties <- read_sf("shapefiles/CA_counties") %>%
  st_transform(4326)

# basic plots -------------------------------------------------------------

#plot locations of IMPROVE monitors in the state
ggplot() +
  geom_sf(CA_counties, mapping = aes(geometry = geometry)) +
  geom_sf(monitor_locations, mapping = aes(geometry = geometry), size = 3, shape = 23, fill = "cyan") +
  labs(title = "Available IMPROVE monitors")

#plot the monthly means for all available stations
ggplot(dust_IMPROVE, aes(x = month, y = month_DUST_IMPROVE)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = 1:12) +
  facet_wrap(~SiteCode) +
  labs(title = "Monthly Dust Means (iron corrected) by IMPROVE Monitor Location", x = "Month", y = "Monthly Dust Concentration")
  
ggplot(dust_TS, aes(x = time, y = mean_dust)) +
  #geom_point() +
  geom_line() +
  facet_wrap(~SiteCode, scales = "free_y") +
  labs(title = "Monthly Dust Means (iron corrected) by IMPROVE Monitor Location", x = "Time", y = "Monthly Dust Concentration")


# dust extractions --------------------------------------------------------

#read in california shape
CA <- read_sf("shapefiles/ca-state-boundary") %>%
  st_transform(4326)

#create storage vectors
year <- 2017 #need to do for 2000-2017
months <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
names <- paste0("wustl_", months) 
results <- paste0("wustl_results_", months)

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

#perform analysis on each
for (i in 1:12) {
  assign(results[i], md_wustl_buffers(wustl_raster = rasters[[i]],
                                           buffers = monitor_locations))
}

MD2017_buffers <- rbind.data.frame(wustl_results_01, wustl_results_02, 
                           wustl_results_03, wustl_results_04, 
                           wustl_results_05, wustl_results_06, 
                           wustl_results_07, wustl_results_08, 
                           wustl_results_09, wustl_results_10, 
                           wustl_results_11, wustl_results_12)

#bind them together into one year .csv file (CHANGE THIS PART)
dust_buffers_all <- rbind.data.frame(MD2000_buffers, MD2001_buffers, MD2002_buffers, MD2003_buffers, MD2004_buffers, MD2005_buffers, 
                                     MD2006_buffers, MD2007_buffers, MD2008_buffers, MD2009_buffers, MD2010_buffers, MD2011_buffers,
                                     MD2012_buffers, MD2013_buffers, MD2014_buffers, MD2015_buffers, MD2016_buffers, MD2017_buffers)

#rename the means columns
colnames(dust_buffers_all) <- c("mean1", "mean3", "mean5", "mean10", "SiteCode", "geometry", "month", "year")

#save the .csv file
write_csv(dust_buffers_all, "MD_buffers_WUSTL_0017.csv")

dust_buffers_all <- read_csv("MD_buffers_WUSTL_0017.csv")


# do the actual comparison ------------------------------------------------

dust_comparison <- dust_buffers_all %>%
  mutate(year_num = rep(2000:2017, each = 264), time = make_date(year_num, month)) %>%
  left_join(dust_TS, by = c("time", "SiteCode"))

dust_monthly_comparison <- dust_comparison %>%
  group_by(month(time), SiteCode) %>%
  summarise(mean_improve = mean(mean_dust, na.rm = T), 
            mean1b = mean(mean1, na.rm = T),
            mean3b = mean(mean3, na.rm = T),
            mean5b = mean(mean5, na.rm = T),
            mean10b = mean(mean10, na.rm = T)) %>%
  rename("month" = "month(time)")

#plot for entire time series
ggplot(dust_comparison, aes(x = time, y = mean_dust)) +
  geom_line() +
  #geom_line(aes(x = time, y = mean1), col = "red") +
  #geom_line(aes(x = time, y = mean3), col = "red") +
  #geom_line(aes(x = time, y = mean5), col = "red") +
  geom_line(aes(x = time, y = mean10), col = "red") +
  facet_wrap(~SiteCode, scales = "free_y") +
  labs(title = "Monthly Dust Means (iron corrected) by IMPROVE Monitor Location", x = "Time", y = "Monthly Dust Concentration", subtitle = "10 kilometer buffer for WashU data (red)")

#plot for seasonal trends
ggplot(dust_monthly_comparison, aes(x = month, y = mean_improve)) +
  geom_line() +
  #geom_line(aes(x = month, y = mean1b), col = "blue") +
  #geom_line(aes(x = month, y = mean3b), col = "blue") +
  #geom_line(aes(x = month, y = mean5b), col = "blue") +
  geom_line(aes(x = month, y = mean10b), col = "blue") +
  facet_wrap(~SiteCode, scales = "free_y") +
  scale_x_continuous(breaks = 1:12) +
  labs(title = "Monthly Dust Means (iron corrected) by IMPROVE Monitor Location", x = "Time", y = "Monthly Dust Concentration", subtitle = "10 kilometer buffer for WashU data (blue")


# correlations ------------------------------------------------------------

#correlations for entire data set
cor(dust_comparison$mean1, dust_comparison$mean_dust, use = "complete.obs")
cor(dust_comparison$mean3, dust_comparison$mean_dust, use = "complete.obs")
cor(dust_comparison$mean5, dust_comparison$mean_dust, use = "complete.obs")
cor(dust_comparison$mean10, dust_comparison$mean_dust, use = "complete.obs")

#monthly correlations
cor(dust_monthly_comparison$mean1b, dust_monthly_comparison$mean_improve, use = "complete.obs")
cor(dust_monthly_comparison$mean3b, dust_monthly_comparison$mean_improve, use = "complete.obs")
cor(dust_monthly_comparison$mean5b, dust_monthly_comparison$mean_improve, use = "complete.obs")
cor(dust_monthly_comparison$mean10b, dust_monthly_comparison$mean_improve, use = "complete.obs")

#removing the upper quartile
no_extreme_dust <- dust_comparison %>%
  filter(mean10 <= quantile(mean10, 0.25)) #%>%
  filter(SiteCode %in% c("PINN1", "DOME1", "KAIS1", "FRES1", "RAFA1", "SEQU1"))
  
#correlations for bottome 75% of mean 10k data set
cor(no_extreme_dust$mean1, no_extreme_dust$mean_dust, use = "complete.obs")
cor(no_extreme_dust$mean3, no_extreme_dust$mean_dust, use = "complete.obs")
cor(no_extreme_dust$mean5, no_extreme_dust$mean_dust, use = "complete.obs")
cor(no_extreme_dust$mean10, no_extreme_dust$mean_dust, use = "complete.obs")
  
#removing the lower quartile
extreme_dust <- dust_comparison %>%
  filter(mean_dust >= quantile(mean_dust, 0.75, na.rm = TRUE))

#correlations for upper 75% of mean 10k data set
cor(extreme_dust$mean1, extreme_dust$mean_dust, use = "complete.obs")
cor(extreme_dust$mean3, extreme_dust$mean_dust, use = "complete.obs")
cor(extreme_dust$mean5, extreme_dust$mean_dust, use = "complete.obs")
cor(extreme_dust$mean10, extreme_dust$mean_dust, use = "complete.obs")

#plot the lower values
ggplot(extreme_dust, aes(x = time, y = mean_dust)) +
  geom_line() +
  #geom_line(aes(x = time, y = mean1), col = "red") +
  #geom_line(aes(x = time, y = mean3), col = "red") +
  #geom_line(aes(x = time, y = mean5), col = "red") +
  geom_line(aes(x = time, y = mean10), col = "red") +
  facet_wrap(~SiteCode, scales = "free_y") +
  labs(title = "Monthly Dust Means (iron corrected) by IMPROVE Monitor Location", x = "Time", y = "Monthly Dust Concentration", subtitle = "10 kilometer buffer for WashU data (red), upper 25% of data")


# anomaly analysis --------------------------------------------------------

#take the average of the months and subtract from each month then do the comparison over again
dust_monthly_comparison$month <- as.character(dust_monthly_comparison$month)
dust_comparison$month <- as.character(rep(1:12, each = 396))

anomalies <- dust_comparison %>%
  left_join(dust_monthly_comparison, by = c("month", "SiteCode")) %>%
  mutate(deviation_IMPROVE = mean_dust - mean_improve,
         deviation1 = mean1 - mean1b,
         deviation3 = mean3 - mean3b,
         deviation5 = mean5 - mean5b,
         deviation10 = mean10 - mean10b)

#correlations
cor(anomalies$deviation1, anomalies$deviation_IMPROVE, use = "complete.obs")
cor(anomalies$deviation3, anomalies$deviation_IMPROVE, use = "complete.obs")
cor(anomalies$deviation5, anomalies$deviation_IMPROVE, use = "complete.obs")
cor(anomalies$deviation10, anomalies$deviation_IMPROVE, use = "complete.obs")

#plot the deviations
anomalies %>%
  #filter(SiteCode == "BLIS1") %>%
  ggplot( aes(x = time, y = deviation_IMPROVE)) +
  #geom_point(size = 0.2) +
  geom_line() +
  #geom_line(aes(x = time, y = deviation1), col = "red", size = 0.2) +
  #geom_line(aes(x = time, y = mean3), col = "red") +
  #geom_line(aes(x = time, y = mean5), col = "red") +
  geom_line(aes(x = time, y = deviation10), col = "red", size = 0.2) +
  facet_wrap(~SiteCode, scales = "free_y") +
  labs(title = "Deviation in Dust Means (iron corrected) by IMPROVE Monitor Location", x = "Time", y = " Monthly Difference in Dust Concentration", subtitle = "10 kilometer buffer for WashU data (red)")




# quartiles for sites -----------------------------------------------------

quartiles <- dust_comparison %>%
  group_by(SiteCode) %>%
  summarise(q25 = quantile(mean_dust, 0.25, na.rm = TRUE),
            q50 = quantile(mean_dust, 0.50, na.rm = TRUE),
            q75 = quantile(mean_dust, 0.75, na.rm = TRUE))

stargazer(quartiles, type = "html", out = "test.doc")

