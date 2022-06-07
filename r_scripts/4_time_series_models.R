#running different models for dust analysis
#Amanda Weaver
#need to run time_series.R before this script to have CT_sf

#libraries and set up
packages_models <- c("tidyverse", "dlnm", "splines", "data.table", "sandwich", 
                     "miceadds", "tsModel", "lubridate", "splines", "glmmTMB", 
                     "bbmle", "stargazer", "sjPlot", "DHARMa", "broom.mixed", "dotwhisker", "sf", "lme4", "glmmTMB")
lapply(packages_models, library, character.only = TRUE)

endemic_counties <- c("Kern", "Fresno", "Tulare", "San Luis Obispo", "Ventura", "Kings", "Monterey", 
                      "San Joaquin", "Los Angeles", "Merced",  "Stanislaus", "Santa Barbara","Madera")

endemic_counties_X <- c("WKern", "EKern", "WFresno", "WTulare", "San Luis Obispo", "Ventura", "Kings", "Monterey", 
                        "San Joaquin", "NLA", "Merced", "Stanislaus", "Santa Barbara", "WMadera")

#read in base dust data and counties split
CDE_data <- read_csv("base_dust_data_CLEAN_FINAL.csv") %>%
  left_join(select(CT_sf, -area_km), by = "GEOID") %>% #fix the geometry back again
  select(-geometry.x) %>%
  rename("geometry" = "geometry.y") %>%
  arrange(pop_dense) %>% #create the population density percentiles
  mutate(pop_tile = ntile(pop_dense, 50))

#split counties read in for mapping
counties_split <- read_sf("shapefiles/split_counties") %>%
  filter(county1 %in% endemic_counties_X) #filter out to the study region

#get the GEOIDs from split counties
split_GEOID <- read_csv("GEOIDS_in_Divided_Counties.csv") %>%
  dplyr::filter(county1 %in% endemic_counties_X) %>% #just the split counties
  mutate(GEOID = substring(GEOID, 2), GEOID = as.numeric(GEOID))

# urbanicity plots --------------------------------------------------------
#drop by percentile of urbanicity (some census tracts are missing?)
CDE_data %>%
  ggplot() + 
  geom_sf(mapping = aes(geometry = geometry, fill = pop_tile), size = 0.05) +
  geom_sf(counties_split, mapping = aes(geometry = geometry), fill = NA, size = 0.2, color = "red") +
  coord_sf() +
  scale_fill_viridis_c() +
  labs(fill = "Population Density") +
  theme_bw()

#plot the included census tracts
CDE_data %>%
  dplyr::filter(pop_tile < 20) %>%
  ggplot() + 
  geom_sf(mapping = aes(geometry = geometry, fill = pop_tile), size = 0.05) +
  geom_sf(counties_split, mapping = aes(geometry = geometry), fill = NA, size = 0.2, color = "red") +
  scale_fill_viridis_c(limits = c(0,50)) + #keep the limits the same as the previous plot
  labs(fill = "Population Density") +
  theme_bw()



# data subsetting ----------------------------------------------------------

#actual dropping (bottom 40%)
no_urban <- CDE_data %>%
  filter(pop_tile < 20) %>% #bottom 40% of census tract density
  select(-pop_tile) %>%
  left_join(select(split_GEOID, - ...1), by = "GEOID") %>% #join with the split counties
  rename("split_county" = "county1")

# data shape --------------------------------------------------------------

#look at distribution of data
hist(CDE_data$dust_mean) #a bit skewed
hist(CDE_data$winter_total) #a bit skewed
hist(CDE_data$summer_mean) #weird
hist(CDE_data$pop_dense) #very skewed
hist(no_urban$pop_dense) #still skewed

# basic visualization -----------------------------------------------------

#mean annual cases what census tracts have the highest burden? doesn't look overly patchy
CDE_data %>%
  dplyr::group_by(GEOID, OnsetYear) %>%
  dplyr::summarise(annual_cases = sum(N_all)) %>%
  dplyr::group_by(GEOID) %>%
  dplyr::summarise(mean = mean(annual_cases)) %>%
  left_join(CT_sf, by = "GEOID") %>%
  ggplot(mapping = aes(geometry = geometry, fill = log(mean))) +
  geom_sf(size = 0.05) +
  coord_sf(crs = 3488) +
  scale_fill_viridis_c()

# explore spline for seasonality ------------------------------------------

#best to do it based on the pollution time series
#spline exploration of dust data
CDE_data %>% 
  dplyr::group_by(county, time) %>%
  dplyr::summarise(mean_dust = mean(dust_mean, na.rm = TRUE)) %>%
  ggplot(aes(x = time, y = mean_dust)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ splines::ns(x, 3*18), se = FALSE, col = "red") +
  geom_smooth(method = "lm", se = FALSE, formula = y ~ splines::ns(x, 2), col = "blue") +
  facet_wrap(~county) +
  ylab("Monthly Mean Dust Concentration") +
  xlab("Date") +
  labs(title = "3 knots per year for seasonality and 2 knots total for multi-year trend")

no_urban %>% 
  dplyr::group_by(county, time) %>%
  dplyr::summarise(mean_dust = mean(dust_mean, na.rm = TRUE)) %>%
  ggplot(aes(x = time, y = mean_dust)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ splines::ns(x, 3*18), se = FALSE, col = "red") +
  geom_smooth(method = "lm", se = FALSE, formula = y ~ splines::ns(x, 2), col = "blue") +
  facet_wrap(~county) +
  ylab("Monthly Mean Dust Concentration") +
  xlab("Date") +
  labs(title = "3 knots per year for seasonality and 2 knots total for multi-year trend", subtitle = "No urban census tracts")

#spline exploration for case data
CDE_data %>% 
  dplyr::group_by(county, time) %>%
  dplyr::summarise(case_count = sum(N_all, na.rm = TRUE)) %>%
  ggplot(aes(x = time, y = case_count)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ splines::ns(x, 2*18), se = FALSE, col = "red") +
  geom_smooth(method = "lm", se = FALSE, formula = y ~ splines::ns(x, 2), col = "blue") +
  facet_wrap(~county, scales = "free_y") +
  ylab("Monthly Case Count") +
  xlab("Date") +
  labs(title = "2 knots per year for seasonality and 2 knots total for multi-year trend", subtitle = "*y-axis vary")

no_urban %>% 
  dplyr::group_by(county, time) %>%
  dplyr::summarise(case_count = sum(N_all, na.rm = TRUE)) %>%
  ggplot(aes(x = time, y = case_count)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ splines::ns(x, 2*18), se = FALSE, col = "red") +
  geom_smooth(method = "lm", se = FALSE, formula = y ~ splines::ns(x, 2), col = "blue") +
  facet_wrap(~county, scales = "free_y") +
  ylab("Monthly Case Count") +
  xlab("Date") +
  labs(title = "2 knots per year for seasonality and 2 knots total for multi-year trend", subtitle = "*y-axis vary, no urban census tracts")

# mixed effects modeling --------------------------------------------------

#mixed effects modeling (all NB2 and in no_urban data)
#starting with dust lag 2 (Didn't converge)
m_2 <- glmmTMB(N_all ~ offset(log(TotalPopulation+1)) + 
                 scale(dust_lag2) + #exposure of interest
                 ns(time, knots=3*18) +  # splines for time, allow these effects to be non-linear
                 ns(lat) + ns(long) +
                 (1|split_county), # random effect for county location
               family = "nbinom2",
               data = no_urban)

m_23 <- update(m_2, ~ . + scale(dust_lag3))
m_234 <- update(m_23, ~ . + scale(dust_lag4))

#with full dataset
m_2F <- update(m_2, ~ . , data = CDE_data) #didn't converge
m_23F <- update(m_23, ~ . , data = CDE_data)
m_234F <- update(m_234, ~ . , data = CDE_data)


# negative controls -------------------------------------------------------

#create lags into the future
neg_controls <- no_urban %>%
  group_by(GEOID) %>% #for proper creation of lags
  arrange(time) %>%
  mutate(dust_lead1 = lead(dust_mean, 1, default = NA),
         dust_lead2 = lead(dust_mean, 2, default = NA))

#run models with future dust as the main exposure 
NC_1 <- glmmTMB(N_all ~ offset(log(TotalPopulation+1)) +
                  scale(dust_lead1) +
                  ns(time, knots = 3*18) +
                  ns(lat) + ns(long) +
                  (1|split_county),
                family = "nbinom2",
                data = neg_controls)

NC_23 <- update(NC_2, ~ . + scale(dust_lag3))
NC_234 <- upadate(NC_23, ~ . + scale(dust_lag4))

NC_1c <- update(NC_1, ~ . + scale(PercentBlack) + scale(PercentFilipino) + scale(PercentHispanic))


# other main effect characterizations -------------------------------------
#starting with dust lag 1 (doesn't converge)
m_1 <- glmmTMB(N_all ~ offset(log(TotalPopulation+1)) + 
                 scale(dust_lag1) + #exposure of interest
                 ns(time, knots=3*18) +  # splines for time, allow these effects to be non-linear
                 ns(lat) + ns(long) +
                 (1|split_county), # random effect for census tract location
               family = "nbinom2",
               data = no_urban)

m_12 <- update(m_1, ~ . + scale(dust_lag2))
m_123 <- update(m_1, ~ . + scale(dust_lag2) + scale(dust_lag3)) #doesn't converge
m_1234 <- update(m_1, ~ . + scale(dust_lag2) + scale(dust_lag3) + scale(dust_lag4))

#with full dataset
m_1F <- update(m_1, ~ . , data = CDE_data_full)
m_12F <- update(m_12, ~ . , data = CDE_data_full)
m_123F <- update(m_123, ~ . , data = CDE_data_full)
m_1234F <- update(m_1234, ~ . , data = CDE_data_full)


#cumulative 1 and 2
m_cum <- glmmTMB(N_all ~ offset(log(TotalPopulation+1)) + 
                          scale(cum_12)  + #exposure of interest
                          ns(time, knots=3*18) +  # splines for time, allow these effects to be non-linear
                          (1|split_county) + # random effect for county location
                          ns(lat) + ns(long),
                   family = "nbinom2",
                   data = no_urban)

m_cum_3 <- update(m_cum, ~ . + scale(dust_lag3))
m_cum_34 <- update(m_cum_3, ~ . + scale(dust_lag4))

#updating in full data set
m_cumF <- update(m_cum, ~ . , data = CDE_data_full)
m_cum_3F <- update(m_cum_3, ~ . , data = CDE_data_full)
m_cum_34F <- update(m_cum_34, ~ . , data = CDE_data_full)


#with random intercept and random slope
m_cum_34_R <- glmmTMB(N_all ~ offset(log(TotalPopulation+1)) + 
                        scale(cum_12)  + #exposure of interest
                        + scale(dust_lag3) + scale(dust_lag4) + 
                        ns(time, knots=3*18) +  # splines for time, allow these effects to be non-linear
                        (cum_12|county) + # random effect for county location and dust (both intercept and slope)
                        ns(lat) + ns(long),
                      family = "nbinom2",
                      data = no_urban)

#plot the RE structure
plot_model(m_cum_34, type = "re", title = "Random Effects Estimates - Cumulative Exposure Model")

# model comparisons -------------------------------------------------------

#compare models
AICtab(m_cum, m_cum_3, m_cum_34, m_2, m_23, m_234, m_1, m_12, m_123, m_1234, base = TRUE)
AICtab(m_cumF, m_cum_3F, m_cum_34F, m_2F, m_23F, m_234F, m_1F, m_12F, m_123F,  m_1234F, base = TRUE)

#visualize the model coefficients
plot_model(m_cum_34, type = "est", terms = c("cum_12", "dust_lag3", "dust_lag4"))

#create nice table for slides (m cumulative and 1234 and 234)
tab_model(m_cum_34, m_1234, m_234, file = "results.htm", p.style = "stars")
tab_model(m_cum_34F, m_1234F, m_234F, file = "results_FD.htm", p.style = "stars")

#with zero inflated based on GEOID
model_mixed_ZI <- glmmTMB(N_all ~ offset(log(TotalPopulation+1)) + 
                            scale(cum_12) + #exposure of interest
                            ns(time, knots=3*18) +  # splines for time, allow these effects to be non-linear
                            ns(lat) + ns(long) +
                            (1|county), # random effect for county location
                          zi = ~GEOID, #zero inflation based on GEOID
                          family = "poisson",
                          data = no_urban)

#use DHARMa to look at residuals (maybe okay)
simulationOutput <- simulateResiduals(fittedModel = m_cum_34, plot = F)
plot(simulationOutput)
testOutliers(type = "bootstrap", simulationOutput = simulationOutput)
testDispersion(simulationOutput)
testZeroInflation(simulationOutput)


# interaction modeling and visualization ----------------------------------
#with interactions for season
m_cum_month <- update(m_cum, ~ . + scale(cum_12)*dust_month)
m_cum_season <- update(m_cum_34, ~ . + scale(cum_12)*dust_season)
m_2_season <- update(m_234, ~ . + scale(dust_lag2)*dust_season)

#with county interactions
m_cum_county <- update(m_cum_34, ~ . + scale(cum_12)*county)
m_2_county <- update(m_234, ~ . + scale(dust_lag2)*county)
m_2_county_split <- update(m_2,  ~ . + scale(dust_lag2)*county1, data = (no_urban_split))

m_2_county_split_v2 <- glmmTMB(N_all ~ offset(log(TotalPopulation+1)) + 
                                    scale(dust_lag2)  + #exposure of interest
                                    ns(time, knots=3*18) +  # splines for time 
                                    ns(lat) + ns(long) + county1 + scale(dust_lag2)*county1,
                                  family = "nbinom2",
                                  data = no_urban_split)
  
#calculate the confidence intervals for each season 
#seasonal modification
K <- rbind(c(0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0), #SPRING
           c(0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1), #WINTER
           c(0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), #FALL
           c(0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0)) #SUMMER
           
#county modification (reference is EKern)
C <- rbind(c(0, 1, 0, 0, 0, 0, 
             0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
             0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), #EKern 
           
           c(0, 1, 0, 0, 0, 0, 
             0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
             1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), #Kings 
           
           c(0, 1, 0, 0, 0, 0, 
             0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
             0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), #Merced  
           
           c(0, 1, 0, 0, 0, 0, 
             0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
             0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), #Monterey  
           
           c(0, 1, 0, 0, 0, 0, 
             0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
             0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0), #North Los Angeles  
           
           c(0, 1, 0, 0, 0, 0, 
             0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
             0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0), #San Joaquin
           
           c(0, 1, 0, 0, 0, 0, 
             0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
             0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0), #SLO 
           
           c(0, 1, 0, 0, 0, 0, 
             0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
             0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0), #Santa Barbara  
           
           c(0, 1, 0, 0, 0, 0, 
             0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
             0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0), #Stanislaus
           
           c(0, 1, 0, 0, 0, 0, 
             0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
             0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0), #Ventura
           
           c(0, 1, 0, 0, 0, 0, 
             0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
             0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0), #Fresno
           
           c(0, 1, 0, 0, 0, 0, 
             0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
             0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0), #West Kern 
           
           c(0, 1, 0, 0, 0, 0, 
             0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
             0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0), #Madera  
           
           c(0, 1, 0, 0, 0, 0, 
             0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
             0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1)) #Tulare  
           
           
comp <- glht(m_2_season, linfct = K)
comp_v2 <- glht(m_2_county_split_v2, linfct = C)

#for seasons
plot_multcomp <- function(obj, ...){
  ci_out <- confint(obj, ...)
  ci_df <- as.data.frame(exp(ci_out$confint))
  ci_df$group <- c("1", "2", "3", "4")
  ggplot2::ggplot(ci_df) +
    ggplot2::aes(x = Estimate, y = group, color = group) +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_errorbar(aes(xmin = lwr, xmax = upr), width = 0.2, size = 1) +
    ggplot2::geom_vline(xintercept = 1, linetype = 2) +
    ggplot2::labs(x = "IRR", y = "Season", color = "Season") +
  scale_color_hue(labels = c("Summer", "Fall", "Winter", "Spring"))
}

#for counties
plot_multcomp_counties <- function(obj, ...){
  ci_out <- confint(obj, ...)
  ci_df <- as.data.frame(exp(ci_out$confint))
  ci_df$group <- counties
  ggplot2::ggplot(ci_df) +
    ggplot2::aes(x = Estimate, y = group, color = group) +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_errorbar(aes(xmin = lwr, xmax = upr), width = 0.2, size = 1) +
    ggplot2::geom_vline(xintercept = 1, linetype = 2) +
    ggplot2::labs(x = "IRR", y = "County", color = "County")
}

counties <- c ("EKern", "Kings", "Merced", "Monterey", "NLA",  "San Joaquin", 
               "San Luis Obispo","Santa Barbara",  "Stanislaus", "Ventura", "WFresno",
               "WKern", "WMadera", "WTulare")

CIs <- confint(comp)
CIs_df <- as.data.frame(exp(CIs$confint))
CIs_df <- as.data.frame(exp(CIs$confint)) %>%
  cbind.data.frame(counties)
CIs_df %>%
  arrange(Estimate)
CIs_df

plot_multcomp(comp)
plot_multcomp_counties(comp_v2)

#make a map
split_counties <- read_sf("shapefiles/split_counties") %>%
  filter(county1 %in% endemic_counties_X) %>%
  left_join(CIs_df, by = c("county1" = "counties") )

ggplot() +
  geom_sf(split_counties, color = "black", size = 0.25,
          mapping = aes(geometry = geometry, fill = Estimate)) +
  scale_fill_viridis_c() +
  labs(fill = "IRR Estimate")



# plotting models ---------------------------------------------------

#interaction plots
mylist <- list(dust_lag2 = seq(0,4,by=0.5), dust_season = c("Summer","Spring", "Fall", "Winter"))
emmip(model5, dust_season ~ dust_lag2, at=mylist, CIs=TRUE, xlab = "Dust lagged 2 months", type = "response")

#coefficient plots
m_cum_34_tidy <- broom.mixed::tidy(m_cum_34, exponentiate = TRUE)
m_1234_tidy <- broom.mixed::tidy(m_1234, exponentiate = TRUE)
m_234_tidy <- broom.mixed::tidy(m_234, exponentiate = TRUE)

m_cum_34F_tidy <- broom.mixed::tidy(m_cum_34F, exponentiate = TRUE)
m_1234F_tidy <- broom.mixed::tidy(m_1234F, exponentiate = TRUE)
m_234F_tidy <- broom.mixed::tidy(m_234F, exponentiate = TRUE)

  
tidy_models_NU <- rbind(m_cum_34_tidy, m_1234_tidy, m_234_tidy) %>%
  mutate(model = rep(c("Cumulative Model", "Dust Lag 1 Model", "Dust Lag 2 Model"), 
                     times = c(9, 10, 9)))

tidy_models_F <- rbind(m_cum_34F_tidy, m_1234F_tidy, m_234F_tidy) %>%
  mutate(model = rep(c("Cumulative Model", "Dust Lag 1 Model", "Dust Lag 2 Model"), 
                     times = c(9, 10, 9)))

#create dot whisker plot
dwplot(tidy_models_NU,
       vline = geom_vline(
         xintercept = 1,
         colour = "grey60",
         linetype = 2),
       vars_order = c("scale(cum_12)", "scale(dust_lag1)", "scale(dust_lag2)", "scale(dust_lag3)", "scale(dust_lag4)"),
       model_order = c("Cumulative Model", "Dust Lag 1 Model", "Dust Lag 2 Model"),
       ci_method = "wald") %>%
       relabel_predictors(c(
           `scale(dust_lag1)` = "Dust Lagged 1 Months",
           `scale(dust_lag2)` = "Dust Lagged 2 Months",
           `scale(dust_lag3)` = "Dust Lagged 3 Months",
           `scale(dust_lag4)` = "Dust Lagged 4 Months",
           `scale(cum_12)` = "Cumulative Dust 1 and 2")) +
  theme_bw() +
  theme(
    legend.position = c(0.007, 0.01),
    legend.justification = c(0, 0),
    legend.background = element_rect(colour = "grey80"),
    legend.title.align = .5
  ) +
  xlab("IRR Estimate and 95% CI") + 
  ylab("") +
  scale_x_continuous(breaks = seq(-1, 2.5, by = 0.05)) +
  theme(legend.title = element_blank()) +
  ggtitle("Negative Binomial Model Coefficients")

#look at predictions
data10$predsm <- predict(modelsm, type = "response")

model_predict3 <- no_urban %>%
  dplyr::filter_at(vars(dust_lag2), all_vars(!is.na(.))) 

model_predict3$pred <- predict(model3, type = "response")

#create prediction plot
model_predict3 %>%
  group_by(GEOID, time) %>%
  summarize(total_pred = sum(pred), total_out = sum(N_all)) %>%
  ggplot() +
  geom_point(aes(x = time, y = total_out)) +
  geom_point(aes(x = time, y = total_pred), color = "red") +
  facet_wrap(~GEOID, scales = "free_y")

# look at spatial autocorrelation -----------------------------------------

#LINEAR MODELS
#get the residuals from the model
model_resid <- no_urban %>%
  dplyr::filter_at(vars(cum_12, dust_lag3, lat, long), all_vars(!is.na(.))) %>%
  mutate(residuals = m_cum_34$residuals) %>%
  group_by(GEOID) %>%
  dplyr::mutate(mean_res = mean(residuals, na.rm =)) %>%
  ungroup()

#look at them spatially to see if spatial autocorellation remains
model_resid %>% 
  ggplot() +
  geom_sf(mapping = aes(geometry = geometry, fill = mean_res), lwd = 0) +
  scale_fill_viridis_c("Residuals", na.value = "grey")+
  theme_grey(base_size = 14) +
  theme(legend.position="bottom", legend.box = "horizontal")


# non-linear modeling -----------------------------------------------------

#define the center for your exposure of interest (current month mean dust), 25th percentile
cen <- quantile(CDE_data_full$cum_12, probs = 0.25, na.rm = TRUE)
cen_no.urban <- quantile(no_urban$cum_12, probs = 0.25, na.rm = TRUE)
cen_2_NU <- quantile(no_urban$dust_lag2, probs = 0.25, na.rm = TRUE)

#define the range that the spline will be created over (dust)
bound <- range(CDE_data_full$cum_12, na.rm = TRUE)
bound_no.urban <- range(no_urban$cum_12, na.rm = TRUE)
bound_2_NU <- range(no_urban$dust_lag2, na.rm = TRUE)

#create the onebasis for your exposure of interest
splinevar <- onebasis(no_urban$cum_12, knots = c(1,3, 6), 
                      bound=bound_no.urban, cen=cen_no.urban) #cumulative

splinevar <- onebasis(no_urban$dust_lag2, knots = c(2, 4), 
                      bound=bound_2_NU, cen=cen_2_NU) #lagged two months



#run the model
#no lags
NL_cum24 <- glmer.nb(N_all ~ offset(log(TotalPopulation+1)) + 
                      splinevar  + #exposure of interest
                       + scale(dust_lag3) + scale(dust_lag4) +
                      ns(time, knots=3*18) +  # splines for time, allow these effects to be non-linear
                      (1|county) + # random effect for county location
                      ns(lat) + ns(long),
                    data = no_urban)
  
NL_234 <- glmer.nb(N_all ~ offset(log(TotalPopulation+1)) + 
                       splinevar  + #exposure of interest
                       + scale(dust_lag3) + scale(dust_lag4) +
                       ns(time, knots=3*18) +  # splines for time, allow these effects to be non-linear
                       (1|county) + # random effect for county location
                       ns(lat) + ns(long),
                     data = no_urban)  

## Use cross pred to predict the outcome while varying the exposure of interest
predsplinevar <- crosspred(splinevar, NL_234, cen = cen_2_NU)

## Put results into a dataframe. exponentiate bc used poisson distribution
df.res <- as.data.frame(cbind("IRR" = predsplinevar$allRRfit,
                              "LCI" = predsplinevar$allRRlow,
                              "UCI" = predsplinevar$allRRhigh,
                              "x"   = predsplinevar$predvar))

# plot results -------

#plot shows gam as well as histogram of exposure
# multiply density by 15 to adjust for scale (may need to modify for your purposes)
ggplot(df.res) +
  geom_line(aes(x = x, y = IRR)) + 
  geom_ribbon(aes(x = x, ymin = UCI, ymax = LCI), alpha = 0.2) +
  geom_histogram(data = no_urban, mapping = aes(x = dust_mean, y = ..density..*4), fill = "darkcyan", alpha = 0.2) +
  theme_bw() +
  xlab("Lagged Dust (ug/m3)") +
  labs(title = "Dust Lagged Exposure-Response", subtitle = "Lag Two Months") +
  ylim(0,4) +
  xlim(0,4.55) + #10 for the full the size, 5 for most of the dust support
  geom_hline(yintercept = 1, color = "red") +
  geom_point(aes(x=cen_2_NU, y=1), colour="blue") +
  geom_rug(sides = "b", inherit.aes = FALSE, data = no_urban, mapping = aes(x = dust_lag2), size = 0.05)


# ----- Step 5: Extract IRRs for a one IQR increase

#75th and 95th percentile of mean dust
per75 <- quantile(no_urban$dust_lag2, probs = 0.75, na.rm = TRUE) 
per95 <- quantile(no_urban$dust_lag2, probs = 0.95, na.rm = TRUE) 
per98 <- quantile(no_urban$dust_lag2, probs = 0.99, na.rm = TRUE)

# find which value of x is closest to the value for the 75th percentile and extract IRR and 95% CI
df.res[which.min(abs(df.res$x-per75)),][1:3]
#find for 95th
df.res[which.min(abs(df.res$x-per95)),][1:3]
#98th
df.res[which.min(abs(df.res$x-per98)),][1:3]
#max
df.res[which.min(abs(df.res$x-3.5)),][1:3]
#max
df.res[which.min(abs(df.res$x-1.3)),][1:3]


# unconstrained DLNM --------------------------------------------------------

#CDE data should be ordered correctly can also just replace with the no urban dataset
#create the cross basis terms 
dust_DLNM <- no_urban %>%
  group_by(GEOID) %>%
  arrange(time) %>%
  as.data.frame()

dust_basis <- dlnm::crossbasis(dust_DLNM$dust_mean, lag = 6,
                         argvar=list(fun = "lin"),
                         arglag=list(fun="poly", degree = 3),
                         group = dust_DLNM$GEOID)

#does this need to have an adjustment for year too? model doesn't converge if so
model_basis <- glmer.nb(N_all ~ offset(log(TotalPopulation+1)) + 
                          dust_basis  + #exposure of interest
                          ns(time, knots=3*18) +  # splines for time, allow these effects to be non-linear
                          (1|county) + # random effect for county location
                          ns(lat) + ns(long),
                        data = dust_DLNM)

summary(model_basis)

pred_basis <- crosspred(dust_basis, model_basis, cen = 0.5)

#IRR for a 1 mg/m3 increase in dust exposure 
plot(pred_basis, "slices", var = 1, col=3, ylab="IRR", ci.arg=list(density=15,lwd=3), 
     main="Lag-response curve for a 1-unit increase in dust (IQR) w/ spline month, 80th pop dense")

plot(pred_basis, "slices", var=1, ci="bars", ylab="IRR", type="p", col=2, pch=19,
     ci.level=0.95, main="Lag-response a 1-unit increase (95CI)")

#look at residuals
plot(model_basis)

