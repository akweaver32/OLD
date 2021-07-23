#time series analysis - Poisson
#at the county level

#NEED: county level wind and soil data 

#read in impervious surface by county and simplify
head(ImpSurf_countiesCA)
imp_surface <- ImpSurf_countiesCA %>%
  select("NAME", "AvgImpvSurfPct") %>%
  mutate(NAME = as_factor(NAME), AvgImpvSurfPct = as.numeric(AvgImpvSurfPct)) %>%
  rename(county = NAME, imp_surf_pct = AvgImpvSurfPct)
#join with cocci_counties data
test <- left_join(cocci_counties, imp_surface, by = "county")

#ppt = avg precipitation in milimeters
county_precip <- df_CA_county_PRISM_ppt %>%
  select(c("NAME", "date", "ppt")) %>%
  rename(county = NAME)
#join with test_poisson
test_poisson2 <- left_join(test_poisson, county_precip, by = c("county", "date"))

#dummy Poisson model
test_poisson <- test %>%
  group_by(date, county) %>%
  summarise(count = n()) %>%
  left_join(imp_surface, by = "county")

m1 <- glm(count ~ imp_surf_pct + ppt, family = "poisson", data = test_poisson2)
summary(m1)

