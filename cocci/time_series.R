#time series analysis - Poisson
#at the county level

cocci_counties <- cocci_counties %>%
  mutate_at(vars(county, incarcerated), factor)

