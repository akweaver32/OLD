#time series analysis - Poisson
#hexagon grid

#NEED: county level wind and soil data 
CA_sf <- st_as_sf(CA) %>%
  st_transform(crs = 3488)
hex_10k <- st_make_grid(CA_sf, cellsize = 20 * 1000, crs = 3488, what = "polygons", square = FALSE) %>% #20 because diameter
  st_as_sf() %>%
  st_intersection(CA_sf)

#plot
ggplot() +
  geom_sf(counties_sf, mapping = aes(geometry = geometry)) +
  geom_sf(hex_10k, mapping = aes(geometry = x), color = "black") +
  geom_sf(ll_sf, mapping = aes(geometry = geometry), color = "red", size = 0.1) +
  coord_sf(crs = 3488)

ggsave("ca_hex_grid_10k.png")

ggplot() + 
  geom_sf(CA_sf, mapping = aes(geometry = geometry), color = "black") +
  coord_sf(crs = 3488)
  
  
plot(CA_sf)

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

