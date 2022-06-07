#basic figures

#basic figures for cocci dust project
packages_figures <- c("tidyverse", "lubridate", "maps", "rgeos", "sf", "stars", "raster", "gganimate")
lapply(packages_figures, library, character.only = TRUE)


# county case mapping -----------------------------------------------------
endemic_counties_X <- c("WKern", "EKern", "WFresno", "WTulare", "San Luis Obispo", "Ventura", "Kings", "Monterey", "San Joaquin", "NLA", "Merced", "Stanislaus", "Santa Barbara", "WMadera")

#CREATE STUDY REGION FIGURE
counties_split <- read_sf("shapefiles/split_counties") %>%
  filter(county1 %in% endemic_counties_X) #endemic_counties_X is the ones Jen used

ggplot() +
  geom_sf(counties_split, mapping = aes(geometry = geometry, fill = county1)) +
  geom_sf(CDE_data, mapping = aes(geometry = geometry), fill = NA, color = "black", size = 0.05) +
  labs(fill = "County") +
  theme_bw()


CDE_data %>%
  group_by(county, OnsetYear) %>%
  dplyr::summarise(cases = sum(N_all)) %>%
  left_join(counties, by = c("county" = "NAME")) %>%
  ggplot() +
  geom_sf(mapping = aes(geometry = geometry, fill = log(cases))) +
  facet_wrap(~OnsetYear) +
  scale_fill_viridis_c()

CDE_data %>%
  group_by(GEOID, OnsetYear) %>%
  dplyr::summarise(cases = sum(N_all)) %>%
  left_join(CT_sf, by = "GEOID") %>%
  ggplot() +
  geom_sf(mapping = aes(geometry = geometry, fill = log(cases)), size = 0.05) +
  facet_wrap(~OnsetYear) +
  scale_fill_viridis_c()

# dust seasonality --------------------------------------------------------

ggplot(splines_explore, aes(date, dust)) +
  geom_point() +
  geom_smooth(span = 0.055, se = F) +
  facet_wrap(~county)


# case seasonality --------------------------------------------------------

CDE_data %>%
  group_by(county, OnsetYear, OnsetMonth) %>%
  summarise(cases = sum(N_all)) %>%
  mutate(date = make_date(OnsetYear, OnsetMonth)) %>%
  ggplot(aes(date, cases)) +
  geom_point(size = 0.3) +
  facet_wrap(~county)


# population density plot -------------------------------------------------

#plot the census tracts by pop dense categories
CDE_data_pop %>%
  ggplot(mapping = aes(geometry = geometry.y, fill = decile_pop, color = decile_pop)) +
  geom_sf() +
  coord_sf(crs = 3488)

