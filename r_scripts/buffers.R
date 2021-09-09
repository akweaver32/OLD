#buffers and masking out non dust areas

#check duplicate lat long points
ll_sp <- SpatialPoints(lat_long)
ll_duplicates <- remove.duplicates(ll_sp) #12216 duplicates?

#project cocci points
ll_sf <- st_as_sf(lat_long, coords = c("long", "lat"), crs = 4326) %>%
  st_transform(crs = 3488) #units are meters

#create buffer of 26k and 10k, 16 mi (rough avg commute in for USA)
#26k assumes straight line commute
#10k a little bigger than izzy but might account for dust movement
buffer_26k <- st_buffer(ll_sf, 25800)
buffer_10k <- st_buffer(ll_sf, 10000) #3488 from ll_sf
buffer_10k$geometry <- st_transform(buffer_10k$geometry, crs = 4326)

#bind to cocci data (double check that the order is not changed, shouldn't be)
cases_buffer <- cbind.data.frame(cases, buffer_10k) %>%
  mutate(CID = as.numeric(CID))

#go to cc_adapt.R to join to case control

