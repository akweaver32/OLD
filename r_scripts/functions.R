#functions

#write a function to convert lat long to ca counties
lonlat_to_county <- function(ll_df, 
                         map, 
                         name_col = "name") {
  ## Convert points data.frame to an sf POINTS object
  pts <- st_as_sf(ll_df, coords = c("long", "lat"), crs = 4326)
  
  ## Transform spatial data to some planar coordinate system
  ## (e.g. Web Mercator) as required for geometric operations
  county <- st_transform(map, crs = 3857)
  pts <- st_transform(pts, crs = 3857)
  
  ## Find names of county (if any) intersected by each point
  county_names <- county[[name_col]]
  ii <- as.integer(st_intersects(pts, county))
  county_names[ii]
  return(cbind(ll_df, county_names[ii]))
}

lonlat_to_state <- function(pointsDF,
                            states = spData::us_states,
                            name_col = "NAME") {
  ## Convert points data.frame to an sf POINTS object
  pts <- st_as_sf(pointsDF, coords = c("long", "lat"), crs = 4326)
  
  ## Transform spatial data to some planar coordinate system
  ## (e.g. Web Mercator) as required for geometric operations
  states <- st_transform(states, crs = 3857)
  pts <- st_transform(pts, crs = 3857)
  
  ## Find names of state (if any) intersected by each point
  state_names <- states[[name_col]]
  ii <- as.integer(st_intersects(pts, states))
  state_names[ii]
}

