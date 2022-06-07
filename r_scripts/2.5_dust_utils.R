## Script name: dust_utils.R
##
## Purpose of script: Helper functions for dust analysis
##

check_path <- function(path) { 
  # Raise error if path does not exist 
  if ((length(path) == 0) || (!file.exists(path))) { 
    stop(paste0("The path ",path," does not exist")) 
  } 
  else { return(path) }
}

read_wustl <- function(WUSTL_FOLDER, year, month, geom) { 
  # Read in WUSTL data as a raster for a given year and month, and crop to input geometry 
  # WUSTL_FOLDER (char): path to folder containing WUSTL data 
  # year (char): year to grab data for 
  # month (char): month to grab data for 
  # geom (sf): geometry to crop raster to
  
  wildcard <- paste0(WUSTL_FOLDER ,"/GWRwSPEC_SOIL_NA_", year, month,"_", year, month,".nc") # Pattern matching
  filepath <- Sys.glob(wildcard) # Get file path 
  if (length(filepath) == 0) { stop(paste0("File with wildcard ",wildcard," does not exist")) } # Raise error if file doesn't exist
  
  wustl_raster <- raster(filepath) %>% # Read in raster 
    raster::crop(geom) %>% 
    raster::mask(geom) 
  return(wustl_raster)
}

read_centralValley <- function(shapefilePath) {
  # Read in CA shapefile. Restrict to CA cocci endemic areas and convert to crs=4326 
  
  #endemic_counties_X <- c("Kern", "Fresno", "Tulare", "San Luis Obispo", "Ventura", "Kings", "Monterey", 
                          #"San Joaquin", "Los Angeles", "Merced", "Stanislaus", "Santa Barbara", "Madera")
  endC <- c("029", "019", "107", "079", "111", "031", "053", "077", "037", "047", "099", "083", "039") #these but in FIPS codes
  
  ca_tract <- sf::read_sf(shapefilePath) %>% # Read in shapefile 
    st_set_crs("EPSG:3857") %>% 
    dplyr::filter(COUNTYFP %in% endC) %>% # Just get census tracts in the Central Valley
    dplyr::select(NAME, COUNTYFP, geometry) # Select columns of interest
  ca_tract$geometry <- sf::st_transform(ca_tract$geometry, crs=4326) # Convert geometry to 4326 CRS
  return(ca_tract)
}
 
#double check about the st_crs vs. st_transform
read_census <- function(shapefilePath) {
  endC <- c("029", "019", "107", "079", "111", "031", "053", "077", "037", "047", "099", "083", "039") #these but in FIPS codes
  ca_tract <- sf::read_sf(shapefilePath) %>%
    st_set_crs("ESPG:3857") %>%
    dplyr::select(COUNTYFP, GEOID, NAME, geometry) %>%
    dplyr::filter(COUNTYFP %in% endC)
  st_crs(ca_tract$geometry) <- 4326
  return(ca_tract)
}


mean_dust_wustl_polys <- function(wustl_raster, census_raster){ 
  # COMPUTE MEAN DUST CONCENTRATON FOR A SPECIFIC MONTH BY CENSUS TRACT
  
  # Get coverage area by polygon 
  mean_dust_extracted <- exactextractr::exact_extract(wustl_raster, # Raster data
                                                      census_tracts, # Polygons to extract raster to 
                                                      'mean', # Get mean for census tract
                                                      progress=TRUE, # Show progress bar
                                                      force_df = TRUE) 
  # bind to census tract information                                                     
  dust_census_results <- cbind(mean_dust_extracted, census_tracts$GEOID, 
                               month = months[i], year = as.factor(year)) %>%
    dplyr::rename("dust_mean" = "mean",
                  "GEOID" = "census_tracts$GEOID")# Bind list of data frames
  
  return(dust_census_results)
}

md_wustl_buffers <- function(wustl_raster, buffers) {
  #perform four extractions
  mean_dust_extracted_1k <- exactextractr::exact_extract(wustl_raster,
                                                      monitor_locations$buffer_1k,
                                                      'mean',
                                                      progress = TRUE,
                                                      force_df = TRUE)
  
  mean_dust_extracted_3k <- exactextractr::exact_extract(wustl_raster,
                                                         monitor_locations$buffer_3k,
                                                         'mean',
                                                         progress = TRUE,
                                                         force_df = TRUE)
  
  mean_dust_extracted_5k <- exactextractr::exact_extract(wustl_raster,
                                                         monitor_locations$buffer_5k,
                                                         'mean',
                                                         progress = TRUE,
                                                         force_df = TRUE)
  
  mean_dust_extracted_10k <- exactextractr::exact_extract(wustl_raster,
                                                         monitor_locations$buffer_10k,
                                                         'mean',
                                                         progress = TRUE,
                                                         force_df = TRUE)
  
  #bind the results together
  dust_buffers_results <- cbind(mean_dust_extracted_1k, mean_dust_extracted_3k, 
                                mean_dust_extracted_5k, mean_dust_extracted_10k, 
                                buffers[1], month = months[i], year = as.factor(year)) %>%
    as.data.frame()
  
}



# nicole's functions ------------------------------------------------------


time_elapsed_pretty <- function(start, end) {
  # Print time elapsed in pretty format 
  dsec <- as.numeric(difftime(end, start, unit = "secs"))
  hours <- floor(dsec / 3600)
  minutes <- floor((dsec - 3600 * hours) / 60)
  seconds <- dsec - 3600*hours - 60*minutes
  paste0(
    sapply(c(hours, minutes, seconds), function(x) {
      formatC(x, width = 2, format = "d", flag = "0")
    }), collapse = ":")
}

wustl_raster_analysis <- function(wustl_raster, counties) { 
  # Convert raster to dataframe of points and determine which county each point (pixel) is in 
  # Code modified from Stack Exchange response from user dof1985
  # https://gis.stackexchange.com/questions/282750/identify-polygon-containing-point-with-r-sf-package
  
  wustl_df <- rasterToPoints(wustl_raster) %>% # Convert raster to data frame of coordinates
    as.data.frame
  pnts_sf <- do.call("st_sfc",c(lapply(1:nrow(wustl_df),  function(i) { # Convert each lat, lon point to sf Point object with a CRS 
    st_point(as.numeric(wustl_df[i,1:2])) 
  }), list("crs" = 4326))) %>% 
    st_transform(2163) # Transform to planar 
  counties_trans <- st_transform(counties, 2163) # Transform to planar 
  wustl_df$COUNTY <- apply(st_intersects(counties_trans, pnts_sf, sparse = FALSE), 2, # Check which county the point intersects with 
                           function(col) {counties[which(col), ]$NAME})
  df_results <- wustl_df %>% # Beautify final dataframe output 
    dplyr::rename(longitude = x, latitude = y) %>% # Rename columns to longitude, latitude 
    cbind(pixel_ID = as.numeric(rownames(wustl_df)))  # Get pixel ID from dataframe index 
  df_results <- df_results[,c("pixel_ID","COUNTY","latitude","longitude",names(wustl_raster))] # Reorder columns 
  return(df_results)
}

