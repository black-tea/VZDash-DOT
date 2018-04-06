library(sf)
library(dplyr)
library(httr)

# MO Codes
ped_inv_codes = '3003'
bike_inv_codes = c('3008', '3016', '3017', '3018')
mc_inv_codes = c('3009', '3013', '3014', '3015')
hit_and_run_codes = c('3029', '3030')

# Current Plan: Download only 2018 data (so there is not too much in memory)
# Next steps: Download entire dataset by (1) get length and (2) set limit to length and (3) save to Spatialite & query Spatialite
# sqlite would be better when I setup a container

# Access socrata api for collisions
# Metadata URL https://data.lacity.org/api/views/metadata/v1/k8cc-2d49

# Step 1: need to access the socrata API to get the length - number of records
response <- GET('https://data.lacity.org/resource/k8cc-2d49.json?$select=count(dr_no)')
record_ct <- content(response, 'parsed')[[1]][[1]]

# Step 2: make the request
#request = paste0('https://data.lacity.org/resource/k8cc-2d49.geojson?$limit=',record_ct)
#collisions = read_sf(request)

# Step 1&2 (current): Query for collisions since this year, with no limit
request = paste0('https://data.lacity.org/resource/k8cc-2d49.geojson?$where=date_extract_y(date_occ)=2018&$limit=',record_ct)
collisions = read_sf(request)

# Step 3: Clean MO Codes
collisions <- collisions %>%
  # rowwise() makes sure that the mutate operations don't use data from the entire df
  rowwise() %>%
  mutate(severity = case_when(grepl('3027',mocodes) ~ 1,
                              grepl('3024',mocodes) ~ 2,
                              grepl('3025',mocodes) ~ 3,
                              grepl('3026',mocodes) ~ 4,
                              grepl('3028',mocodes) ~ 0)) %>%
  mutate(mode = case_when(any(sapply(ped_inv_codes, grepl, mocodes)) ~ 'Ped',
                          any(sapply(bike_inv_codes, grepl, mocodes)) ~ 'Bike',
                          any(sapply(mc_inv_codes, grepl, mocodes)) ~ 'MC')) %>%
  mutate(hit_and_run = ifelse(any(sapply(hit_and_run_codes, grepl, mocodes)),'Y',NA)) %>%
  # remove rowwise operation
  ungroup() %>%
  # recast as sf object
  st_sf()

# Export to geojson (future will dump to sqlite + spatailite)
#write_sf(collisions, 'data/lapd_collisions/collisions.geojson')
st_write(collisions, 'data/lapd_collisions/collisions.geojson',delete_dsn = TRUE)

