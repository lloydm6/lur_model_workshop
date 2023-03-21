library(sf)
library(tmap)
library(tidyverse)

#######################################################
# READ ME ###
# uses all the landuse in environment (from dload_clean_compile_lu_data.R) and extracts the land use for the monitoring sites
# input: 
  # the extract land use functions
  # monitoring sites created by generate_monitoring_sites.R
  # land use R objects in the environment created by running dload_clean_compile_lu_data.R
# output:
  # csv of the land use around all the sites: to_sites_lu.csv
#######################################################

# functions #####

source('extract_lu_functions.R') # read in the functions
source('dload_clean_compile_lu_data.R') # download, clean, and compile land use data

# extract land use around sites ####

# read in the sites ###
to_sites <-
  read_csv("data/monitoring_data/to_monitoring_sites.csv") %>%
  st_as_sf(., coords = c("lon", "lat"), crs = 4326)

# transform to a crs that works with meters and preserves areas and distances
to_sites_utm <- 
  to_sites %>%
  st_transform(., crs = 32617)

# create an sf object of 500 m buffers around each site
to_sites_500m_buff <- 
  to_sites_utm %>%
  st_buffer(., dist = 500)

# create an sf object of 200 m buffers around each site
to_sites_200m_buff <- 
  to_sites_utm %>%
  st_buffer(., dist = 200)

# we could also use other buffer distances. to keep it simple, we are just using 500m and 200m

# use the functions from extract_lu_functions.R to get the land use within the 500 m buffers
to_sites_lu_500m <- 
  area_near_site_fn(to_sites_500m_buff, to_greenspace_sf, "green_500m") %>%
  area_near_site_fn(., to_open_sf, "open_500m") %>%
  area_near_site_fn(., to_com_sf, "commercial_500m") %>%
  area_near_site_fn(., to_ind_sf, "industrial_500m") %>%
  area_near_site_fn(., to_res_sf, "residential_500m") %>%
  length_near_site_fn(., to_hwy_sf, "highway_500m") %>%
  length_near_site_fn(., to_majroad_sf, "majroad_500m") %>%
  length_near_site_fn(., to_road_sf, "road_500m") %>%
  length_near_site_fn(., to_busroustes_sf, "busroutes_500m") %>%
  length_near_site_fn(., to_railway_sf, "rail_500m") %>%
  number_near_site_fn(., to_npri_pm, "npri_pm_500m") %>%
  number_near_site_fn(., to_npri_nox, "npri_nox_500m") %>%
  number_near_site_fn(., to_busstops_sf, "busstop_500m") %>%
  distance_to_site_fn(., to_shore_sf, "distance_shore") %>%  # the distance_to_site_fn with give the same answer regardless of buffer size
  distance_to_site_fn(., to_airport, "distance_airport") %>%
  distance_to_site_fn(., to_port, "distance_port") %>%
  distance_to_site_fn(., to_trst_sf, "distance_trxstn") %>%
  distance_to_site_fn(., to_npri_pm %>% st_union(), "distance_npri_pm") %>%
  distance_to_site_fn(., to_npri_nox %>% st_union(), "distance_npri_nox")

# repeat for the 200 m buffer
to_sites_lu_200m <- 
  area_near_site_fn(to_sites_200m_buff, to_greenspace_sf, "green_200m") %>%
  area_near_site_fn(., to_open_sf, "open_200m") %>%
  area_near_site_fn(., to_com_sf, "commercial_200m") %>%
  area_near_site_fn(., to_ind_sf, "industrial_200m") %>%
  area_near_site_fn(., to_res_sf, "residential_200m") %>%
  length_near_site_fn(., to_hwy_sf, "highway_200m") %>%
  length_near_site_fn(., to_majroad_sf, "majroad_200m") %>%
  length_near_site_fn(., to_road_sf, "road_200m") %>%
  length_near_site_fn(., to_busroustes_sf, "busroutes_200m") %>%
  length_near_site_fn(., to_railway_sf, "rail_200m") %>%
  number_near_site_fn(., to_npri_pm, "npri_pm_200m") %>%
  number_near_site_fn(., to_npri_nox, "npri_nox_200m") %>%
  number_near_site_fn(., to_busstops_sf, "busstop_200m")


to_sites_lu <- 
  to_sites_lu_500m %>%
  st_centroid() %>% # once we have the landuse data, we don't need the buffer anymore, just get the centroid, which is the site
  st_transform(., crs = 4326) %>% 
  mutate(lon = st_coordinates(.)[,1], lat = st_coordinates(.)[,2]) %>% # we want to get the latitude and longitude of each site. We could have also done a join with the site data
  st_set_geometry(NULL) %>% # we only want a csv file. we can later use st_as_sf() as we need
  left_join(to_sites_lu_200m %>% st_set_geometry(NULL), .) %>%
  arrange(site_id)

write_csv(to_sites_lu, "data/land_use_data/to_sites_lu.csv") # save the data

to_sites_lu_sf <- 
  to_sites_lu %>%
  st_as_sf(., coords = c("lon", "lat"), crs = 4326, remove = F)

tm_shape(to_greenspace_sf) +
  tm_polygons("darkgreen") +
  tm_shape(to_sites_lu_sf %>% st_transform(., crs = 32617) %>% st_buffer(., dist = 500)) +
  tm_dots() +
  tm_text("green_500m")


