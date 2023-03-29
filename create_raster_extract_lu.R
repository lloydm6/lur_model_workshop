
library(sf)
library(raster)
library(tmap)
library(stars)
library(tidyverse)

###############################################################################
# READ ME ###
# creates a raster of the study area, the raster will become the prediction surface
# uses all the landuse in environment (from dload_clean_compile_lu_data.R) and extracts the land use for the entire study area
##### THIS IS A LONG RUNNER - CODE IS COMMENTED OUT, UNCOMMENT TO USE ##################
# input: 
  # the extract land use functions
  # .rds of study area (sf object) saved by generate_monitoring_sites.R
  # land use R objects in the environment created by running dload_clean_compile_lu_data.R
  # monitoring sites created by generate_monitoring_sites.R (this is just for a check at the end)
# output:
  # csv of the land use for every cell in the raster: to_fishnet_lu.csv
  # empty raster as an .rds file (empty = only cell id numbers that link to to_fishnet_lu.csv)
###############################################################################

# functions #####

source('extract_lu_functions.R')
# source('dload_clean_compile_lu_data.R') #we've already gone through this

# get area
to_border_sf <- 
  readRDS("data/study_area/to_border_sf.rds")

to_border_sf_utm <- 
  to_border_sf %>%
  st_transform(., crs = 32617)

# create raster of study area
st_bbox(to_border_sf_utm)
to_raster_limits <- st_bbox(to_border_sf_utm %>% st_buffer(., dist = 500))
to_raster_limits[1]
to_raster <- raster(ncol = 85, nrow = 50, # i guessed at how many 500 m cells would be in the bbox
                    xmn = to_raster_limits[1], 
                    xmx = to_raster_limits[3], 
                    ymn = to_raster_limits[2], 
                    ymx = to_raster_limits[4])
# one change one place?

res(to_raster)  # we see the resolution isn't exactly 500 m x 500 m
res(to_raster) <- 500 # set it to 500 m x 500 m 
projection(to_raster) <- "+proj=utm +zone=17 +datum=WGS84" # we forgot to set the crs when creating the raster, so we can do it here
raster::values(to_raster) <- 1:ncell(to_raster) # give a cell id to each cell. this will be usefull later. 
names(to_raster) <- "cell_id"  # the default is "layer", set the cell id name to cell_id

tm_shape(to_border_sf_utm) +
  tm_polygons("purple", alpha = 0.5) +
  tm_shape(to_raster) +
  tm_raster(alpha = 0.5)

# saveRDS(to_raster, "data/study_area/to_raster.rds")

# we can transform our raster to vector geography. That is what we'll do in order to extract land use data around each cell. 
to_fishnet <- st_as_sf(st_as_stars(to_raster), as_points = F, merge = T)

to_fishnet_500m <- st_buffer(to_fishnet, dist = 500)
to_fishnet_200m <- st_buffer(to_fishnet, dist = 200)
# 
# # This takes quite a while, only want to do it once
# to_fishnet_500m_lu <-
#   area_near_site_fn(to_fishnet_500m, to_greenspace_sf, "green_500m") %>%
#   area_near_site_fn(., to_open_sf, "open_500m") %>%
#   area_near_site_fn(., to_com_sf, "commercial_500m") %>%
#   area_near_site_fn(., to_ind_sf, "industrial_500m") %>%
#   area_near_site_fn(., to_res_sf, "residential_500m") %>%
#   length_near_site_fn(., to_hwy_sf, "highway_500m") %>%
#   length_near_site_fn(., to_majroad_sf, "majroad_500m") %>%
#   length_near_site_fn(., to_road_sf, "road_500m") %>%
#   length_near_site_fn(., to_busroustes_sf, "busroutes_500m") %>%
#   length_near_site_fn(., to_railway_sf, "rail_500m") %>%
#   number_near_site_fn(., to_npri_pm, "npri_pm_500m") %>%
#   number_near_site_fn(., to_npri_nox, "npri_nox_500m") %>%
#   number_near_site_fn(., to_busstops_sf, "busstop_500m") %>%
#   distance_to_site_fn(., to_shore_sf, "distance_shore") %>%
#   distance_to_site_fn(., to_airport, "distance_airport") %>%
#   distance_to_site_fn(., to_port, "distance_port") %>%
#   distance_to_site_fn(., to_trst_sf, "distance_trxstn") %>%
#   distance_to_site_fn(., to_npri_pm %>% st_union(), "distance_npri_pm") %>%
#   distance_to_site_fn(., to_npri_nox %>% st_union(), "distance_npri_nox")
# 
# to_fishnet_200m_lu <-
#   area_near_site_fn(to_fishnet_200m, to_greenspace_sf, "green_200m") %>%
#   area_near_site_fn(., to_open_sf, "open_200m") %>%
#   area_near_site_fn(., to_com_sf, "commercial_200m") %>%
#   area_near_site_fn(., to_ind_sf, "industrial_200m") %>%
#   area_near_site_fn(., to_res_sf, "residential_200m") %>%
#   length_near_site_fn(., to_hwy_sf, "highway_200m") %>%
#   length_near_site_fn(., to_majroad_sf, "majroad_200m") %>%
#   length_near_site_fn(., to_road_sf, "road_200m") %>%
#   length_near_site_fn(., to_busroustes_sf, "busroutes_200m") %>%
#   length_near_site_fn(., to_railway_sf, "rail_200m") %>%
#   number_near_site_fn(., to_npri_pm, "npri_pm_200m") %>%
#   number_near_site_fn(., to_npri_nox, "npri_nox_200m") %>%
#   number_near_site_fn(., to_busstops_sf, "busstop_200m")
# 
# to_fishnet_lu <-
#   to_fishnet_500m_lu %>%
#   st_centroid() %>%
#   st_transform(., crs = 4326) %>%
#   mutate(lon = st_coordinates(.)[,1], lat = st_coordinates(.)[,2]) %>%
#   st_set_geometry(NULL) %>%
#   left_join(to_fishnet_200m_lu %>% st_set_geometry(NULL), .) %>%
#   arrange(layer)
# 
# write_csv(to_fishnet_lu, "data/land_use_data/to_fishnet_lu.csv")


