library(tidytransit)
library(purrr)
library(data.table)
library(osmdata)
library(sf)
library(tmap)
library(tidyverse)

#######################################################
# READ ME ###
# this file downloads, cleans, and compiles a bunch of land use parameters for 
# input: 
  # .rds of study area (sf object) saved by generate_monitoring_sites.R
  # links to various land use data sources
# output:
  # R objects in the environment for use in extract_lu.R (I didn't bother saving them, but I may do it later)
#######################################################


# read in our study area #####
to_border_sf <- readRDS("data/study_area/to_border_sf.rds")

# data from the city of Toronto open data ###### 
# sometimes you are given nice clean databases of land use
# other times you might need find it for yourself. It's best to have help with a subject matter expert, but you may have to make due with what you can find. 
# greenspace
temp <- tempfile()
download.file(url = "https://ckan0.cf.opendata.inter.prod-toronto.ca/dataset/9a284a84-b9ff-484b-9e30-82f22c1780b9/resource/7a26629c-b642-4093-b33c-a5a21e4f3d22/download/Green%20Spaces.geojson", 
              destfile = temp)

to_greenspace_sf <- 
  st_read(temp) %>% # read in the .shp file using st_read()
  st_union() %>% # unify all the simple features into one simple feature
  st_transform(., crs = 32617) # transform into a projected crs, this will allow us to work in meters

# why use st_union? the original .shp file has a row for each greenspace and many columns of information
# we don't really care about the names of parks and we aren't treating them as separate entities
# st_union will make one single speacial feature with all the parks 

tmap_mode("view")
tm_shape(to_greenspace_sf) + 
  tm_polygons("green")

# "centerline" file?
# upon inspection it is a bunch of "lines" such as roads, the shore, and railroads 
temp <- tempfile()
download.file(url = "https://ckan0.cf.opendata.inter.prod-toronto.ca/dataset/1d079757-377b-4564-82df-eb5638583bfb/resource/7bc94ccf-7bcf-4a7d-88b1-bdfc8ec5aaf1/download/Centreline%20-%20Version%202.geojson", 
              destfile = temp)
centerline_sf <- 
  st_read(temp) %>%
  st_transform(., crs = 32617)

centerline_sf$FEATURE_CODE_DESC %>% unique()

tm_shape(centerline_sf %>% filter(FEATURE_CODE_DESC == "Collector")) + 
  tm_lines("purple") +
  tm_shape(centerline_sf %>% filter(FEATURE_CODE_DESC == "Major Arterial")) + 
  tm_lines("darkgreen")

to_rail_sf <- 
  centerline_sf %>% 
  filter(FEATURE_CODE_DESC == "Major Railway") %>%
  st_union()

to_shore_sf <- 
  centerline_sf %>% 
  filter(FEATURE_CODE_DESC == "Major Shoreline") %>%
  st_union()

to_hwy_sf <- 
  centerline_sf %>% 
  filter(FEATURE_CODE_DESC %in% c("Expressway", "Expressway Ramp")) %>%
  st_union()

to_majroad_sf <- 
  centerline_sf %>% 
  filter(FEATURE_CODE_DESC %in% c("Major Arterial", "Major Arterial Ramp")) %>%
  st_union()

to_road_sf <- 
  centerline_sf %>% 
  filter(FEATURE_CODE_DESC %in% c("Local", "Collector", "Collector Ramp", "Laneway", "Minor Arterial")) %>%
  st_union()

# zoning 
# you can read the metadata file to understand the data
temp <- tempfile()
download.file(url = "https://ckan0.cf.opendata.inter.prod-toronto.ca/dataset/34927e44-fc11-4336-a8aa-a0dfb27658b7/resource/d75fa1ed-cd04-4a0b-bb6d-2b928ffffa6e/download/Zoning%20Area%20-%204326.geojson", 
              destfile = temp)

zoning_sf <- 
  st_read(temp) %>%
  st_transform(., crs = 32617)

tm_shape(zoning_sf %>% filter(GEN_ZONE == 4)) + 
  tm_polygons("red") + 
  tmap_options(check.and.fix = TRUE)

tm_shape(zoning_sf %>% filter(PRCNT_COMM > 0)) + 
  tm_polygons("blue")

to_open_sf <- 
  zoning_sf %>% 
  filter(GEN_ZONE == 1) %>%  # from the metadata, GEN_ZONE == 1 is open area
  st_union()

to_com_sf <- 
  zoning_sf %>% 
  filter(PRCNT_COMM > 0) %>% # here I am choosing any commercial activity to be commercial. a subject matters expert may disagree. 
  st_union()

to_ind_sf <- 
  zoning_sf %>% 
  filter(GEN_ZONE == 4) %>% # from the metadata, GEN_ZONE == 4 is industrial area
  filter(st_is_valid(.)) %>%  # some of the shapes in the shapefile are not valid, often this is a polygon that doesn't close on itself. we could inspect to see exactly what's going on here. 
  st_union()

to_res_sf <- 
  zoning_sf %>% 
  filter(is.na(GEN_ZONE)) %>% # from the metadata, GEN_ZONE == 0 is residential area, though it looks like this reads in as missing values. there may be a better way to identify residential area. What do you see when you explore the data and metadata?
  filter(st_is_valid(.)) %>%
  st_union()

# transfer stations
# there are probably a lot of heavy vehicles coming and going to the transfer station. 
temp <- tempfile()
temp2 <- tempfile()
download.file(url = "https://ckan0.cf.opendata.inter.prod-toronto.ca/dataset/71ba09c0-de4b-4440-8a94-891c1e569d08/resource/f594583f-8d9f-44fa-a87f-099bcc05b8f9/download/transfer-stations-wgs84-jan-2012.zip",
              destfile = temp)
unzip(zipfile = temp, exdir = temp2)

to_trst_sf <- 
  st_read(temp2) %>%
  st_union() %>%
  st_transform(., crs = 32617)

# transit routes
# transit files can come in "gtfs" format, the tidytransit package helps with reading
ttc_gtfs <- 
  tidytransit::read_gtfs("https://ckan0.cf.opendata.inter.prod-toronto.ca/dataset/7795b45e-e65a-4465-81fc-c36b9dfff169/resource/cfb6b2b8-6191-41e3-bda1-b175c51148cb/download/TTC%20Routes%20and%20Schedules%20Data.zip")

route_geo <- 
  ttc_gtfs$shapes %>%  # just keep the shapes, i.e., the shapes of the transit route (we don't care about schedules here)
  # filter(shape_id %in% c(953035, 953036)) %>% # when developing this code chunk, I started by just looking at two routes
  split(.$shape_id) %>% # from purrr, we split the dataframe by the shape_id (the transit route id)
  map(., ~select(., lon = shape_pt_lon, lat = shape_pt_lat) %>% # for each item in the list (i.e., each transit route), select just lat and long
        as.matrix(.) %>% # turn them into matrices for the st_linestring() function
        st_linestring(., dim = "XYZ")) # connect each of the points for each route together to create linestrings of the routes. 

to_busroustes_sf <- 
  st_sfc(route_geo) %>% # turn them into simple features collections
  st_sf(., crs = 4326) %>% # set the crs
  filter(st_is_valid(.)) %>%
  st_union() %>%
  st_transform(., crs = 32617)

tm_shape(to_busroustes_sf) +
  tm_lines("purple")

# transit stops
to_busstops_sf <- 
  ttc_gtfs$stops %>% # pick just the bus stops
  select(stop_lon, stop_lat) %>%
  st_as_sf(., coords = c("stop_lon", "stop_lat"), crs = 4326) %>%  # turn them into an sf object
  st_transform(., crs = 32617)
# we will want to count the number of busstops near monitoring sites, so I won't use st_union()

# tm_shape(to_busstops_sf) +
#   tm_dots("darkgreen")

# National Pollution Release Inventory (NPRI) ####
# https://www.canada.ca/en/services/environment/pollution-waste-management/national-pollutant-release-inventory.html
temp <- tempfile()
download.file(url = "https://data-donnees.ec.gc.ca/data/substances/plansreports/reporting-facilities-pollutant-release-and-transfer-data/single-year-data-tables-by-facility-releases-transfers-and-disposals/NPRI-INRP_DataDonn%C3%A9es_2021.csv",
              destfile = temp)

all_npri <- 
  fread(temp)

# we will want to count the number of NPRI near monitoring sites, so I won't use st_union()

to_npri_pm <- 
  all_npri %>%
  select(1,2, 5,6, 15,16, 24) %>% # select the columns we care about. the names are long so I inspected and counted the column numbers
  filter(`Ville / City` == "Toronto", str_detect(`Nom de substance (Anglais) / Substance Name (English)`, "articulate")) %>% # filter to only include the sites that emit particulate matter
  select(5,6) %>%
  st_as_sf(., coords = c("Longitude / Longitude", "Latitude / Latitude"), crs = 4326) %>%
  st_transform(., crs = 32617)

to_npri_nox <- 
  all_npri %>%
  select(1,2, 5,6, 15,16, 24) %>%
  filter(`Ville / City` == "Toronto", str_detect(`Nom de substance (Anglais) / Substance Name (English)`, "itrogen")) %>% # filter to only include the sites that emit NOx. str_detect() just requires that the string contain the target
  select(5,6) %>%
  st_as_sf(., coords = c("Longitude / Longitude", "Latitude / Latitude"), crs = 4326) %>%
  st_transform(., crs = 32617)


# openstreetmaps ####
# openstreetmaps is like wikipedia for maps. users input the data, so it's not guaranteed to be perfect, but it is a great resource. 
# I probably don't trust the openstreetmaps data as much as I trust the city of Toronto data, BUT it has much more data and it goes beyond the borders of Toronto (if that becomes of interest)

# we need to define the area we are going to look at
border_bbox <- 
  to_border_sf %>%
  st_transform(., crs = 32617) %>% # transform so I can use distances of meters
  st_buffer(., dist = 500) %>% # extend the border beyond Toronto by 500 m - air pollution doesn't care about administrative boundaries
  st_transform(., crs = 4326) %>% # I think openstreetmaps expects crs = 4326
  st_bbox()

# industrial
osm_ind_lu <- 
  osmdata::opq(bbox = border_bbox) %>% # define area
  osmdata::add_osm_feature("landuse", "industrial") %>% # select the features you want. 
  osmdata::osmdata_sf() # download the sf objects

# what features can you ask for?
  # you can find what you are looking at: https://www.openstreetmap.org/#map=15/43.8048/-79.2525 
  # use the query features (mouse with ? on the right) to see what features are called in osm

osm_ind_build <- 
  opq(bbox = border_bbox) %>%
  add_osm_feature("building", "industrial") %>%   # in osm, there is landuse and buildings. are they different? do you care? lots of decisions to make when using osmdata
  osmdata_sf()

osm_ind_lu # you can see that osmdata gives several types of outputs, here we are interested in the polygons

# we could take just the land use or just the building, but here we'll just combine them together
to_osm_ind_lu <- 
  st_union(
    st_union(st_geometry(osm_ind_lu$osm_polygons)), 
    st_union(st_geometry(osm_ind_build$osm_polygons))
  ) %>% st_sf() %>% st_transform(., crs = 32617)

# how does the osmdata and toronto zoning data differ?
tm_shape(to_osm_ind_lu) +
  tm_polygons("red", alpha = 0.5) +
  tm_shape(to_ind_sf) +
  tm_polygons("darkgreen", alpha = 0.5)

# here I decided to simply combine them
to_ind_sf <- st_union(to_osm_ind_lu, to_ind_sf)

# airport
osm_airport_aero <- 
  opq(bbox = border_bbox) %>%
  add_osm_feature("aeroway", "aerodrome") %>%
  osmdata_sf()

to_airport <-
  osm_airport_aero$osm_polygons %>%
  st_centroid() %>%
  st_union() %>%
  st_transform(., crs = 32617)

# port
osm_port_lu <- 
  opq(bbox = border_bbox) %>%
  add_osm_feature("industrial", "port") %>%
  osmdata_sf()

to_port <- 
  osm_port_lu$osm_polygons %>%
  st_centroid() %>%
  st_union() %>%
  st_transform(., crs = 32617)

tm_shape(to_airport) +
  tm_dots("green") +
  tm_shape(to_port) +
  tm_dots("purple")

# railyard
osm_rail <- 
  opq(bbox = border_bbox) %>%
  add_osm_feature("railway", "rail") %>%
  osmdata_sf()

osm_rail_lu <- 
  opq(bbox = border_bbox) %>%
  add_osm_feature("landuse", "railway") %>%
  osmdata_sf()

# now compare the different sources of railways
tm_shape(osm_rail_lu$osm_polygons) +
  tm_polygons("black") +
  tm_shape(osm_rail$osm_lines) +
  tm_lines("purple") +
  tm_shape(to_rail_sf) +
  tm_lines("green")

to_railway_sf <- 
  osm_rail$osm_lines %>%
  st_union() %>%
  st_transform(., crs = 32617)

### END







# nice to include, try later - googletraffic #####

# library(googletraffic)
# library(raster)
# key <- 'AIzaSyDEAwSMd-GSOE6nmUY0we6EUx_vckKM-6k'
# r <- gt_make_raster_from_polygon(polygon = st_sf(to_sf),
#                                  zoom = 16,
#                                  google_key = key)
# r_df <- 
#   rasterToPoints(r, spatial = T) %>%
#   as.data.frame()
# 
# names(r_df) <- c("value", "lon", "lat")
# 
# ggplot() +
#   geom_raster(data = r_df, 
#               aes(x = lon, y = lat, 
#                   fill = as.factor(value))) +
#   labs(fill = "Traffic\nLevel") +
#   scale_fill_manual(values = c("green2", "orange", "red", "#660000")) +
#   coord_quickmap() + 
#   theme_void() +
#   theme(plot.background = element_rect(fill = "white", color="white"))

# night lights GEE 


# ebird 








