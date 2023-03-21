library(lhs)
library(tmap)
library(geohash)
library(sf)
library(tidyverse)

#######################################################
# READ ME ###
# this file downloads the extent of the study area and generates sites and links those sites to desired outcomes
# input: 
  # link to shapefile of target study area
  # surface of target outcome
# output:
  # .rds of study area (sf object) for use by other files
  # csv file with sites (rows) that has lat, lon, and outcome of interest at those sites
#######################################################

# create directories (i.e. folders). You could also point and click to create these directories
getwd() # verify what directory you are in
main_dir <- ""  # in case you want to change the main directory
new_dirs <- paste0(main_dir, 
                  c("figures", 
                    "data", 
                    "data/land_use_data", 
                    "data/models", 
                    "data/monitoring_data", 
                    "data/study_area")); new_dirs

# a loop to create each of the new directories
for (i in 1:length(new_dirs)){
  
  if(!dir.exists(new_dirs[i])){ # if the directory does not exist
    dir.create(new_dirs[i])   # then create the new directory
  } else {
    print(paste0(new_dirs[i], " already exists")) # else, let me know which ones already exist. this prevents overwriting an existing directory
  }
}


# download toronto shapefile ####
# create two temporary files so you don't need to think about where you will save and unzip the map
temp1 <- tempfile()
temp2 <- tempfile()

# download map to the first temporary file, that link is to a zip file of all of Canada
download.file(url = "https://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/files-fichiers/2016/lada000b16a_e.zip", 
              destfile = temp1)

# unzip it from the first temporary file and unzip it to the second temporary file
unzip(zipfile = temp1, exdir = temp2)

# read in the map using the sf package
# more information on "simple features" can be found here: https://r-spatial.github.io/sf/articles/sf1.html
to_border_sf <- st_read(temp2) %>%
  filter(CDNAME == "Toronto") %>% # the file is of all of Canada, based on some googling, we know that the CDUID of Montreal is 2466
  # filter(CDUID %in% c("3521", "3519", "3520", "3518")) %>% # this would be for Toronto and the surrounding area
  st_union() %>% #unify all of the parts of Toronto into one object. In this analysis, we are just looking for the boundary of the modelling area and we don't really care about divisions within it
  st_transform(., crs = 4326) # transform the coordinates to WGS 84

# take a look at our sf object
to_border_sf

# inspect using tmap
tmap_mode("view")  # set tmap to interactive viewing. We can switch back to plot using tmap_mode("plot")
tm_shape(to_border_sf) +  # identify the geospatial object we want to plot
  tm_polygons(alpha = 0.2)  # identify how we want to plot it

# we'll save it. From now on, we can just read in that .rds file. If we want to repeat the analysis for a different area, then we just need to make changes in one spot (i.e., above) and then the rest of the workflow will change
saveRDS(to_border_sf, "data/study_area/to_border_sf.rds")
# to_border_sf <- readRDS("data/study_area/to_border_sf.rds")


# randomly select sample sites ####

# we can inspect the bounding box of the study area. these are the spatial limits of the area
st_bbox(to_border_sf)

# we will be randomly selecting sites using "latin hypercube sampling" to give a variety of latitude and longitudes
set.seed(1997) # it's good to set the seed for reproducibility
lhs_dim_names <- c("lon","lat")
lhs_n_params <- length(lhs_dim_names)
lhs_n_samples <- 200
# lower bound
lhs_lb <- c(lon = st_bbox(to_border_sf)[[1]], lat = st_bbox(to_border_sf)[[2]]) 
# upper bound
lhs_ub <- c(lon = st_bbox(to_border_sf)[[3]], lat = st_bbox(to_border_sf)[[4]])
# Sample unit Latin Hypercube (uniform random!)
lhs_unif_sample <- lhs::randomLHS(lhs_n_samples, lhs_n_params)
colnames(lhs_unif_sample) <- lhs_dim_names

# Rescale to min/max of each parameter
lhs_scaled_sample <- matrix(nrow=lhs_n_samples, ncol=lhs_n_params)
colnames(lhs_scaled_sample) <- lhs_dim_names
for (i in 1:lhs_n_params){
  lhs_scaled_sample[,i] <- qunif(lhs_unif_sample[,i],
                                 min = lhs_lb[i],
                                 max = lhs_ub[i])
}

# make the lat lon into a sf objects
lhs_scaled_sample_sf <- 
  lhs_scaled_sample %>% 
  as.data.frame() %>%
  st_as_sf(., coords = c("lon", "lat"), crs = 4326, remove = F)

# take a look
tm_shape(to_border_sf) +
  tm_polygons(alpha = 0.2) + 
  tm_shape(lhs_scaled_sample_sf) +
  tm_dots()

# a lot were in the water, lets only keep those that intersect with our modeling area
site_keepers <- st_intersects(lhs_scaled_sample_sf, to_border_sf, sparse = F)

# inspect the output of st_intersects(), it is a vector of T/F indicating if our sites (spatial points) intersect with the study area (spatial polygon)
site_keepers

tm_shape(to_border_sf) +
  tm_polygons(alpha = 0.2) + 
  tm_shape(lhs_scaled_sample_sf[site_keepers,]) + # can use T/F vector to select the rows
  tm_dots()

# there is also the st_intersection() function which returns an sf object of the actually intersecting shape
  # in our case, that would be points. polygons intersecting with polygons would return a polygon
  # st_intersection() takes a lot longer than st_intersects() so I usually avoid it when I can

to_sites <- 
  lhs_scaled_sample[site_keepers, ] %>% 
  as.data.frame() %>%
  mutate(ghc_p5 = gh_encode(lat, lon, 5)) %>%  # note the geohash code of the site, we will discuss this later
  st_as_sf(., coords = c("lon", "lat"), crs = 4326, remove = F) %>%
  arrange(lon) %>%
  mutate(site_id = 1:n())

# A geohash code is a unique alpha-numeric code for gridsquares anywhere on earth. 
# You can think of it as a way to spatially group our road segments together. 
# This will be useful for when we split our data into train, validate, and test sets. 
# The quick scatter plot below shows a section of the study area and the colors represent different geohash gridsquares. 
# Notice how the colors are grouped together.
# More information on geohash codes can be found here:  
  #   http://ellse.org/uncategorized/how-geohash-works/  
  #   https://www.movable-type.co.uk/scripts/geohash.html  

tm_shape(to_border_sf) +
  tm_polygons(alpha = 0.2) + 
  tm_shape(to_sites) +
  tm_dots(col = "ghc_p5") +
  tm_text("site_id")

# get pollution levels at monitoring sites #####
###### ME CHANGE THIS TO CANUE DATA ######
pred_surf <- 
  st_read("../../../../../Sync/Research Projects/HEI RFA 19-1/data_files/processed/Montreal and Toronto Surfaces/Backcast and 2020 Surfaces/Toronto/toronto_backast_and_2020_surfaces.shp") %>%
  filter(year == 2020, model == "lur")

site_surf_intx <- st_intersects(to_sites, pred_surf)

surf_intx <- 
  pred_surf %>% 
  slice(unlist(site_surf_intx))

tm_shape(surf_intx) +
  tm_polygons("bc") + 
  tm_shape(to_sites) +
  tm_dots()

to_sites <- 
  to_sites %>%
  mutate(bc = surf_intx$bc,
         ufp = surf_intx$ufp)

write_csv(to_sites %>% st_set_geometry(NULL), "data/monitoring_data/to_monitoring_sites.csv")

