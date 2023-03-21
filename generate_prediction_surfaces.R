library(stars)
library(sf)
library(tmap)
library(raster)
library(mgcv)
library(tidyverse)

#######################################################
# READ ME ###
# this file uses trained models to generate prediction surfaces
# input: 
  # trained models from trained_lurs.R
  # study area raster and land use parameters from create_raster_extract_lu.R
  # monitoring sites and study area sf object just for reference/exploratory plots
# output:
  # final surfaces of various models

#######################################################

to_fishnet_lu <- read_csv("data/land_use_data/to_fishnet_lu.csv")
to_raster <- readRDS("data/study_area/to_raster.rds")

lm_all_data <- readRDS("data/models/lm_all_data.rds")
gam_all_data <- readRDS("data/models/gam_all_data.rds")

train_set_gams_list <- readRDS("data/models/train_set_gams_list.rds")
train_set_lms_list <- readRDS("data/models/train_set_lms_list.rds")

to_sites_lu_sf <- 
  read_csv("data/land_use_data/to_sites_lu.csv") %>%
  st_as_sf(., coords = c("lon", "lat"), crs = 4326)
to_border_sf_utm <- 
  readRDS("data/study_area/to_border_sf.rds") %>% 
  st_transform(., crs = 32617)

# raster stack comparing lm and gams on all data 
# create copies of same raster, these will be layers
to_raster_lm <- to_raster_gam <- to_raster

to_fishnet_lu_pred <- 
  to_fishnet_lu %>%
  mutate(pred_bc_lm = predict(lm_all_data, .),  # generate linear model predictions for each cell in the raster
         pred_bc_gam = predict(gam_all_data, .) %>% as.vector()) %>% # generate gam predictions for each cell in the raster
  arrange(cell_id)

raster::values(to_raster_lm) <- to_fishnet_lu_pred$pred_bc_lm
raster::values(to_raster_gam) <- to_fishnet_lu_pred$pred_bc_gam

to_raster_stack <- stack(to_raster, to_raster_lm, to_raster_gam)

names(to_raster_stack) <- c("cell_id", "pred_bc_lm", "pred_bc_gam")

tmap_mode("view")
tm_shape(to_border_sf_utm) +
  tm_polygons("gray", alpha = 0.1) +
  tm_shape(to_raster_stack) +
  tm_raster(alpha = 0.5)

# find where the negatives are 
tm_shape(to_border_sf_utm) +
  tm_polygons("lightblue", alpha = 0.1) +
  tm_shape(raster(to_raster_stack, layer = 3) < 0) +  # a simple operation on the gam layer gives a useful result
  tm_raster(alpha = 0.5)

# use mask() to get rid of cells beyond the borders. the first argument is the raster and the second is the vector, but instead of an sf objects, it needs to be a spatial object  
to_raster_stack_all_data <- 
  mask(to_raster_stack,
       as_Spatial(to_border_sf_utm %>% st_buffer(., dist = 100))) # set a buffer so we don't drop too many cells along the border

tm_shape(to_border_sf_utm) +
  tm_polygons("lightblue", alpha = 0.1) +
  tm_shape(to_raster_stack_all_data) +
  tm_raster(alpha = 0.5) + 
  tm_shape(to_sites_lu_sf) +
  tm_dots("purple")  # add our monitoring sites

# make stacks of the 4 folds of each model. 
to_raster_lm1 <- to_raster_lm2 <- to_raster_lm3 <- to_raster_lm4 <- to_raster
to_raster_gam1 <- to_raster_gam2 <- to_raster_gam3 <- to_raster_gam4 <- to_raster
my_sets <- 1:4

to_fishnet_cv_preds <- 
  map(train_set_lms_list, ~mutate(to_fishnet_lu, pred_lm = predict(., to_fishnet_lu))) %>% # take the list of lm(), and use it to predict on the raster values. the output will be a list of dataframes         
  map2(., train_set_gams_list, ~mutate(.x, pred_gam = predict(.y, .x) %>% as.vector())) %>% # use map2() to take the list of dataframes and use the list of gams to generate predictions
  imap(., ~mutate(.x, ho_set = .y)) %>% # add a column that indicates the name of the dataset (i.e., which set was held out for model training)
  bind_rows() %>%
  dplyr::select(ho_set, cell_id, lon, lat, pred_lm, pred_gam) %>%
  pivot_wider(names_from = "ho_set", values_from = c("pred_lm", "pred_gam")) %>%
  arrange(cell_id)

# use lm predictions to set raster values
raster::values(to_raster_lm1) <- to_fishnet_cv_preds$pred_lm_ho_1
raster::values(to_raster_lm2) <- to_fishnet_cv_preds$pred_lm_ho_2
raster::values(to_raster_lm3) <- to_fishnet_cv_preds$pred_lm_ho_3
raster::values(to_raster_lm4) <- to_fishnet_cv_preds$pred_lm_ho_4

# create stack
to_raster_stack_lm_ho <- stack(to_raster_lm1, to_raster_lm2, to_raster_lm3, to_raster_lm4)
names(to_raster_stack_lm_ho) <- paste0("pred_lm_ho_", 1:4)

to_raster_stack_lm_ho <- mask(to_raster_stack_lm_ho, as_Spatial(to_border_sf_utm %>% st_buffer(., dist = 100)))

# let's use prettier colours this time.
# install.packages("shinyjs") # install if not already installed
# tmaptools::palette_explorer() # this will show colour palettes
# click "stop" in veiwer when done

tm_shape(to_border_sf_utm) +
  tm_polygons("lightblue", alpha = 0.1) +
  tm_shape(to_raster_stack_lm_ho) +
  tm_raster(alpha = 0.7, palette = "-RdYlGn", breaks = seq(0, 10000,by = 1000)) + 
  tm_shape(to_sites_lu_sf) +
  tm_dots("purple")


# repeat for GAM
raster::values(to_raster_gam1) <- to_fishnet_cv_preds$pred_gam_ho_1
raster::values(to_raster_gam2) <- to_fishnet_cv_preds$pred_gam_ho_2
raster::values(to_raster_gam3) <- to_fishnet_cv_preds$pred_gam_ho_3
raster::values(to_raster_gam4) <- to_fishnet_cv_preds$pred_gam_ho_4

to_raster_stack_gam_ho <- stack(to_raster_gam1, to_raster_gam2, to_raster_gam3, to_raster_gam4)
names(to_raster_stack_gam_ho) <- paste0("pred_gam_ho_", 1:4)

to_raster_stack_gam_ho <- mask(to_raster_stack_gam_ho, as_Spatial(to_border_sf_utm %>% st_buffer(., dist = 100)))

tm_shape(to_border_sf_utm) +
  tm_polygons("lightblue", alpha = 0.1) +
  tm_shape(to_raster_stack_gam_ho) +
  tm_raster(alpha = 0.7, palette = "-RdYlGn", breaks = seq(0, 10000,by = 1000)) + 
  tm_shape(to_sites_lu_sf) +
  tm_dots("purple")


tm_shape(to_border_sf_utm) +
  tm_polygons("lightblue", alpha = 0.1) +
  tm_shape(mean(to_raster_stack_gam_ho)) +  # you can also look at the average of the layers
  tm_raster(alpha = 0.7, palette = "-RdYlGn", breaks = seq(0, 10000,by = 1000)) + 
  tm_shape(to_sites_lu_sf) +
  tm_dots("purple")

# now you have some surfaces:
mean(to_raster_stack_gam_ho)
mean(to_raster_stack_lm_ho)
to_raster_stack_all_data

# you could change them to polygons:
to_fishnet_gam_ho <- st_as_sf(st_as_stars(to_raster_stack_gam_ho), as_points = F, merge = T)
# notice is only does the first layer

# you also have other information you can link to it using layer:
to_fishnet_lu


# if we have time, fun with rayshader ####
library(hexbin)
library(rayrender)
library(rayshader)
library(rayimage)

to_raster_stack_all_data

to_fishnet_gam <- st_as_sf(st_as_stars(raster(to_raster_stack_all_data, layer = 3)), as_points = F, merge = T)

fishnet_gg <- 
  ggplot(to_fishnet_gam) +
  geom_sf(aes(fill = pred_bc_gam), lwd = 0) +
  scale_fill_distiller(palette = "RdYlGn", direction = -1, aesthetics = "fill", breaks = seq(0, 10000,by = 1000)) +
  theme(legend.position = "none",
        axis.line=element_blank(),
        axis.text.x=element_blank(), axis.title.x=element_blank(),
        axis.text.y=element_blank(), axis.title.y=element_blank(),
        axis.ticks=element_blank(),
        panel.background = element_blank()) +
  # theme_bw() +
  xlab("Longitude") + ylab("Latitude") + labs(fill = "Predicted BC Concentration (ng/m\u00B3)")

plot_gg(fishnet_gg,width=12,height=7,scale=250,windowsize=c(1000, 600), solid = FALSE,
        raytrace=FALSE, zoom = 0.35, phi = 45, theta = 15, triangulate = TRUE, max_error = 0)

# can save that view as an image
render_snapshot("figures/rayshader_bc_pred.png")

render_movie(  title_text = "Toronto Predicted BC Concentration",
               title_offset = c(20, 20),
               title_color = "black",
               title_size = 90,
               title_font = "sans",
               fps = 30, frames = 720,
               "figures/rayshader_bc_pred.mp4")




