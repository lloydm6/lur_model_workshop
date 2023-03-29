
##############################################################################################################
# PACKAGES #######################################################
# create a vector of all the packages (i.e. libraries) we will use
required_packages <- c("purrr",        # purrr for data manipulation using the map() and split() functions
                       "shinyjs",      # shinyjs we'll once to help us pick a color template
                       "rayrender",    # raydender at the end for visualization
                       "rayshader",    # rayshader at the end for visualization
                       "rayimage",     # rayimage at the end for visualization
                       "gridExtra",    # gridExtra for arranging plots
                       "Hmisc",        # Hmisc for quick descriptive statistics using describe()
                       "sf",           # sf for all of our vector geometries
                       "broom",        # broom for tidy model summaries
                       "mgcv",         # mgcv to train generalized additive models
                       "mgcViz",       # mgcViz to visualize generalized additive models
                       "spdep",        # spdep to calculate Moran's Index
                       "ape",          # ape to calculate weights for Moran's Index
                       "tmap",         # tmap for quick exploratory plots
                       "stars",        # stars to convert rasters to polygons
                       "raster",       # raster to create and process raster data
                       "tidyverse")    # tidyverse for most of our data manipulation/processing

# check to see if they've already been install. If they haven't been install, then they are a new package 
# subset the required_packages to just the new packages
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]

# install each new package
if(length(new_packages)) install.packages(new_packages)

# load all the packages
sapply(required_packages, require, character.only = T)

# # alternatively, you could load each package individually
# library(purrr)
# library(sf)
# library(rayrender)
# library(rayshader)
# library(rayimage)
# library(gridExtra)
# library(stars)
# library(raster)
# library(shinyjs)
# library(broom)
# library(Hmisc)
# library(mgcv)
# library(mgcViz)
# library(spdep)
# library(ape)
# library(tidyverse)

##############################################################################################################
##############################################################################################################
# READ ME ###
# this file trains lm and gam lur models and then evaluates them. then these models are used to generate prediction surfaces.
# input: 
  # sites with monitoring data and land use (data/land_use_data/to_sites_lu.csv)
  # study area sf object just for reference in plots
  # study area raster (data/study_area/to_raster.rds)
  # land use parameters for each cell in the study area raster (data/land_use_data/to_fishnet_lu.csv)
# output:
  # .rds of various models
  # it also generates several plots on model diagnostics which could be exported
  # final surfaces of various models
##############################################################################################################

# folder set up ######
# create directories (i.e. folders). You could also point and click to create these directories
getwd() # verify what directory you are in
main_dir <- ""  # in case you want to change the main directory
new_dirs <- paste0(main_dir, 
                   c("figures", 
                     "data", 
                     "data/land_use_data", 
                     "data/models", 
                     "data/study_area")); new_dirs

# a loop to create each of the new directories
for (i in 1:length(new_dirs)){
  
  if(!dir.exists(new_dirs[i])){ # if the directory does not exist
    dir.create(new_dirs[i])   # then create the new directory
  } else {
    print(paste0(new_dirs[i], " already exists")) # else, let me know which ones already exist. this prevents overwriting an existing directory
  }
}

#######################################################################################################################
# !!! IMPORTANT !!! Move the "to_sites_lu.csv" and "to_fishnet_lu.csv" files to the data/land_use_data/ folder
######################################################################################################################

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

to_border_sf_utm <- st_transform(to_border_sf, crs = 32617)

# inspect using tmap
tmap_mode("view")  # set tmap to interactive viewing. We can switch back to plot using tmap_mode("plot")
tm_shape(to_border_sf) +  # identify the geospatial object we want to plot
  tm_polygons(col = "darkblue", alpha = 0.2)  # identify how we want to plot it

# we'll save it. From now on, we can just read in that .rds file. If we want to repeat the analysis for a different area, then we just need to make changes in one spot (i.e., above) and then the rest of the workflow will change
saveRDS(to_border_sf, "data/study_area/to_border_sf.rds")
# to_border_sf <- readRDS("data/study_area/to_border_sf.rds")

# read in the simulated monitoring data.
# each row represents a site
# it also has the land use data within 200 m and 500 m of each site
to_sites_lu <- read_csv("data/land_use_data/to_sites_lu.csv")

# inspect the data
Hmisc::describe(to_sites_lu)

# variable selection ####
# we want to see the associations between each land use parameter and pm2.5
# let's use univariable linear regressions
uni_lin_regs <- 
  to_sites_lu %>%
  pivot_longer(cols = green_200m:distance_npri_nox, names_to = "lu_var", values_to = "value") %>%  # make the wide data long
  split(.$lu_var) %>%   # turn the data frame into a list of data frames. each data frame in this list will have a single land use variable
  map(., ~lm(data = ., pm25 ~ value)) %>%  # for each data frame in the list, regress the land use variable values onto the outcome of interest. 
  map(., ~broom::glance(.)) %>% # glance() summarizes the linear model
  imap(., ~mutate(.x, lu_var = .y)) %>% # imap() is like map() except is also uses the names of the items in the list. here we are using the names to create a new column that is filled with the names
  bind_rows() %>%  # bind the rows of all the items in the list. this gets us ba
  dplyr::select(lu_var, r2 = r.squared, logLik, AIC, pval = p.value) # we are only interested in certain measures. We could also calculate RMSE here

# if two buffers of the same variable are selected (e.g., busstop_200m and busstop_500m), then we only want to keep the one with the highest R2
# create a dataframe of selected variables
selected_vars_df <- 
  uni_lin_regs %>%
  filter(pval < 0.05) %>%  # just look at the land use variables that are associated with the outcome
  mutate(var_name = str_remove(lu_var, "_[:digit:]00m")) %>%  # create a new column for the names that removes the part of the name that indicates the size of the buffer. this will allow us to 
  split(.$var_name) %>% # split by the land use names (without the buffer). this groups them together so we can take the more useful buffer
  map(., ~arrange(., desc(r2)) %>% slice(., 1)) %>%  # for each land use variable, sort by the value of r2, and then select the top row (i.e. the highest value of r2)
  bind_rows() %>%
  dplyr::select(-var_name)

# inspect one of associations. 
ggplot(data = to_sites_lu, aes(x = busroutes_500m, y = pm25)) +
  geom_point() + 
  geom_smooth(col = "darkgreen") +
  geom_smooth(method = "lm", col = "firebrick") + 
  xlab("Length of Bus Roustes within 500 meters (m)") + ylab("PM2.5 Concentration (ug/m\U00B3)") +
  theme_bw()
# it may not seem like much, but everything counts

# are any associations being driven by outliers?
# repeat but with the outliers removed
uni_lin_regs_no_outs <- 
  to_sites_lu %>%
  pivot_longer(cols = green_200m:distance_npri_nox, names_to = "lu_var", values_to = "value") %>%
  split(.$lu_var) %>%
  map(., ~mutate(., mean_val = mean(value), sd_val = sd(value), std_val = (value - mean_val)/sd_val)) %>%  # calculate standardized value for each land use variable, 
  map(., ~filter(., abs(std_val) < 2)) %>%  # remove any observations that are greater that 2 sds from the mean
  map(., ~lm(data = ., pm25 ~ value) %>% glance()) %>% # repeat the regressions
  imap(., ~mutate(.x, lu_var = .y)) %>%
  bind_rows() %>%
  dplyr::select(lu_var, r2 = r.squared, logLik, AIC, pval = p.value)

selected_vars_no_outs_df <- 
  uni_lin_regs_no_outs %>%
  filter(pval < 0.05) %>%
  mutate(var_name = str_remove(lu_var, "_[:digit:]00m")) %>%
  split(.$var_name) %>%
  map(., ~arrange(., desc(r2)) %>% slice(., 1)) %>%
  bind_rows() %>%
  dplyr::select(-var_name)

# see which are driven by outliers
outlier_check <- 
  full_join(selected_vars_no_outs_df %>% dplyr::select(lu_var, no_outliers_r2 = r2),
          selected_vars_df %>% dplyr::select(lu_var, r2 = r2)); outlier_check
# any with NA in the no_outliers_r2 column are diven by outliers
# need to decide what to do. keep only those that are associated with pm2.5 both with and without outliers?
plot(to_sites_lu$pm25 ~ to_sites_lu$road_500m)

# which variables will we exclude based on uni_lin_regs_no_outs 
lu_var_to_remove <- 
  outlier_check %>% 
  filter(is.na(no_outliers_r2)) %>% pull(lu_var) 

# we don't want pairs of variables that are highly correlated. 
  # first because they have somewhat redundant information (i.e., not a huge benefit to having both)
  # second because we have to assume the correlation is the same throughout the entire study area. that may or may not be an assumption you are comfortable making. 
# check correlations between variables
lu_cor_mtx <- 
  to_sites_lu %>%
  dplyr::select(selected_vars_df$lu_var) %>%  # keep only the variables we have selected so far
  cor(., method = "spearman") # this returns a matrix of correlations between all variables

diag(lu_cor_mtx) <- 0  # the diagonal is the correlations of variables with themselve, which is 1. set to 0 because we don't want to see them when we are looking at high correlations

lu_hi_cor <- 
  lu_cor_mtx %>%
  as.data.frame() %>%
  mutate_all(., ~. > 0.7) %>%  # we want to identify pairs with correlation > 0.7, that is the threshold we are using for variable selection. other thresholds are defensible as well
  filter_all(., any_vars(. == TRUE)) %>%  # keep the rows with high correlations
  t() %>%  # transpose to make the columns rows and vice versa
  as.data.frame() %>%
  filter_all(., any_vars(. == TRUE)) # keep the rows (which were columns) with high correlations

# so we see that bus routes and bus stops are highly correlated (not surprising)
# we also see that green and open are highly correlated (also not surprising)
# remove the one from each pair, remove the one with lower r2

selected_vars_df %>% 
  filter(lu_var %in% colnames(lu_hi_cor))
# remove busstop, remove green, and remove open (green and open were already removed due to outliers)
lu_var_to_remove <- c(lu_var_to_remove, "busstop_500m") # add this to the variables we want to remove

# it's handy to have an object of all the variable names
lur_vars <- 
  selected_vars_df %>%
  filter(!lu_var %in% lu_var_to_remove) %>% # after the workshop, you can try repeating this process without removing these variables, what happens?
  pull(lu_var)  # just pull the land use variable name column

# train linear model - add RMSE to model results #####

# check the distribution of the outcome. if it is skewed, we may consider log-transforming it
hist(to_sites_lu$pm25)
# probably not skewed enough to justify transforming

# we'll create a formula object that we can use any time we use lm()
# we keep longitude and latitude in it in order to model the spatial dependencies that are not captured by the land use parameters
lm_lur_formula <- as.formula(paste0("pm25 ~ ", paste(lur_vars, collapse = " + "), " + lon + lat"))
lm_lur_formula

# try training a model on all data
lm_all_data <- lm(data = to_sites_lu, lm_lur_formula)
summary(lm_all_data) # is this summary a fair evaluation of the model?  

# split into 4 sets. split by geohash code.
possible_ghc_p5 <- unique(to_sites_lu$ghc_p5) # find the unique geohash codes
set.seed(1997)
my_sets <- 1:4  # we want four sets
gph_p5_sets <- split(possible_ghc_p5, my_sets)  # split the geohash codes into four sets

to_sites_lu <- 
  to_sites_lu %>%
  mutate(set = case_when(ghc_p5 %in% gph_p5_sets$`1` ~ 1, # assign each set based on the geohash code split
                         ghc_p5 %in% gph_p5_sets$`2` ~ 2,
                         ghc_p5 %in% gph_p5_sets$`3` ~ 3,
                         TRUE ~ 4))
describe(to_sites_lu$set) # verify it's a nice looking split

# inspect a map of the folds
ggplot(data = to_border_sf) +
  geom_sf(fill = "grey60") +
  geom_sf(data = to_sites_lu %>% st_as_sf(., coords = c("lon", "lat"), crs = 4326), aes(col = as.factor(set))) +
  scale_color_manual(name = "Fold", values = c("darkgreen", "firebrick", "orange", "purple")) +
  theme_void()


# now we can train four different linear models, one for each hold out set. This is cross validation 
train_set_lms_list <- 
  purrr::map(as.list(my_sets), ~filter(to_sites_lu, set != .)) %>% # this creates a list where each item in the list is the dataframe with one of the set held out 
  # map(., ~select(., site_id, set)) # uncomment to help us see what previous line is doing. Basically creating a list where each item in the list is the monitoring data with one of the sets held out
  setNames(., paste0("ho_", my_sets)) %>%  # give a name to each item in the list. using "ho" as "hold-out"
  map(., ~lm(data = ., lm_lur_formula))  # train a lm() on each item in the list

# now generate predictions in the test sets
lm_pred_test_sets <- 
  map(as.list(my_sets), ~filter(to_sites_lu, set == .)) %>% # map(., ~select(., site_id, set)) # create a list of only the hold out sets
  setNames(., paste0("test_", my_sets)) %>%
  map2(., train_set_lms_list, ~mutate(.x, pred_lm = predict(.y, .x))) %>% # map2() takes two lists, so use the list of linear models to generate predictions in the list of hold out sets
  map(., ~mutate(., resid_lm = pm25 - pred_lm)) # get the residuals

# a quick verification that we did what we wanted to do
to_sites_lu %>% filter(set == 4) %>%
  predict(train_set_lms_list$ho_4, .)
lm_pred_test_sets$test_4 %>% pull(pred_lm)

# now compare the model predicted values to the measured values in the test sets using a linear regression
# get the r2 value in the test sets
lm_ho_model_eval <- 
  lm_pred_test_sets %>%
  map(., ~lm(data = ., pm25 ~ pred_lm) %>% glance()) %>% # compare predicted to observed
  bind_rows(); lm_ho_model_eval 
# notice how the r2 value varies depending on which data split is used

# get the mean r2 value. this is our estimate of our linear model's performance (i.e., result of model validation)
lm_ho_model_eval %>%
  pull(r.squared) %>% mean()

# contrast that to the r2 when we trained the model on all the data
lm_all_data %>% glance() %>% pull(r.squared)

# use each of the hold out models to generate predictions in the entire dataset. This is just for visualizations. 
to_sites_lu_lm_preds <- 
  map(train_set_lms_list, ~mutate(to_sites_lu, pred_lm = predict(., to_sites_lu))) %>%
  imap(., ~mutate(.x,
                  resid_lm = pm25 - pred_lm,
                  train_on = .y)) %>%
  bind_rows() %>% 
  pivot_wider(., names_from = "train_on", values_from = c("pred_lm", "resid_lm")) %>%
  mutate(mean_pred_lm = rowMeans(dplyr::select(., starts_with("pred_")))) %>%
  mutate(mean_resid_lm = rowMeans(dplyr::select(., starts_with("resid_")))) %>%
  mutate(pred_lm_all = predict(lm_all_data, .),
         resid_lm_all = pm25 - pred_lm_all)

# compare the distribution of observed PM2.5 with the distributions of each of the linear models we have trained
# we hope they are all pretty similar
to_sites_lu_lm_preds %>%
  rename(observed = pm25) %>%
  pivot_longer(cols = c(observed, pred_lm_ho_1:pred_lm_ho_4, mean_pred_lm, pred_lm_all)) %>%
  ggplot(data =., aes(x = value, col = name, fill = name)) +
  geom_density(alpha = 0.5) + 
  xlab("PM2.5 Concentration (ug/m\U00B3)") +
  scale_color_discrete(name = "PM2.5") + scale_fill_discrete(name = "PM2.5") + 
  theme_bw()

# compare the observed to the predicted in a scatter plot
ggplot(data = to_sites_lu_lm_preds, aes(x = pred_lm_all, y = pm25)) +
  geom_point(col = "#B8321A") +
  theme_bw() +
  geom_abline(slope = 1, intercept = 0) +
  xlab("lm() Predicted PM2.5 Concentration (ug/m\U00B3)") +
  ylab("Measured PM2.5 Concentration (ug/m\U00B3)")

# instead of linear models, train Generalized Additive Models, but only to allow more flexibility on the lat and long #####
gam_lur_formula <- as.formula(paste0("pm25 ~ ", paste(lur_vars, collapse = " + "), " + te(lon, lat, k = 7)")) # te(lon, lat, k = 7) is a tensor producti of lat and long. bascially an interaction term. k = 7 is the number of basis functions, which it the level of flexibility. higher is more flexible, but we run out of degrees of freedom. you will get an error if it's too high    
gam_lur_formula
# notice how the formula is very similar to the lm() formula, but it has a different term for latitude and longitude

gam_all_data <- mgcv::gam(data = to_sites_lu, gam_lur_formula, method = "REML") # train on all data


# use the same split as the lm(), train GAM on 4 folds
train_set_gams_list <- 
  map(as.list(my_sets), ~filter(to_sites_lu, set != .)) %>%
  setNames(., paste0("ho_", my_sets)) %>%
  map(., ~gam(data = ., gam_lur_formula, method = "REML"))

# generate predictions in the test sets
gam_pred_test_sets <- 
  map(as.list(my_sets), ~filter(to_sites_lu, set == .)) %>%
  setNames(., paste0("test_", my_sets)) %>%
  map2(., train_set_gams_list, ~mutate(.x, pred_gam = predict(.y, .x))) %>%
  map(., ~mutate(., resid_gam = pm25 - pred_gam))

# double check to make sure it worked
to_sites_lu %>% filter(set == 4) %>%
  predict(train_set_gams_list$ho_4, .)
gam_pred_test_sets$test_4 %>% pull(pred_gam)

# now compare the GAM model predicted values to the measured values in the test sets using a linear regression
# get the r2 value in the test sets
gam_ho_model_eval <- 
  gam_pred_test_sets %>%
  map(., ~lm(data = ., pm25 ~ pred_gam) %>% glance()) %>%
  bind_rows() 

# get the mean r2 value, this is your estimate of model performance for the GAM
gam_ho_model_eval %>%
  pull(r.squared) %>% mean()
summary(gam_all_data)$dev.expl # compare to r2 when not using hold out sets. 

to_sites_lu_gam_preds <- 
  map(train_set_gams_list, ~mutate(to_sites_lu, pred_gam = predict(., to_sites_lu))) %>%
  imap(., ~mutate(.x, 
                  resid_gam = pm25 - pred_gam,
                  train_on = .y)) %>%
  bind_rows() %>%
  pivot_wider(., names_from = "train_on", values_from = c("pred_gam", "resid_gam")) %>%
  mutate(mean_pred_gam = rowMeans(dplyr::select(., starts_with("pred_gam")))) %>%
  mutate(mean_resid_gam = rowMeans(dplyr::select(., starts_with("resid_gam")))) %>%
  mutate(pred_gam_all = predict(gam_all_data, .),
         resid_gam_all = pm25 - pred_gam_all)

# compare the distribution of observed PM2.5 with the distributions of each of the GAM models we have trained
# we hope they are all pretty similar
to_sites_lu_gam_preds %>%
  pivot_longer(cols = c(pm25, pred_gam_ho_1:pred_gam_ho_4, mean_pred_gam, pred_gam_all)) %>%
  ggplot(data =., aes(x = value, col = name, fill = name)) +
  geom_density(alpha = 0.5) +
  xlab("PM2.5 Concentration (ug/m\U00B3)") +
  scale_color_discrete(name = "PM2.5") + scale_fill_discrete(name = "PM2.5") + 
  theme_bw()

ggplot(data = to_sites_lu_gam_preds, aes(x = pred_gam_all, y = pm25)) +
  geom_point(col = "#44781E") +
  theme_bw() +
  geom_abline(slope = 1, intercept = 0) +
  xlab("gam() Predicted PM2.5 Concentration (ug/m\U00B3)") +
  ylab("Measured PM2.5 Concentration (ug/m\U00B3)")


# save models
saveRDS(gam_all_data, "data/models/gam_all_data.rds")
saveRDS(lm_all_data, "data/models/lm_all_data.rds")
saveRDS(train_set_gams_list, "data/models/train_set_gams_list.rds")
saveRDS(train_set_lms_list, "data/models/train_set_lms_list.rds")


# check model assumptions #####
# https://www.r-bloggers.com/2019/11/the-hidden-diagnostic-plots-for-the-lm-object/
# https://data.library.virginia.edu/diagnostic-plots/
# http://www.sthda.com/english/articles/39-regression-model-diagnostics/161-linear-regression-assumptions-and-diagnostics-in-r-essentials/
# https://book.stat420.org/model-diagnostics.html

# typical lm() model diagnostic plots

# for the model trained on all the data
par(mfrow = c(1,5))
for(j in 1:5){
  plot(lm_all_data, which = j, main = paste0("all_data"))
}

# for each of the models trained with a set (aka fold) held out
par(mfrow = c(1,1))
par(mfrow = c(4,4))
for(i in 1:4){
  for(j in 1:4){
    plot(train_set_lms_list[[i]], which = j, main = paste0("hold_out_set_", i))
  }
}
par(mfrow = c(1,1))
# some interesting points
# inspect Cook's distance points from each lm
lm_cooks_points <- list(c(31, 61, 70), c(1, 74, 76), c(4, 66, 76), c(60, 62, 63))
lm_qq_points <- list(c(61, 70, 65), c(76, 74, 78), c(66, 76, 72), c(60, 63, 62))

# see if they are the same sites that are problematic for each of those lm
map(train_set_lms_list, ~.$model) %>%
  map2(lm_cooks_points, ., ~slice(.y, .x)) %>%
  imap(., ~mutate(.x, model_name = .y) %>% relocate(model_name)) %>%
  bind_rows() %>% 
  arrange(pm25)

# typical gam() model diagnostic plots
check(getViz(gam_all_data))

par(mfrow = c(1,1))
par(mfrow = c(4,4))
gam.check(train_set_gams_list$ho_1)
gam.check(train_set_gams_list$ho_2)
gam.check(train_set_gams_list$ho_3)
gam.check(train_set_gams_list$ho_4)
par(mfrow = c(1,1))


# we can also visualize the te(lat,lon) term
# that is the term that is meant to be modelling the spatial dependencies not captured by our land use variables
par(mfrow = c(1,2))
vis.gam(gam_all_data, c("lon", "lat"), color = "terrain", plot.type = "contour",
        theta = 0, phi = 45)
vis.gam(gam_all_data, c("lon", "lat"), color = "terrain", plot.type = "persp",
        theta = 0, phi = 45)

par(mfrow = c(2,2))
vis.gam(train_set_gams_list$ho_1, c("lon", "lat"), color = "terrain", plot.type = "persp",
        theta = 0, phi = 45)
vis.gam(train_set_gams_list$ho_2, c("lon", "lat"), color = "terrain", plot.type = "persp",
        theta = 0, phi = 45)
vis.gam(train_set_gams_list$ho_3, c("lon", "lat"), color = "terrain", plot.type = "persp",
        theta = 0, phi = 45)
vis.gam(train_set_gams_list$ho_4, c("lon", "lat"), color = "terrain", plot.type = "persp",
        theta = 0, phi = 45)

# spatial clustering of residuals #####
# now that we have done the typical model diagnostic plots, we can take a look at the spatial clustering of residuals

# let's join the lm() and gam() predictions into one dataframe. this will make the coding easier
to_sites_lu_preds <- 
  to_sites_lu_lm_preds %>% 
  left_join(to_sites_lu_gam_preds %>% dplyr::select(site_id, pred_gam_ho_1:resid_gam_all))

# create a sf object so we can plot it
to_sites_lu_preds_sf <- 
  to_sites_lu_preds %>%
  st_as_sf(., coords = c("lon", "lat"), crs = 4326, remove = F) %>%  # specify the lat and long columns as well as the coordinate reference system
  st_transform(., crs = 32617) %>%  # project the CRS to have distance in meters
  mutate(lon_utm = st_coordinates(.)[,1], lat_utm = st_coordinates(.)[,2])  # create columns with the norhting and eastings. 

# are the residuals spatially clustered? try the eyeball test
tm_shape(to_sites_lu_preds_sf) + 
  tm_dots("resid_lm_all")

tm_shape(to_sites_lu_preds_sf) + 
  tm_dots("resid_gam_all")

# we can also make plots that are a little closer to publication-ready
ggplot(data = to_border_sf_utm) +
  geom_sf(fill = "grey80") +
  geom_sf(data = to_sites_lu_preds_sf, aes(col = resid_lm_all)) + 
  scale_color_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, name = "Linear Model Residuals") + # we set the midpoint to white so it's easier to see clusters of similar color
  theme_bw()

# this plots spatial distributions of model errors for each model, side by side
to_sites_lu_preds_sf %>% 
  pivot_longer(., cols = c("resid_lm_all", "resid_gam_all"), names_to = "Model", values_to = "Residuals") %>%
  mutate(Model = case_when(Model == "resid_lm_all" ~ "Linear Model",
                           TRUE ~ "Generalized Additive Model")) %>%
  ggplot(data = .) + 
  geom_sf(data = to_border_sf_utm, fill = "grey80") +
  geom_sf(aes(fill = Residuals), pch = 21) + 
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +  # we set the midpoint to white so it's easier to see clusters of similar color
  facet_wrap(~Model) + 
  theme_bw()
ggsave("figures/residuals_plot.png",
       width = 7, height = 3, units = "in", dpi = 300)
# passed or failed the eyeball test??

# Calculate and test Moran's Index #####
# guide to Moran's Index https://www.statisticshowto.com/morans-i/#:~:text=Calculations%20for%20Moran's%20I%20are,j%20with%20the%20overall%20mean.&text=The%20Moran's%20statistic%20is%20calculated,%CC%84y)2)%2Fn).
  # -1 is perfect clustering of dissimilar values (you can also think of this as perfect dispersion).
  # 0 is no autocorrelation (perfect randomness.)
  # +1 indicates perfect clustering of similar values (it’s the opposite of dispersion).

# first need to define the spatial relationship between sites. we will use distance
nb_object <- dnearneigh(to_sites_lu_preds_sf, 0, 100000) # choice of upper distance affects results
lw <- nb2listwdist(nb_object, to_sites_lu_preds_sf, type = "idw") # use inverse distance as the weights for Moran's I

# try the moran.test() function from the spdep library
try_mtest <- spdep::moran.test(to_sites_lu_preds$resid_lm_ho_3, lw) # test to see if the residuals are spatially clustered
try_mtest

# write a function to extract information from the moran.test() in a format that we like
moran_result_df_fn <- function(x){
  p_val <- x$p.value
  observed_mi <- x$estimate[1]
  expected_mi <- x$estimate[2]
  variance <- x$estimate[3]
  stat <- x$statistic
  fn_output <- data.frame(observed_mi, expected_mi, p_val, variance, stat, row.names = NULL)

  return(fn_output)
}

# now get moran's index for each model we have trained
moran_i_results <-
  to_sites_lu_preds %>%
  dplyr::select(contains("resid_")) %>%  # only want the columns with residuals
  pivot_longer(cols = contains("resid")) %>% 
  split(.$name) %>%  # create a list where each item in the least is the residuals from a single model
  map(., ~pull(., value) %>% moran.test(., lw)) %>%  # apply the moran.test() function to each item in the list. use the lw as the weights
  map(., ~moran_result_df_fn(.)) %>% # use the function we wrote to extract useful information
  imap(., ~mutate(.x, model = .y)) %>%
  bind_rows() %>%
  mutate(resids_clstrd = case_when(p_val < 0.05 ~ "yes",  # determine if the observed moran's i is likely to do change or due to clustered residuals 
                                   TRUE ~ "no")) %>%
  mutate(model_type = str_extract(model, "lm|gam")) %>%
  relocate(c(model, model_type, resids_clstrd)) %>%
  arrange(observed_mi)

moran_i_results

# if you are interested, you can also manually calculate moran's I

dist_mtx <- 
  st_distance(to_sites_lu_preds_sf, to_sites_lu_preds_sf)

inv_dist_mtx <- 1/dist_mtx
diag(inv_dist_mtx) <- 0

my_morans_i_fn <- function(y_vec, w_matrix){
  
  n <- length(y_vec)
  y <- y_vec
  ybar <- mean(y_vec)
  
  dy <- y - ybar
  g <- expand.grid(dy, dy)
  yiyj <- g[,1] * g[,2]
  
  pm <- matrix(yiyj, ncol=n)
  
  pmw <- pm * w_matrix
  
  spmw <- sum(pmw)
  
  smw <- sum(w_matrix)
  sw  <- spmw / smw
  
  vr <- n / sum(dy^2)
  
  MI <- vr * sw
  
  EI <- -1/(n-1)
  
  return(list(morans_i = MI, expected_i = EI))
}

my_morans_i_fn(to_sites_lu_preds$resid_lm_all, inv_dist_mtx)
spdep::moran.test(to_sites_lu_preds$resid_lm_all, lw)

my_morans_i_fn(to_sites_lu_preds$resid_gam_all, inv_dist_mtx)
spdep::moran.test(to_sites_lu_preds$resid_gam_all, lw)

####################################################################################################
# GENERATE PREDICTION SURFACES #####################################################################
####################################################################################################
# the code below has been written to be stand-alone. 
  # that means that if you had already trained your models, you could load the packages at the top and then skip to this part 
  # you could have this code as a separate file in order to help you manage a project. I like doing that in order to have a control point

# create a raster of the study area #######
# read in polygon of study area. we'll use this to get the spatial extent of the raster
to_border_sf <- 
  readRDS("data/study_area/to_border_sf.rds")

# transform the crs so we can use meters
to_border_sf_utm <- 
  to_border_sf %>%
  st_transform(., crs = 32617)

# inspect the bounding box and use it for the spatial extent of the raster
st_bbox(to_border_sf_utm)
to_raster_limits <- 
  st_bbox(to_border_sf_utm %>% 
            st_buffer(., dist = 500)) # we'll have the raster go 500 m beyond the city limits
to_raster_limits[1]
to_raster <- raster(ncol = 85, nrow = 50, # i guessed at how many rows and columns of 500 m cells would be in the bbox. I was aiming for a raster with 500 m x 500 m resolution
                    xmn = to_raster_limits[1], 
                    xmx = to_raster_limits[3], 
                    ymn = to_raster_limits[2], 
                    ymx = to_raster_limits[4])

# we are aiming for 500 m x 500 m cells. I picked that resolution for computational speed during this workshop. depending on the pollutant of interest, your data, and your resources, you may chose a higher resoultion (e.g., 100 m x 100 m)
res(to_raster)  # we see the resolution isn't exactly 500 m x 500 m
res(to_raster) <- 500 # set it to 500 m x 500 m, this will change the exact number of rows, number of columns, and the xmax and ymin. it will not change the xmin and ymax (i.e. the top left corner stays fixed)
projection(to_raster) <- "+proj=utm +zone=17 +datum=WGS84" # we forgot to set the crs when creating the raster, so we can do it here
raster::values(to_raster) <- 1:ncell(to_raster) # give a cell id to each cell. this will be usefull later. 
names(to_raster) <- "cell_id"  # the default is "layer", set the cell id name to cell_id

tmap_mode("view")
tm_shape(to_border_sf_utm) +
  tm_polygons("purple", alpha = 0.5) +
  tm_shape(to_raster) +
  tm_raster(alpha = 0.5)

# get land use around each raster cell ########
# I have already done this for you. each row in this file represents a cell in the raster. we can link it to the raster via the cell_id column we created
to_fishnet_lu <- read_csv("data/land_use_data/to_fishnet_lu.csv")

# to_fishnet_lu is not a spatial object, but we can still do quick plots simply using lat and lon as y and x. 
ggplot(data = to_fishnet_lu, aes(x = lon, y = lat, col = green_200m)) +
  geom_point() + 
  scale_color_gradient(low = "white", high = "darkgreen") + 
  theme_bw()
# does the plot make sense? I'd say so. the blurriness is due in part to the resolution of the raster and the averaging of greenspace within the 200m buffer. plotting green_500m will be even more blury

# # FYI - you can transform our raster to vector geography. That is what I did in order to extract land use data around each cell. 
# to_fishnet <- st_as_sf(st_as_stars(to_raster), as_points = F, merge = T)
# # then you can get a buffer around each of the cells
# to_fishnet_500m <- st_buffer(to_fishnet, dist = 500)
# to_fishnet_200m <- st_buffer(to_fishnet, dist = 200)

# read in models ######
# we trained and saved the models in the code above. now we read them in. I like doing this in order to create workflow control points

# lm and gam trained on all data
lm_all_data <- readRDS("data/models/lm_all_data.rds")
gam_all_data <- readRDS("data/models/gam_all_data.rds")

# out of interest, we can also look into the each of the models trained during 4-fold cross-validation
train_set_gams_list <- readRDS("data/models/train_set_gams_list.rds")
train_set_lms_list <- readRDS("data/models/train_set_lms_list.rds")

# read in the sites for reference in some of the plots
to_sites_lu_sf <- 
  read_csv("data/land_use_data/to_sites_lu.csv") %>%
  st_as_sf(., coords = c("lon", "lat"), crs = 4326)

# predict ##########################################
# now we'll generate predictions for each cell and assign those values to the raster we created

# generate predictions for all the raster cells
to_fishnet_lu_pred <- 
  to_fishnet_lu %>% # the columns in this dataframe have the same names as the variables in the lur models. it does not matter that there are extra columns that are not in the lur model objects. it would matter and results in errors if the columns had different names or if there were variables in the model objects that were not in the dataframe
  mutate(pred_pm25_lm = predict(lm_all_data, .),  # generate linear model predictions for each cell in the raster
         pred_pm25_gam = predict(gam_all_data, .) %>% as.vector()) %>% # generate gam predictions for each cell in the raster
  arrange(cell_id) # it probably didn't change the order of the rows, but I compulsively make sure that they are in the correct order!

# create two copies of same raster
to_raster_lm <- to_raster_gam <- to_raster

# assign prediction values to each of the rasters. 
# they are assigned from left to right, top to bottom, starting in the top right corner. 
# this is the same order that cell_id was created, so if to_fishnet_lu_pred rows are in cell_id order, then they be will assigned to the correct cell
  # we don't have missing values in any of our data, but be aware that they can cause issues
raster::values(to_raster_lm) <- to_fishnet_lu_pred$pred_pm25_lm
raster::values(to_raster_gam) <- to_fishnet_lu_pred$pred_pm25_gam

# now create a raster stack, which is a raster with multiple layers
to_raster_stack <- stack(to_raster, to_raster_lm, to_raster_gam); to_raster_stack

names(to_raster_stack) <- c("cell_id", "pred_pm25_lm", "pred_pm25_gam"); to_raster_stack

# take a look at the two prediction layers. notice similarities? differences?
tm_shape(to_border_sf_utm) +
  tm_polygons("gray", alpha = 0.1) +
  tm_shape(dropLayer(to_raster_stack, 1)) +  # we'll drop the cell_id layer so our color scale is useful. you can try tm_shape(to_raster_stack) + here to see what happend to the color scale
  tm_raster(alpha = 0.5)

# you can also just inspect the gam layer
tm_shape(to_border_sf_utm) +
  tm_polygons("lightblue", alpha = 0.1) +
  tm_shape(raster(to_raster_stack, layer = "pred_pm25_gam")) + # instead of dropping a layer, we just retain a single layer
  tm_raster(alpha = 0.5)

# use mask() to get rid of cells beyond the borders. the first argument is the raster and the second is the vector, but instead of an sf objects, it needs to be a spatial object  
to_raster_stack_all_data <- 
  raster::mask(to_raster_stack, as_Spatial(to_border_sf_utm %>% st_buffer(., dist = 100))) # set a buffer so we don't drop too many cells along the border

tm_shape(to_border_sf_utm) +
  tm_polygons("lightblue", alpha = 0.1) +
  tm_shape(dropLayer(to_raster_stack_all_data, 1)) +
  tm_raster(alpha = 0.5) + 
  tm_shape(to_sites_lu_sf) +
  tm_dots("purple")  # add our monitoring sites

# just like we did for the "all_data" models, make stacks for each of the 4 folds lm() models. 
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

to_raster_stack_lm_ho <- 
  raster::mask(to_raster_stack_lm_ho, as_Spatial(to_border_sf_utm %>% st_buffer(., dist = 100)))

# let's use more pleasing colours this time. I'm a fan of red is bad, green is good.
# tmaptools::palette_explorer() # this will show colour palettes
# click "stop" in veiwer when done

tm_shape(to_border_sf_utm) +
  tm_polygons("lightblue", alpha = 0.1) +
  tm_shape(to_raster_stack_lm_ho) +
  tm_raster(alpha = 0.7, palette = "-RdYlGn", breaks = seq(5.5, 9, by = 0.25)) + 
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
  tm_raster(alpha = 0.7, palette = "-RdYlGn", breaks = seq(5.5, 9, by = 0.25)) + 
  tm_shape(to_sites_lu_sf) +
  tm_dots("purple")


tm_shape(to_border_sf_utm) +
  tm_polygons("lightblue", alpha = 0.1) +
  tm_shape(mean(to_raster_stack_gam_ho)) +  # you can also look at the average of the layers. this is one of the advantages of using rasters, you can do really fast and simple math on the layers
  tm_raster(alpha = 0.7, palette = "-RdYlGn", breaks = seq(5, 9, by = 0.25)) + 
  tm_shape(to_sites_lu_sf) +
  tm_dots("purple")


# now you have some surfaces:
mean(to_raster_stack_gam_ho) # mean predictions from the 4 gam folds
mean(to_raster_stack_lm_ho) # mean predictions from the 4 lm folds
to_raster_stack_all_data # predictions from lm and gams trained on all the data (there would be our final models)

# you could change them to polygons:
to_fishnet_gam_ho <- st_as_sf(st_as_stars(to_raster_stack_gam_ho), as_points = F, merge = T)
# notice is only does the first layer

# you have land use information you can link to it using cell_id:
left_join(to_fishnet_gam_ho %>% mutate(cell_id = 1:nrow(to_fishnet_gam_ho)), to_fishnet_lu)

# instead of using tmap, use ggplot to make nice plot for the presentation
gam_surface_for_presentation <- 
  ggplot(data = to_border_sf_utm) +
  geom_sf() +
  geom_tile(data = as.data.frame(as(to_raster_stack_all_data, "SpatialPixelsDataFrame")), 
            aes(x = x, y = y, fill = pred_pm25_gam), show.legend = F) +
  scale_fill_distiller(palette =  "RdYlGn") +
  theme_void(); gam_surface_for_presentation
# the spatial variation kind of gets lost here

# try transforming the color scale
gam_surface_for_presentation_exp_col <-
  ggplot(data = to_border_sf_utm) +
  geom_sf() +
  geom_tile(data = as.data.frame(as(to_raster_stack_all_data, "SpatialPixelsDataFrame")), 
            aes(x = x, y = y, fill = pred_pm25_gam), show.legend = F) +
  scale_fill_distiller(palette =  "RdYlGn", trans = "exp") + # transform the color scale to make it "pop"
  theme_void()

# let's see them side by side
gridExtra::grid.arrange(gam_surface_for_presentation, gam_surface_for_presentation_exp_col)
# it's the same data, but which one is "better"?

################################################################################################
# if we have time, fun with rayshader ##########################################################
  # more info here: https://www.rayshader.com/
  # this is fun because it gives a "feel" to the map. science is a social act and I find this helps to engage people
to_fishnet_gam <- st_as_sf(st_as_stars(raster(to_raster_stack_all_data, layer = 3)), as_points = F, merge = T)

fishnet_gg <- 
  ggplot(to_fishnet_gam) +
  geom_sf(aes(fill = pred_pm25_gam), lwd = 0) +
  scale_fill_distiller(trans = "exp", palette = "RdYlGn", direction = -1, aesthetics = "fill", breaks = seq(5, 9, by = 0.25)) +
  theme(legend.position = "none",   # you can play around with the settings here
        axis.line=element_blank(),
        axis.text.x=element_blank(), axis.title.x=element_blank(),
        axis.text.y=element_blank(), axis.title.y=element_blank(),
        axis.ticks=element_blank(),
        panel.background = element_blank()) +
  # theme_bw() +
  xlab("Longitude") + ylab("Latitude") + labs(fill = "Predicted PM2.5 Concentration (ug/m\u00B3)")

# this will take a moment. a window should open up. the window is interactive
plot_gg(fishnet_gg,
        width=12,height=7,scale=750,windowsize=c(1000, 600), solid = FALSE,
        raytrace=FALSE, zoom = 0.35, phi = 35, theta = 20, triangulate = TRUE, max_error = 0)

# can save that view as an image
render_snapshot("figures/rayshader_pm25_pred_exp.png")

# you can also make a movie!
render_movie(  title_text = "Toronto Predicted Outdoor PM2.5 Concentrations",
               title_offset = c(20, 20),
               title_color = "black",
               title_size = 40,
               title_font = "sans",
               fps = 30, frames = 720,
               filename = "figures/rayshader_pm25_pred_exp")

# if you're feeling extra saucy, you can also try this: https://www.tylermw.com/datacoaster-tycoon/
  # it makes a video of a rollercoaster riding around your study area


# misc presentation plots #####

# google slides wants .png, publications may want .tiff, .pdg, or .svg
ggsave("figures/gam_pred_surface_exp.png",
       gam_surface_for_presentation_exp_col,
       width = 8, height = 6, units = "in", dpi = 300)

# misc slide plots
ggplot(data = to_border_sf_utm) +
  geom_sf(fill = "grey80") +
  theme_void()
ggsave("figures/blank_to.png",
       width = 4, height = 3, units = "in", dpi = 300)

ggplot(data = to_border_sf_utm) +
  geom_sf(fill = "grey60") +
  geom_sf(data = to_sites_lu_sf, col = "purple") +
  theme_void()
ggsave("figures/sites.png",
       width = 4, height = 3, units = "in", dpi = 300)

# land use downloaded
temp <- tempfile()
download.file(url = "https://ckan0.cf.opendata.inter.prod-toronto.ca/dataset/1d079757-377b-4564-82df-eb5638583bfb/resource/7bc94ccf-7bcf-4a7d-88b1-bdfc8ec5aaf1/download/Centreline%20-%20Version%202.geojson", 
              destfile = temp)
centerline_sf <- 
  st_read(temp) %>%
  st_transform(., crs = 32617)

ggplot(data = to_border_sf_utm) +
  geom_sf(fill = "grey80") +
  geom_sf(data = centerline_sf %>% filter(FEATURE_CODE_DESC == "Major Arterial"), col = "darkblue") +
  theme_void()
ggsave("figures/maj_roads.png",
       width = 4, height = 3, units = "in", dpi = 300)

temp <- tempfile()
download.file(url = "https://ckan0.cf.opendata.inter.prod-toronto.ca/dataset/9a284a84-b9ff-484b-9e30-82f22c1780b9/resource/7a26629c-b642-4093-b33c-a5a21e4f3d22/download/Green%20Spaces.geojson", 
              destfile = temp)

to_greenspace_sf <- 
  st_read(temp) %>% # read in the .shp file using st_read()
  st_union() %>% # unify all the simple features into one simple feature
  st_transform(., crs = 32617) # transform into a projected crs, this will allow us to work in meters

ggplot(data = to_border_sf_utm) +
  geom_sf(fill = "grey80") +
  geom_sf(data = to_greenspace_sf, fill = "green") +
  theme_void()
ggsave("figures/green_space.png",
       width = 4, height = 3, units = "in", dpi = 300)

temp <- tempfile()
download.file(url = "https://ckan0.cf.opendata.inter.prod-toronto.ca/dataset/34927e44-fc11-4336-a8aa-a0dfb27658b7/resource/d75fa1ed-cd04-4a0b-bb6d-2b928ffffa6e/download/Zoning%20Area%20-%204326.geojson", 
              destfile = temp)

to_ind_sf <- 
  st_read(temp) %>%
  st_transform(., crs = 32617) %>% 
  filter(GEN_ZONE == 4) %>% # from the metadata, GEN_ZONE == 4 is industrial area
  filter(st_is_valid(.)) %>%  # some of the shapes in the shapefile are not valid, often this is a polygon that doesn't close on itself. we could inspect to see exactly what's going on here. 
  st_union()

ggplot(data = to_border_sf_utm) +
  geom_sf(fill = "grey80") +
  geom_sf(data = to_ind_sf, fill = "red") +
  theme_void()
ggsave("figures/industrial.png",
       width = 4, height = 3, units = "in", dpi = 300)

lur_ex_sites <- to_sites_lu_sf %>% filter(site_id %in% c(90, 84, 75)) %>% st_transform(., crs = 32617) %>% mutate(pm25 = paste0(round(pm25, 2), " ug/m\U00B3"))
lur_ex_site_buffer <- lur_ex_sites %>% st_buffer(., dist = 500)
lur_ex_bbox <-  lur_ex_sites %>% st_buffer(., dist = 1000) %>% st_bbox()
lur_ex_border <- st_crop(to_border_sf_utm, lur_ex_bbox)
lur_ex_green <- st_crop(to_greenspace_sf, lur_ex_bbox)
lur_ex_ind <- st_crop(to_ind_sf, lur_ex_bbox)
lur_ex_road <- st_crop(centerline_sf %>% filter(FEATURE_CODE_DESC %in% c("Local", "Collector", "Collector Ramp", "Laneway", "Minor Arterial")), lur_ex_bbox)
lur_ex_ind <- st_crop(to_ind_sf, lur_ex_bbox)

ggplot() +
  geom_sf(data = lur_ex_border, fill = "grey80") +
  geom_sf(data = lur_ex_green, fill = "green", alpha = 0.8) +
  geom_sf(data = lur_ex_ind, fill = "red", alpha = 0.8) +
  geom_sf(data = lur_ex_road, col = "darkblue") +
  geom_sf(data = lur_ex_site_buffer, fill = "white", alpha = 0.5) +
  geom_sf(data = lur_ex_sites, col = "black", size = 3) +
  geom_sf_label(data = lur_ex_sites, aes(label = pm25), nudge_y = -175) + 
  theme_bw() + 
  xlab("Longitude") + ylab("Latitude")
ggsave("figures/lur_ex.png",
       width = 10, height = 5.5, units = "in", dpi = 300)






