
##############################################################################################################
# PACKAGES #######################################################
# create a vector of the packages (i.e. librairies) we will use
required_packages <- c("purrr", 
                      "sf", 
                      "broom", 
                      "Hmisc", 
                      "mgcv", 
                      "mgcViz", 
                      "spdep", 
                      "ape",
                      "tmap",
                      "tidyverse")

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
# this file trains lm and gam lur models and then evaluates them
# input: 
  # sites with monitoring data and land use (to_sites_lu.csv)
  # study area sf object just for reference/exploratory plots
# output:
  # .rds of various models
  # it also generates several plots on model diagnostics which could be exported
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
  geom_density(alpha = 0.5) + xlab("PM2.5 Concentration (ug/m\U00B3)") +
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
  geom_density(alpha = 0.5) +  + xlab("PM2.5 Concentration (ug/m\U00B3)") +
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
ggsave("geohealth_network_lur_presentation_files/slide_images/residuals_plot.png",
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
try_mtest <- spdep::moran.test(to_sites_lu_preds$resid_lm_ho_3, lw)
try_mtest

# write a function to extract information from the moran.test() in a format that we like
moran_result_df_fn <- function(x){
  p_val <- x$p.value
  observed_mi <- x$estimate[1]
  expected_mi <- x$estimate[2]
  variance <- x$estimate[3]
  stat <- x$statistic
  fn_output <- data.frame(observed_mi, expected_mi, p_val, variance, stat)
  row.names(fn_output) <- NULL
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






