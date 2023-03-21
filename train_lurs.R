library(spdep)
library(ape)
library(purrr)
library(sf)
library(broom)
library(mgcv)
library(mgcViz)
library(tidyverse)

#######################################################
# READ ME ###
# this file trains lm and gam lur models and then evaluates them
# input: 
  # sites with monitoring data and land use (to_sites_lu.csv)
  # study area sf object just for reference/exploratory plots
# output:
  # .rds of various models
  # it also generates several plots on model diagnosticsin which could be exported
#######################################################

to_sites_lu <- read_csv("data/land_use_data/to_sites_lu.csv")


# variable selection ####
uni_lin_regs <- 
  to_sites_lu %>%
  pivot_longer(cols = green_200m:distance_npri_nox, names_to = "lu_var", values_to = "value") %>%  # make the wide data long
  split(.$lu_var) %>%   # turn the data frame into a list of data frames. each data frame in this list will have a single land use variable
  map(., ~lm(data = ., bc ~ value)) %>%  # for each data frame in the list, regress the land use variable values onto the outcome of interest. 
  map(., ~broom::glance(.)) %>% # glance() summarizes the linear model
  imap(., ~mutate(.x, lu_var = .y)) %>% # imap() is like map() except is also uses the names of the items in the list. here we are using the names to create a new column that is filled with the names
  bind_rows() %>%  # bind the rows of all the items in the list. this gets us ba
  dplyr::select(lu_var, r2 = r.squared, logLik, AIC, pval = p.value) # we are only interested in certain measures. We could also calculate RMSE here

# if two buffers of the same variable are selected (e.g., busstop_200m and busstop_500m), then we only want to keep the one with the highest R2
selected_vars_df <- 
  uni_lin_regs %>%
  filter(pval < 0.05) %>%  # just look at the land use variables that are associated with the outcome
  mutate(var_name = str_remove(lu_var, "_[:digit:]00m")) %>%  # create a new column for the names that removes the part of the name that indicates the size of the buffer. this will allow us to 
  split(.$var_name) %>% # split by the land use names (without the buffer). this groups them together so we can take the more useful buffer
  map(., ~arrange(., desc(r2)) %>% slice(., 1)) %>%  # for each land use variable, sort by the value of r2, and then select the top row (i.e. the highest value of r2)
  bind_rows() %>%
  dplyr::select(-var_name)


# are any being driven by outliers?
# repeat but with the outliers removed
uni_lin_regs_no_outs <- 
  to_sites_lu %>%
  pivot_longer(cols = green_200m:distance_npri_nox, names_to = "lu_var", values_to = "value") %>%
  split(.$lu_var) %>%
  map(., ~mutate(., mean_val = mean(value), sd_val = sd(value), std_val = (value - mean_val)/sd_val)) %>%  # calculate standardized value for each land use variable, 
  map(., ~filter(., abs(std_val) < 2)) %>%  # remove any observations that are greater that 2 sds from the mean
  map(., ~lm(data = ., bc ~ value) %>% glance()) %>%
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
          selected_vars_df %>% dplyr::select(lu_var, r2 = r2))
# any with NA in the no_outliers_r2 column are diven by outliers
# need to decide what to do. keep only those that are both?
plot(to_sites_lu$bc ~ to_sites_lu$road_500m)

lu_var_to_remove <- 
  outlier_check %>% 
  filter(is.na(no_outliers_r2)) %>% pull(lu_var) # c("npri_pm_500m", "open_500m", "road_500m")

# see if any of the distance_to that didn't make it in are non-linear
uni_nlin_regs <- 
  to_sites_lu %>%
  dplyr::select(lon, lat, bc, contains("distance"), -str_subset(selected_vars_df$lu_var, "dist" )) %>%
  pivot_longer(cols = contains("distance"), names_to = "dist_lu_var", values_to = "value") %>%
  split(.$dist_lu_var) %>%
  map(., ~lm(data = ., bc ~ value + I(value^2)) %>% glance()) %>%
  imap(., ~mutate(.x, dist_lu_var = .y)) %>%
  bind_rows() %>%
  dplyr::select(dist_lu_var, r2 = r.squared, logLik, AIC, pval = p.value)


# check correlations between variables
lu_cor_mtx <- 
  to_sites_lu %>%
  dplyr::select(selected_vars_df$lu_var) %>%  # keep only the variables we have selected so far
  cor(., method = "spearman") # this returns a matrix of correlations between all variables

diag(lu_cor_mtx) <- 0  # the diagonal is the correlations of variables with themselve, which is 1. set to 0 because we don't want to see them when we are looking at high correlations

lu_hi_cor <- 
  lu_cor_mtx %>%
  as.data.frame() %>%
  mutate_all(., ~. > 0.7) %>%  # we want to identify pairs with correlation > 0.7
  filter_all(., any_vars(. == TRUE)) %>%  # keep the rows with high correlations
  t() %>%  # transpose to make the columns rows and vice versa
  as.data.frame() %>%
  filter_all(., any_vars(. == TRUE)) # keep the rows (which were columns) with high correlations

# so we see that bus routes and bus stops are highly correlated (not surprising)
# we also see that green and open are highly correlated (also not surprising)
# remove the one from each pair, remove the one with lower r2

selected_vars_df %>% 
  filter(lu_var %in% colnames(lu_hi_cor))
# remove busroutes and remove open (open was already removed due to outliers)
lu_var_to_remove <- c(lu_var_to_remove, "busroutes_500m")

# it's handy to have an object of all the variable names
lur_vars <- 
  selected_vars_df %>%
  filter(!lu_var %in% lu_var_to_remove) %>% # on your own, try repeating this process without removing these variables, what happens?
  pull(lu_var)  # just pull the land use variable name column

# train linear model - add RMSE to model results #####

# check the distribution of the outcome. if it is skewed, we may consider log-transforming it
hist(to_sites_lu$bc)

# we'll create a formula object that we can use any time we use lm()
# we keep longitude and latitude in it in order to model the spatial dependencies that are not captured by the land use parameters
lm_lur_formula <- as.formula(paste0("bc ~ ", paste(lur_vars, collapse = " + "), " + lon + lat"))
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
hist(to_sites_lu$set) # verify it's a nice looking split

# now we can train four different linear models, one for each hold out set. This is cross validation 
train_set_lms_list <- 
  purrr::map(as.list(my_sets), ~filter(to_sites_lu, set != .)) %>% 
  # map(., ~select(., site_id, set)) # uncomment to help us see what previous line is doing. Basically creating a list where each item in the list is the monitoring data with one of the sets held out
  setNames(., paste0("ho_", my_sets)) %>%  # give a name to each item in the list. using "ho" as "hold-out"
  map(., ~lm(data = ., lm_lur_formula))  # fit a lm() on each item in the list

# now generate predictions in the test sets
lm_pred_test_sets <- 
  map(as.list(my_sets), ~filter(to_sites_lu, set == .)) %>% # map(., ~select(., site_id, set)) # create a list of only the hold out sets
  setNames(., paste0("test_", my_sets)) %>%
  map2(., train_set_lms_list, ~mutate(.x, pred_lm = predict(.y, .x))) %>% # map2() takes two lists, so use the list of linear models to generate predictions in the list of hold out sets
  map(., ~mutate(., resid_lm = bc - pred_lm)) # get the residuals

# a quick verification that we did what we wanted to do
to_sites_lu %>% filter(set == 4) %>%
  predict(train_set_lms_list$ho_4, .)
lm_pred_test_sets$test_4 %>% pull(pred_lm)

# now get the r2 value in the test sets
lm_ho_model_eval <- 
  lm_pred_test_sets %>%
  map(., ~lm(data = ., bc ~ pred_lm) %>% glance()) %>% # compare predicted to observed
  bind_rows() 

lm_ho_model_eval %>%
  pull(r.squared) %>% mean()

# use each of the hold out models to generate predictions in the entire dataset. This is for visualizations later. 
to_sites_lu_lm_preds <- 
  map(train_set_lms_list, ~mutate(to_sites_lu, pred_lm = predict(., to_sites_lu))) %>%
  imap(., ~mutate(.x,
                  resid_lm = bc - pred_lm,
                  train_on = .y)) %>%
  bind_rows() %>% 
  pivot_wider(., names_from = "train_on", values_from = c("pred_lm", "resid_lm")) %>%
  mutate(mean_pred_lm = rowMeans(dplyr::select(., starts_with("pred_")))) %>%
  mutate(mean_resid_lm = rowMeans(dplyr::select(., starts_with("resid_")))) %>%
  mutate(pred_lm_all = predict(lm_all_data, .),
         resid_lm_all = bc - pred_lm_all)

to_sites_lu_lm_preds %>%
  pivot_longer(cols = c(bc, pred_lm_ho_1:pred_lm_ho_4, mean_pred_lm, pred_lm_all)) %>%
  ggplot(data =., aes(x = value, col = name, fill = name)) +
  geom_density(alpha = 0.5) + 
  theme_bw()

# instead of linear models, train Generalized Additive Models, but only to allow more flexibility on the lat and long #####
gam_lur_formula <- as.formula(paste0("bc ~ ", paste(lur_vars, collapse = " + "), " + te(lon, lat, k = 7)")) # te(lon, lat, k = 7) is a tensor producti of lat and long. bascially an interaction term. k = 7 is the number of basis functions, which it the level of flexibility. higher is more flexible, but we run out of degrees of freedom. you will get an error if it's too high    
gam_all_data <- mgcv::gam(data = to_sites_lu, gam_lur_formula, method = "REML") # train on all data

# train on 4 folds
train_set_gams_list <- 
  map(as.list(my_sets), ~filter(to_sites_lu, set != .)) %>%
  setNames(., paste0("ho_", my_sets)) %>%
  map(., ~gam(data = ., gam_lur_formula, method = "REML"))

gam_pred_test_sets <- 
  map(as.list(my_sets), ~filter(to_sites_lu, set == .)) %>%
  setNames(., paste0("test_", my_sets)) %>%
  map2(., train_set_gams_list, ~mutate(.x, pred_gam = predict(.y, .x))) %>%
  map(., ~mutate(., resid_gam = bc - pred_gam))

to_sites_lu %>% filter(set == 4) %>%
  predict(train_set_gams_list$ho_4, .)
gam_pred_test_sets$test_4

gam_ho_model_eval <- 
  gam_pred_test_sets %>%
  map(., ~lm(data = ., bc ~ pred_gam) %>% glance()) %>%
  bind_rows() 

gam_ho_model_eval %>%
  pull(r.squared) %>% mean()

to_sites_lu_gam_preds <- 
  map(train_set_gams_list, ~mutate(to_sites_lu, pred_gam = predict(., to_sites_lu))) %>%
  imap(., ~mutate(.x, 
                  resid_gam = bc - pred_gam,
                  train_on = .y)) %>%
  bind_rows() %>%
  pivot_wider(., names_from = "train_on", values_from = c("pred_gam", "resid_gam")) %>%
  mutate(mean_pred_gam = rowMeans(dplyr::select(., starts_with("pred_gam")))) %>%
  mutate(mean_resid_gam = rowMeans(dplyr::select(., starts_with("resid_gam")))) %>%
  mutate(pred_gam_all = predict(gam_all_data, .),
         resid_gam_all = bc - pred_gam_all)

to_sites_lu_gam_preds %>%
  pivot_longer(cols = c(bc, pred_gam_ho_1:pred_gam_ho_4, mean_pred_gam, pred_gam_all)) %>%
  ggplot(data =., aes(x = value, col = name, fill = name)) +
  geom_density(alpha = 0.5) + 
  theme_bw()

# save models
saveRDS(gam_all_data, "data/models/gam_all_data.rds")
saveRDS(lm_all_data, "data/models/lm_all_data.rds")
saveRDS(train_set_gams_list, "data/models/train_set_gams_list.rds")
saveRDS(train_set_lms_list, "data/models/train_set_lms_list.rds")


# model checks #####
# https://www.r-bloggers.com/2019/11/the-hidden-diagnostic-plots-for-the-lm-object/
# https://data.library.virginia.edu/diagnostic-plots/
# http://www.sthda.com/english/articles/39-regression-model-diagnostics/161-linear-regression-assumptions-and-diagnostics-in-r-essentials/
# https://book.stat420.org/model-diagnostics.html

# lm
par(mfrow = c(1,1))
par(mfrow = c(4,4))
for(i in 1:4){
  for(j in 1:4){
    plot(train_set_lms_list[[i]], which = j, main = paste0("hold_out_set_", i))
  }
}

par(mfrow = c(1,4))
for(j in 1:4){
  plot(lm_all_data, which = j, main = paste0("all_data"))
}

# inspect Cook's distance points from each lm
lm_cooks_points <- list(30, 37, 41, 23)
lm_qq_points <- list(c(6, 34, 40), c(19, 25, 40), c(9, 26, 29), c(9, 16, 20))

map(lm_train_sets, ~.$model) %>%
  map2(lm_cooks_points, ., ~slice(.y, .x)) %>%
  imap(., ~mutate(.x, model_name = .y) %>% relocate(model_name)) %>%
  bind_rows() %>% 
  arrange(bc)

# gam 
par(mfrow = c(1,1))
par(mfrow = c(4,4))
gam.check(train_set_gams_list$ho_1)
gam.check(train_set_gams_list$ho_2)
gam.check(train_set_gams_list$ho_3)
gam.check(train_set_gams_list$ho_4)

# also check the lat
check(getViz(gam_all_data))

vis.gam(gam_all_data, c("lon", "lat"), color = "terrain", plot.type = "contour",
        theta = 0, phi = 45)
vis.gam(gam_all_data, c("lon", "lat"), color = "terrain", plot.type = "persp",
        theta = 0, phi = 45)
vis.gam(train_set_gams_list$ho_1, c("lon", "lat"), color = "terrain", plot.type = "persp",
        theta = 0, phi = 45)
vis.gam(train_set_gams_list$ho_2, c("lon", "lat"), color = "terrain", plot.type = "persp",
        theta = 0, phi = 45)
vis.gam(train_set_gams_list$ho_3, c("lon", "lat"), color = "terrain", plot.type = "persp",
        theta = 0, phi = 45)
vis.gam(train_set_gams_list$ho_4, c("lon", "lat"), color = "terrain", plot.type = "persp",
        theta = 0, phi = 45)



# check spatial clustering of residuals #####


to_sites_lu_preds <- 
  to_sites_lu_lm_preds %>% 
  left_join(to_sites_lu_gam_preds %>% dplyr::select(site_id, pred_gam_ho_1:resid_gam_all))

to_sites_lu_preds_sf <- 
  to_sites_lu_preds %>%
  st_as_sf(., coords = c("lon", "lat"), crs = 4326, remove = F) %>%
  st_transform(., crs = 32617) %>%
  mutate(lon_utm = st_coordinates(.)[,1], lat_utm = st_coordinates(.)[,2])

# eyeball test
tm_shape(to_sites_lu_preds_sf) + 
  tm_dots("resid_lm_all")

tm_shape(to_sites_lu_preds_sf) + 
  tm_dots("resid_gam_all")

# passed or failed?

# Calculate and test Moran's I #####
nb_object <- dnearneigh(to_sites_lu_preds_sf, 0, 100000) # choice of upper distance affects results
lw <- nb2listwdist(nb_object, to_sites_lu_preds_sf, type = "idw")
try_mtest <- moran.test(to_sites_lu_preds$resid_lm_ho_3, lw)
try_mtest$estimate

moran_result_df_fn <- function(x){
  p_val <- x$p.value
  est <- x$estimate[1]
  exp <- x$estimate[2]
  var <- x$estimate[3]
  stat <- x$statistic
  fn_output <- data.frame(est, exp, p_val, var, stat)
  return(fn_output)
}

# make a list and a function

moran_i_results <-
  to_sites_lu_preds %>%
  dplyr::select(contains("resid_")) %>%
  pivot_longer(cols = contains("resid")) %>%
  split(.$name) %>%
  map(., ~pull(., value) %>% moran.test(., lw)) %>%
  map(., ~moran_result_df_fn(.)) %>%
  imap(., ~mutate(.x, model = .y)) %>%
  bind_rows() %>%
  mutate(resids_clstrd = case_when(p_val < 0.05 ~ "yes",
                                   TRUE ~ "no")) %>%
  mutate(model_type = str_extract(model, "lm|gam")) %>%
  relocate(c(model, model_type, resids_clstrd)) %>%
  arrange(model_type)

moran_i_results



# manual moran's I

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
my_morans_i_fn(to_sites_lu_preds$resid_gam_all, inv_dist_mtx)







