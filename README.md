# lur_model_workshop
Develop, evaluate, and use land use regression models.

This workshop simulates a fixed site pollution monitoring campaign (PM2.5, O3, or Noise) by using CANUE data. CANUE data can be accessed here: https://www.canuedata.ca/

Other data sources could be used to simulate the monitoring campaign. 

Run the files in this order:
1. generate_monitoring_sites.R
2. dload_clean_compile_lu_data.R (this file calls extract_lu_functions.R)
3. extract_site_lu.R
4. create_raster_extract_lu.R
5. train_lurs.R
6. generate_prediction_surfaces.R

ghn_lur_workshop.R is an abbreviated version of the code. It uses the output of items 1-4 to train LURs and generate prediction surfaces.

quick_purrr_intro.R demonstrates purrr functions. 
