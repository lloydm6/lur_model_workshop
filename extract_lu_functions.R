
library(sf)
library(tidyverse)

# area within; lu_sf must be a single multipolygon
area_near_site_fn <- 
  function(buffer_sf, lu_sf, var_name){
    
    # figure out with site buffers intersect with any of the land use
    has_intx <- st_intersects(buffer_sf, lu_sf, sparse = F) %>% as.vector()
    
    # create the new column with NAs
    buffer_sf[[var_name]] <- rep(NA, length(has_intx))
    
    # for the sites that do intersect with land use, calculate the area of the intersection
    # st_intersection is used because it returns an sf object of the intersection
    buffer_sf_area <- buffer_sf[has_intx, ]
    buffer_sf_area[[var_name]] <- st_area(st_intersection(buffer_sf_area, lu_sf)) %>% as.vector()
    
    # for the sites that do not intersect with land use, put the area as 0
    buffer_sf_noarea <- buffer_sf[!has_intx, ]
    buffer_sf_noarea[[var_name]] <- rep(0, nrow(buffer_sf_noarea))
    
    # combine and return
    buffer_sf <- bind_rows(buffer_sf_area, buffer_sf_noarea)
    return(buffer_sf)
    
  }

# length within; lu_sf must be a single multilinestring
length_near_site_fn <- 
  function(buffer_sf, lu_sf, var_name){
    
    # figure out with site buffers intersect with any of the land use
    has_intx <- st_intersects(buffer_sf, lu_sf, sparse = F) %>% as.vector()
    
    # create the new column with NAs
    buffer_sf[[var_name]] <- rep(NA, length(has_intx))
    
    # for the sites that do intersect with land use, calculate the length of the intersection
    # st_intersection is used because it returns an sf object of the intersection
    buffer_sf_length <- buffer_sf[has_intx, ]
    buffer_sf_length[[var_name]] <- st_length(st_intersection(buffer_sf_length, lu_sf)) %>% as.vector()
    
    # for the sites that do not intersect with land use, put the area as 0
    buffer_sf_nolength <- buffer_sf[!has_intx, ]
    buffer_sf_nolength[[var_name]] <- rep(0, nrow(buffer_sf_nolength))
    
    # combine and return
    buffer_sf <- bind_rows(buffer_sf_length, buffer_sf_nolength)
    return(buffer_sf)
    
  }

# we could probably combine the two functions above using class(lu_sf)[1] %>% str_detect(., "STRING") to evalutate if lu_sf is a linestring or a polygon   

# number within; lu_sf must be points with each row being a single point
number_near_site_fn <- 
  function(buffer_sf, lu_sf, var_name){
    
    num_within <- 
      st_intersects(buffer_sf, lu_sf) %>% # default is sparse = T, it will list all the lu_sf that intersect with buffer_sf
      map(., ~length(.)) %>% # the length of the lists in the list is the number of intersections 
      unlist()
    
    # add the new column and return
    buffer_sf[[var_name]] <- num_within
    return(buffer_sf)
    
  }

# distance to; lu_sf must be a single multipoint
distance_to_site_fn <- 
  function(buffer_sf, lu_sf, var_name){
    
    buffer_sf[[var_name]] <- st_distance(st_centroid(buffer_sf), lu_sf) %>% as.vector()
    
    return(buffer_sf)
    
  }