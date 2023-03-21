library(purrr)
library(broom)
library(tidyverse)

# short example of map() from purrr #####
data("mtcars")
data("iris")
my_lil_list <- list(my_cars = mtcars, my_flowers = iris) # make a list of dfs
my_lil_list
my_lil_list[[2]] %>% head()
my_lil_list[[2]] %>% slice(1,10:15)
my_lil_list$my_flowers %>% head()
my_lil_list %>% pluck(., "my_flowers") %>% head()


map(my_lil_list, ~head(.)) # take the list and apply the head() function to each element of the list. map() is like mapply, lapply, etc, but is supposed to be a little more consistent
map(my_lil_list, ~select(., 1:3))
map(my_lil_list, ~select(., 1:3) %>% head()) # could combine functions in a pipe, but once you start doing that, you may as well write a function

my_lil_list %>%
  map(., ~select(., 1:3)) %>%
  map(., ~head(.))

my_lil_fn <-
  function(x, from_col, to_col){
    x %>%
      select(., from_col:to_col) %>%   #could we make this more flexible?
      head()
  }

map(my_lil_list, ~my_lil_fn(., 1, 3))

# example with iris data set
iris %>%
  split(.$Species) %>%
  map(., ~lm(data = ., Sepal.Length ~ Sepal.Width)) %>%
  map(., ~tidy(.))

iris %>%
  nest(nest_data = c(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)) %>%
  mutate(lm_fit = map(nest_data, ~lm(data = ., Sepal.Length ~ Sepal.Width)),
         tidy_results = map(lm_fit, ~tidy(.))) %>%
  unnest(tidy_results) %>%
  split(.$Species)

# can also use glance() instead of tidy()
iris %>%
  split(.$Species) %>%
  map(., ~lm(data = ., Sepal.Length ~ Sepal.Width)) %>%
  map(., ~glance(.))