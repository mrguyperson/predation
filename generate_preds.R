library('tidyverse')
library('plyr')

number_of_bass <- 300
transect_length  <- 1000
n_transects  <- 2
grid_side <- 5

create_preds <- function(number_of_bass, transect_length, n_transects){
  data.frame(distance_from_shore = runif(n_transects * number_of_bass, min = 0, max = 5), 
             distance_downstream = runif(n_transects * number_of_bass, min = 0, max = n_transects * transect_length)) %>%
    arrange(distance_downstream)
}

grid_num <- function(x, grid_side){
  grid_val <- round_any(x, grid_side, ceiling)
  return(grid_val/grid_side)
  
}

add_grid_nums <- function(df, x, grid_side){
  df %>% mutate(grid = grid_num(distance_downstream))
}


driver <- function(number_of_bass, transect_length, n_transects, grid_side){
  df <- create_preds(number_of_bass, transect_length, n_transects)
  df %>% mutate(grid = grid_num(distance_downstream, grid_side))

}

df <- driver(number_of_bass, transect_length, n_transects, grid_side)

ggplot(df, aes(x = distance_downstream)) +
  theme_classic(base_size = 30) +
  labs(y = "Distance from shore (m)", x = "Distance downstream (m)") +
  geom_point(aes(y = distance_from_shore))