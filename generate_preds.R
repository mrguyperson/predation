library('tidyverse')
library('plyr')

number_of_bass <- 300
lit_zone_size <- 5
channel_width <- 100
transect_length  <- 1000
n_transects  <- 1
grid_side <- 5

left_bank_preds <- function(number_of_bass, transect_length, n_transects, lit_zone_size){
  data.frame(distance_from_shore = runif(n_transects * number_of_bass / 2, min = 0, max = lit_zone_size), 
             distance_downstream = runif(n_transects * number_of_bass / 2, min = 0, max = n_transects * transect_length)) %>%
    arrange(distance_downstream)
}

right_bank_preds <- function(number_of_bass, transect_length, n_transects, lit_zone_size, channel_width){
  data.frame(distance_from_shore = runif(n_transects * number_of_bass / 2, min = channel_width - lit_zone_size, max = channel_width), 
             distance_downstream = runif(n_transects * number_of_bass / 2, min = 0, max = n_transects * transect_length)) %>%
    arrange(distance_downstream)
}

number_of_stripers <- function(){
  runif(1,min = 0, max = 1200)
}

grid_num <- function(x, grid_side){
  grid_val <- round_any(x, grid_side, ceiling)
  return(grid_val/grid_side)
  
}

add_grid_nums <- function(df, x, grid_side){
  df %>% mutate(grid = grid_num(distance_downstream))
}


driver <- function(number_of_bass, transect_length, n_transects, grid_side, lit_zone_size, channel_width){
  df_lb <- left_bank_preds(number_of_bass, transect_length, n_transects, lit_zone_size)
  df_rb <- right_bank_preds(number_of_bass, transect_length, n_transects, lit_zone_size, channel_width)
  df <- bind_rows(df_lb, df_rb)
  df %>% mutate(grid = grid_num(distance_downstream, grid_side))

}


df <- driver(number_of_bass, transect_length, n_transects, grid_side, lit_zone_size, channel_width)

ggplot(df, aes(x = distance_downstream)) +
  theme_classic(base_size = 30) +
  labs(y = "Distance from shore (m)", x = "Distance downstream (m)") +
  geom_point(aes(y = distance_from_shore))
  
test <- df %>%
  group_by(grid) %>%
  tally() %>%
  mutate(pred_area = n * 0.5^2 * pi,
         encounter_risk = pred_area / grid_side^2) %>%
  arrange(encounter_risk)
