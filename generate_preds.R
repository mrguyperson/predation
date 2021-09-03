library('tidyverse')
library('plyr')

lit_zone_size <- 5
channel_width <- 100
transect_length  <- 1000
n_transects  <- 200
grid_side <- 5

number_of_stripers <- function(){
  stripers <- 0
  while(stripers < 21){
    stripers <- rnorm(1, mean = 332, sd = 477)
    #print(stripers)
    if (stripers >= 21){
      return(stripers)
    }
  }
}

number_of_lmb <- function(){
  lmb <- 0
  while(lmb < 1){
    lmb <- rnorm(1, mean = 333, sd = 195)
    #print(stripers)
    if (lmb >= 2){
      return(lmb)
    }
  }
}



left_bank_preds <- function(number_of_bass, transect_length, n_transects, lit_zone_size){
  data.frame(distance_from_shore = runif(number_of_bass, 
                                         min = 0, 
                                         max = lit_zone_size), 
             distance_downstream = runif(number_of_bass, 
                                         min = transect_length * current_transect - transect_length, 
                                         max = transect_length * current_transect)) %>%
    arrange(distance_downstream)
}

right_bank_preds <- function(number_of_bass, transect_length, n_transects, lit_zone_size, channel_width){
  data.frame(distance_from_shore = runif(number_of_bass, 
                                         min = channel_width - lit_zone_size, 
                                         max = channel_width), 
             distance_downstream = runif(number_of_bass, 
                                         min = transect_length * current_transect - transect_length, 
                                         max = transect_length * current_transect)) %>%
    arrange(distance_downstream)
}

channel_preds <- function(number_of_bass, transect_length, lit_zone_size, channel_width, current_transect){
  data.frame(distance_from_shore = runif(number_of_bass, 
                                         min = lit_zone_size, 
                                         max = channel_width - lit_zone_size),
             distance_downstream = runif(number_of_bass, 
                                         min = transect_length * current_transect - transect_length, 
                                         max = transect_length * current_transect)) %>%
    arrange(distance_downstream)
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
  channel_preds_list = list()
  df_ch <- for(i in seq(n_transects)){
    channel_preds <- channel_preds(transect_length, lit_zone_size, channel_width, i)
    channel_preds_list[[i]] <- channel_preds
  }
  df_ch <- bind_rows(channel_preds_list)
  df <- bind_rows(df_lb, df_rb, df_ch)
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
