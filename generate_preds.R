library('tidyverse')
library('plyr')
library('raster')
library('wesanderson')
library('sn')

lit_zone_size <- 5
channel_width <- 100
transect_length  <- 1000
n_transects  <- 200
grid_size <- 15

# number_of_stripers <- function(mean = 334, sd = 447, skewness = 0.9){
#   stripers <- 0
#   params <- cp2dp(c(mean, sd, skewness), "SN")
#   while(stripers < 21){
#     stripers <- rsn(1, dp = params)
#     if (stripers >= 21){
#       return(round(stripers))
#     }
#   }
# }

number_of_stripers <- function(mean_low = 60, sd_low = 43, mean_high = 870, sd_high = 491.5){
  stripers <- 0
  aggregation <- sample(c(TRUE,FALSE),1)
  
  if(aggregation){
    while(stripers < 1){
      stripers <- rnorm(1, mean = mean_high, sd = sd_high)
      if (stripers >= 1){
        return(round(stripers))
        }
      }
    }
  else{
    while(stripers < 1){
      stripers <- rnorm(1, mean = mean_low, sd = sd_low)
      if (stripers >= 1){
        return(round(stripers))
      }
    }
  }  
}

number_of_lmb <- function(mean = 333, sd = 195){
  lmb <- 0
  while(lmb <= mean - sd | lmb >= mean + sd){
    lmb <- rnorm(1, mean = mean, sd = mean)
    if (lmb >= mean - sd & lmb <= mean + sd){
      return(round(lmb))
    }
  }
}



bank_preds <- function(number_of_bass, transect_length, lit_zone_size, zone_start, zone_end, current_transect){
  data.frame(distance_downstream = runif(number_of_bass, 
                                         min = transect_length * current_transect - transect_length, 
                                         max = transect_length * current_transect),
             distance_from_shore = runif(number_of_bass, 
                                         min = zone_start, 
                                         max = zone_end)) %>%
    arrange(distance_downstream)
}


channel_preds <- function(number_of_bass, transect_length, lit_zone_size, channel_width, current_transect){
  data.frame(distance_downstream = runif(number_of_bass, 
                                         min = transect_length * current_transect - transect_length, 
                                         max = transect_length * current_transect),
             distance_from_shore = runif(number_of_bass, 
                                         min = lit_zone_size, 
                                         max = channel_width - lit_zone_size)) %>%
    arrange(distance_downstream)
}


grid_num <- function(x, grid_size){
  grid_val <- round_any(x, grid_size, ceiling)
  return(grid_val/grid_size)
  
}

add_grid_nums <- function(df, x, grid_size){
  df %>% mutate(grid = grid_num(distance_downstream))
}


get_pred_postitions <- function(transect_length, n_transects, lit_zone_size, channel_width){
  left_bank_preds_list <- list()
  right_bank_preds_list <- list()
  channel_preds_list <- list()
  
  for(i in seq(n_transects)){
    lmb <-  number_of_lmb()
    stripers <- number_of_stripers()
    
    left_bank_preds <- bank_preds(lmb / 2 + stripers / 4, 
                                  transect_length, 
                                  lit_zone_size, 
                                  0, 
                                  lit_zone_size, 
                                  i)
    right_bank_preds <- bank_preds(lmb / 2 + stripers / 4, 
                                   transect_length, 
                                   lit_zone_size, 
                                   channel_width - lit_zone_size, 
                                   channel_width, 
                                   i)
    channel_preds <- channel_preds(stripers / 2,
                                   transect_length, 
                                   lit_zone_size, 
                                   channel_width, 
                                   i)
    left_bank_preds_list[[i]] <- left_bank_preds
    right_bank_preds_list[[i]] <- right_bank_preds
    channel_preds_list[[i]] <- channel_preds
  }
  df <- bind_rows(left_bank_preds_list, right_bank_preds_list, channel_preds_list) %>%
    arrange(distance_downstream)

}

create_stream_raster_frame <- function(df, transect_length, channel_width, grid_size, n_transects){
  r <- raster(xmn = 0, 
              ymn = 0, 
              xmx = transect_length * n_transects, 
              ymx = channel_width, 
              res = grid_size)
  r[] <- 0
  tab <- table(cellFromXY(r, df))
  r[as.numeric(names(tab))] <- tab
  d <- data.frame(coordinates(r), count=r[])
}

calc_enc_probs <- function(df, grid_size, reaction_dis=0.5){
  data.frame(coordinates(df), count=df[]) %>%
    mutate(pred_area = count * reaction_dis^2 * pi,
           enc_prob = pred_area / grid_size^2)
}


graph_pred_positions <- function(df){
  ggplot(df, aes(x = distance_downstream)) +
    theme_classic(base_size = 30) +
    labs(y = "Distance from left bank (m)", x = "Distance downstream (m)") +
    geom_point(aes(y = distance_from_shore))
}


graph_enc_probs <- function(df){
  pa  <- wes_palettes %>% 
    names()
  pal <-  wes_palette(name = pa[7], n = 10, type = "continuous")
  ggplot(df, aes(x, y, fill = enc_prob)) + 
    labs(y = "Distance from left bank (m)", x = "Distance downstream (m)") +
    geom_tile() +
    scale_fill_gradientn(colors=pal)
  
}


tic()
pred_pos <- get_pred_postitions(transect_length, n_transects, lit_zone_size, channel_width)
stream_grid_frame <- create_stream_raster_frame(pred_pos, transect_length, channel_width, grid_size, n_transects)
enc_probs <- calc_enc_probs(stream_grid_frame, grid_size)
toc()

graph_pred_positions(pred_pos)
graph_enc_probs(enc_probs)
