library("tidyverse")

length <- 0.1
cover <- 0.1
depth <- 0.1
light <- 0.1
density <- 0.1
temperature <- 0.1

survival_increase_list<- c(length, 
                           cover,
                           depth, 
                           light, 
                           density, 
                           temperature)

reach_pred_min <- 0.9

s_t <- reach_pred_min + ((1 - reach_pred_min) * (1 - prod(1 - survival_increase_list)))

s_t