library("rstudioapi")
library('tidyverse')
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
source('generate_preds.R')
source('main.R')

# set-up values
pred_pos <- get_pred_postitions()
stream_grid_frame <- create_stream_raster_frame(pred_pos)
enc_probs <- calc_enc_probs(stream_grid_frame)
base_pred_success <- 0.8
reach_pred_min <- 1 - base_pred_success
survival_table <- predation_survival_driver_func()


calculate_num_trans_traversed <- function(transect_length=default_transect_length, 
                                          n_transects=default_n_transects, 
                                          grid_size=default_grid_size){
  
  round(default_transect_length * default_n_transects / default_grid_size)

}

get_enc_prob_vector <- function(enc_prob_dataframe = calc_enc_probs(create_stream_raster_frame(get_pred_postitions()))){
  enc_prob_dataframe %>% pull(enc_prob)
}

randomly_sample_encounter_probs <- function(enc_probs, num_trans_traversed){
  sample(enc_probs, num_trans_traversed)
}

calcule_risk_modifier <- function(survival_table, selected_variable, variable_magnitude){
  var <-  selected_variable
  survival_table %>%
    filter(variable == var & x == variable_magnitude) %>%
    pull(survival)
}

set_up_dataframe_of_fish_lengths <- function(number_of_fish, mean = 7.5, sd = 4, precision = 1){
  vals <- round(rnorm(2*number_of_fish, mean, sd), precision)
  vals <- vals[vals>0][1:number_of_fish]
  data.frame(length = vals)
}

calculate_encounter_prob_based_on_length <- function(fish_length_boost, enc_prob_vector){
  encounter_check_values <- fish_length_boost * enc_prob_vector
}

check_if_encounter_occurs <- function(encounter_prob_based_on_length){
  encounter <- sample(c(0, 1), size = 1, prob = c(1 - encounter_prob_based_on_length, encounter_prob_based_on_length))
}

evaluate_encounter <- function(pred_success = base_pred_success, pred_min = reach_pred_min){
  survival <- sample(c(0,1), 1, prob = c(pred_success, pred_min))
  if(survival == 1.0){
    outcome <- 1.0
  } else {
    outcome <- 0
  }
  
}

num_trans_traversed <- calculate_num_trans_traversed()

enc_prob_vector <- get_enc_prob_vector()

enc_prob_sample <- randomly_sample_encounter_probs(enc_prob_vector, num_trans_traversed)

survival_table <- predation_survival_driver_func()

fish_frame <- set_up_dataframe_of_fish_lengths(100, precision=0) %>%
  mutate(survival_boost = lapply(length, FUN = calcule_risk_modifier, survival_table = survival_table, selected_variable = 'length'))



encounter_func <- function(encounter_check, pred_success = base_pred_success, pred_min = reach_pred_min){
  encounter <- sample(c(0, 1), size = 1, prob = c(1 - encounter_check, encounter_check))
  if(encounter == 1){
    print("Attack!!!")
    print(i)
    survival <- sample(c(0,1), 1, prob = c(pred_success, pred_min))
    if(survival == 1.0){
      outcome <- 1.0
    } else {
      outcome <- 0
    }
  } else {
    outcome <- 1.0
  }
}



number_of_fish <- 20
live_or_die_list <- c()
encounters <- calc_enc_probs(stream_grid_frame) %>%
  pull(enc_prob)
tic()
for(i in seq(number_of_fish)){
  fish_length <- round(rnorm(1, mean = 7.5, sd = 4), 1)
  
  length_prob <- survival_table %>%
    filter(variable == 'length' & x == fish_length) %>%
    pull(survival)
  new_encs <- sample(encounters, num_trans_traversed)
  encounter_check = new_encs * length_prob
  outcome = as.numeric(lapply(encounter_check, encounter_func))
  outcome_sum <- sum(outcome)
  if(outcome_sum < num_trans_traversed){
    survival <- 0
  } else {
    survival <- 1
  }
  live_or_die_list[i] <- survival
}

toc()
sum(live_or_die_list)/number_of_fish

thing = "length"

survival_table %>%
  filter(variable == thing)

calcule_risk_modifier(selected_variable = 'length', variable_magnitude = 10)
