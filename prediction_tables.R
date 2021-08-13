library("tidyverse")

prediction_df <- function(factor, min_x, max_x, interval){
  data.frame(variable = factor, X = seq(min_x, max_x, by = interval))
}

temp_df <- function(){
  prediction_df('temp', 0, 30, 0.1)
}

length_df <- function(){
  prediction_df('length', 0, 50, 0.1)
}

cover_df <- function(){
  prediction_df('dis to cover', 0, 3, 0.01)
}

depth_df <- function(){
  prediction_df('depth', 0, 2, 0.01)
}

full_prediction_df <- function(){
  bind_rows(temp_df(), length_df(), cover_df(), depth_df())
}