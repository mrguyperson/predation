library("tidyverse")

value_df <- function(){
  data.frame(variable = c('temp', 'length', 'dis to cover', 'depth'), 
             min_x = c(0, 0, 0, 0), 
             max_x = c(30, 50, 3, 2), 
             interval = c(0.1, 0.1, 0.01, 0.01)
             )
}

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

new_prediction_df <- function(value_df){
  data.frame(variable = value_df[1], X = seq(value_df[2], value_df[3], by = value_df[4]))
}
df <- value_df()
prediction_list = list()
for (i in 1:nrow(df)){
  new_df <- data.frame(variable = df[i,1], X = seq(df[i,2], df[i,3], by = df[i,4]))
  prediction_list[[i]] <- new_df
}
bind_rows(prediction_list)