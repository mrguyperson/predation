library("tidyverse")
library("rstudioapi")
library("broom")
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
source("data_processing.R")
source("prediction_tables.R")
source("model_table.R")

# outputs a table of predicted survival values for each variable affecting predation
# df is intended to work with functions from prediction_tables.R
# model_table is intended to work with model_table.R

survival_prediction_table <- function(df, model_table){
  df %>%
    # nest the table per variable
    nest(x = -variable) %>%
    
    # join the x values with the table that has the glm models
    inner_join(model_table, by = 'variable') %>%
    
    # preditct survival values based on the x values and glms
    mutate(survival = map2(fit, x, predict.glm, type='response')) %>%
    
    # unnest the x values and survival values
    unnest(c(x, survival)) %>%
    
    # save the variable name, x value, and survival prediction
    select(variable, x, survival)
}
