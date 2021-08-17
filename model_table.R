library("tidyverse")
library("rstudioapi")
library("broom")
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

source("data_processing.R")
source("prediction_tables.R")

# creates a nested tibble of logistic fits to fish parameter data. intended to take
# final_data() as df from data_processing.R

table_of_logistic_models <- function(df){
  df %>%
    # nest the table per variable
    nest(df = c(x, unitless_value), cols=-variable) %>%
    
    # add a new column of fitted glm models for each variable
    mutate(fit = map(df, ~ glm(unitless_value ~ x,
                                 family = quasibinomial(logit),
                                 data = .)))
}

