library("tidyverse")
library("rstudioapi")
library("broom")

# creates a nested tibble of logistic fits to fish parameter data. intended to take
# full_raw_data() as df from data_processing.R

table_of_logistic_models <- function(df){
  df %>%
    # nest the table per variable
    nest(data = c(x, unitless_value)) %>%
    
    # add a new column of fitted glm models for each variable
    mutate(fit = map(data, ~ glm(unitless_value ~ x,
                                 family = quasibinomial(logit),
                                 data = .)))
}

