library("here")
library("tidyverse")
library("openxlsx")
setwd(here())

source("general_logistic_model.R")
source("data_processing.R")

# load the data (path and sheetName are currently hardcoded for testing)

data <- final_data()

survival_variables <- data %>%
  distinct(variable)

for(x_variable in survival_variables){
  print(x_variable)
  #data %>% filter(variable == x_variable)
}

data %>% 
  makeModel()


### check out broom