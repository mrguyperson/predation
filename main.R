library("here")

setwd(here())

source("general_logistic_model.R")
source("data_processing.R")

# load the data (path and sheetName are currently hardcoded for testing)

data <- final_data()

survival_variables <- data %>%
  distinct(variable)

for(variable in survival_variables){
  print(variable)
}

survival_increase_list<- c(length,
                           cover,
                           depth, 
                           light, 
                           density, 
                           temperature)