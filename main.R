setwd(getwd())
source("general_logistic_model.R")

# load the data (path and sheetName are currently hardcoded for testing)

path <- "./inSALMO Fish Parameters.xlsx"
sheetName <- "mortAqByPredMet"

data <- makeData(path, sheetName)

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