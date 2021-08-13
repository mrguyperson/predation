#library("here")
library("tidyverse")
library("openxlsx")
library("rstudioapi")
library("broom")
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

#source("general_logistic_model.R")
source("data_processing.R")

# load the data (path and sheetName are currently hardcoded for testing)


model_table <- final_data() %>% 
  group_by(variable) %>%
  do(fitVariable = glm(unitless_value ~ x,
                            family = quasibinomial(logit),
                            data = .)) #%>%
  #unnest(fitVariable)
model <- function(model_table, factor){
  model_table %>%
    filter(variable == factor) %>%
    select(fitVariable) %>%
    .[[1]] %>%
    .[[1]]
}

  #unnest(cols = c(fitVariable))

final_data() %>% distinct(variable)


df <- data.frame(variable = 'Temp', x = seq(0, 30, by=0.1))
df %>% mutate(predict = predict.glm(model(model_table, 'Temp'), df, type = 'response'))