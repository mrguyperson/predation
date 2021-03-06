library("tidyverse")
library("openxlsx")
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

##### Data on predation preventation from temperature (mortFishAqPredT) #####

makeData  <-  function(path, sheetName){
  read.xlsx(xlsxFile = path,
            sheet = sheetName,
            na.strings = "NA") %>% 
  group_by(author, year, journal) %>% 
  mutate(unitlessValue = 1 - value/max(value)) %>% 
  ungroup()
}

# do a logistic fit

makeModel  <-  function(data){
  #y <- data %>% select(unitlessValue)
  #x <- data %>% select(X)
  data %>% glm(unitlessValue ~ X,
      family=quasibinomial(logit),
      data=.)
}

# calculate the X value where Y = 0.1

calculateX1  <- function(model){
  -(log(1/0.1-1) + model[[1]][1])/model[[1]][2]
} 

# calculate the X value where Y = 0.9

calculateX9 <- function(model){
  -(log(1/0.9-1) + model[[1]][1])/model[[1]][2]
}

# calculate A value for logistic function

calculateA <- function(X1, B){
  A <- log(0.1/0.9) - (B * X1) 
}

# calculate B value for logistic function

calculateB <- function(X1, X9){
  B <- (log(0.1/0.9) * 2) / (X1 - X9)
}
# calculate the increase of survival provided by a habitat variable (based on inSTREAM 7.1)

calculateSurvival <- function(A, B, habitatVariable){
  S <- exp(A + (B * habitatVariable)) / (1 + exp(A + (B * habitatVariable)))
}

# calculate a table of survival values for a range of habitat values

survivalTable <- function(path, sheetName, minVal, maxVal, interval){
  model <- makeModel(makeData(path,sheetName))
  df <- data.frame(X = seq(minVal, maxVal, by=interval))
  df %>% mutate(predict = predict.glm(model, df, type = 'response'))
}

path = "./inSALMO Fish Parameters.xlsx"
sheetName = "mortAqByPredMet"

data <- makeData(path, sheetName)
model <- makeModel(data)


survivalTable(path, sheetName, 0, 30, 0.1)
