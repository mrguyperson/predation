library("tidyverse")
library("openxlsx")


##### Data on predation preventation from temperature (mortFishAqPredT) #####
# load the data (path and sheetName are currently hardcoded for testing)

path <- "./inSALMO Fish Parameters.xlsx"
sheetName <- "mortAqByPredMet"

makeData  <-  function(path, sheetName){
  read.xlsx(xlsxFile = path,
            sheet = sheetName,
            na.strings = "NA") %>% 
  group_by(author, year, journal, species) %>% 
  mutate(unitlessValue = 1 - value/max(value)) %>% 
  ungroup()
}

# do a logistic fit

makeModel  <-  function(data){
  glm(data$unitlessValue ~ data$X,
      family=quasibinomial(logit),
      data=data)
}

# calculate the X value where Y = 0.1

calculateX1  <- function(model){
  -(log(1/0.1-1) + model[[1]][1])/model[[1]][2]
} 

# calculate the X value where Y = 0.9

calculateX9 <- function(model){
  -(log(1/0.9-1) + model[[1]][1])/model[[1]][2]
}

# calculate the increase of survival provided by a habitat variable (based on inSTREAM 7.1)

# note to self: break this function apart so a table of survival values can be easily made


calculateSurvival <- function(path, sheetName, habitatVariable){
  data <- makeData(path, sheetName)
  model <- makeModel(data)
  X1 <- calculateX1(model)
  X9 <- calculateX9(model)
  B <- (log(0.1/0.9) * 2) / (X1 - X9)
  A <- log(0.1/0.9) - (B * X1)
  S <- exp(A + (B * habitatVariable)) / (1 + exp(A + (B * habitatVariable)))
}

# temp used as the environmental variable for testing.

temp <- 3.69

S <- calculateSurvival(path, sheetName, temp)
S

temperature <- seq(0,30,by=0.1)
df <- data.frame(temperature)

model <- makeModel(makeData(path,sheetName))

df %>% mutate(predict = predict.glm(model, type = "response"))
  
