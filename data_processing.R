library("tidyverse")
library("openxlsx")
#library("here")

#setwd(here())

##### Data on predation preventation from temperature (mortFishAqPredT) #####
# load the data
# This uses physological measures of predators to get the potential predation effect of T
predTData  <- function(){
  read.xlsx(xlsxFile = "./inSALMO Fish Parameters.xlsx",
                      sheet = "mortAqByPredMet",
                      na.strings = "NA") %>% 
  group_by(author, year, journal, species) %>% 
  mutate(unitlessValue = 1-value/max(value)) %>% 
  ungroup()  %>%
  select(X, variable, unitlessValue)
}

##### Data on predation preventation from length (mortFishAqPredL) #####
# load the data and convert all metrics to daily survival
predLData <- function(){
  read.xlsx(xlsxFile = "./inSALMO Fish Parameters.xlsx",
                      sheet = "mortFishByMort",
                      na.strings = "NA") %>% 
  filter(note != "outlier") %>% 
  mutate(dailySurvival = NA,
         dailySurvival = ifelse(units == "survival", measure^(1/time_days), dailySurvival),
         dailySurvival = ifelse(units == "daily survival", measure, dailySurvival),
         unitlessValue = ifelse(units == "relative vlun.", 1-measure, dailySurvival)) %>%
  rename(X = length_cm) %>%
  select(X, variable, unitlessValue)
}


##### Data on predation preventation from depth (mortFishAqPredD) #####
# load the data
# this uses the fractional occurenec of small fish as a proxy for survival
# what is the max possible survival

predationPreData <- function(){ 
  read.xlsx(xlsxFile = "./inSALMO Fish Parameters.xlsx",
            sheet = "mortFishByOccurence",
            na.strings = "NA") 
}

predDepthData  <- function(maxSurvival=0.9){ 
  predationPreData() %>% 
    filter(fishSize_mm =="< 50",
           variable == "Depth") %>% 
    mutate(fraction = cumlitaveFraction - lag(cumlitaveFraction),
           unitlessValue = fraction/max(fraction, na.rm = T)*maxSurvival) %>%
    rename(X = value) %>%
    select(X, variable, unitlessValue)
}

##### Data on predation preventation from cover (mortFishAqPredH) #####
# load the data
# this uses the fractional occurenec of small fish as a proxy for survival
# what is the max possible survival
predationPreData <- function(){
  read.xlsx(xlsxFile = "./inSALMO Fish Parameters.xlsx",
            sheet = "mortFishByOccurence",
            na.strings = "NA")
}

predDistData <- function(maxSurvival=0.9){
  predationPreData() %>% 
    filter(fishSize_mm =="< 50",
           variable == "Dis to Cover") %>% 
    mutate(fraction = cumlitaveFraction - lag(cumlitaveFraction),
           fraction = ifelse(is.na(fraction), cumlitaveFraction, fraction),
           unitlessValue = fraction/max(fraction, na.rm = T)*maxSurvival) %>%
    rename(X = value) %>%
    select(variable, X, unitlessValue)
}

final_data  <- function(){
  bind_rows(predTData(), predLData(), predDepthData(), predDistData())
}

