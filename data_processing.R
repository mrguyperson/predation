library("tidyverse")
library("openxlsx")
library("rstudioapi")

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

##### Data on predation preventation from temperature (mortFishAqPredT) #####
# load the data
# This uses physological measures of predators to get the potential predation effect of T
predTData  <- function(){
  read.xlsx(xlsxFile = "./inSALMO Fish Parameters.xlsx",
                      sheet = "mortAqByPredMet",
                      na.strings = "NA") %>% 
  group_by(author, year, journal, species) %>% 
  mutate(unitless_value = 1-value/max(value)) %>% 
  ungroup()  %>%
  dplyr::select(x, variable, unitless_value)
}

##### Data on predation preventation from length (mortFishAqPredL) #####
# load the data and convert all metrics to daily survival
# predLData <- function(){
#   read.xlsx(xlsxFile = "./inSALMO Fish Parameters.xlsx",
#                       sheet = "mortFishByMort",
#                       na.strings = "NA") %>% 
#   filter(note != "outlier") %>% 
#   mutate(dailySurvival = NA,
#          dailySurvival = ifelse(units == "survival", measure^(1/time_days), dailySurvival),
#          dailySurvival = ifelse(units == "daily survival", measure, dailySurvival),
#          unitless_value = ifelse(units == "relative vlun.", 1-measure, dailySurvival)) %>%
#   rename(x = length_cm) %>%
#   select(x, variable, unitless_value)
# }
prey_conv <- function(a, B, pred_L){
  exp(a + B * log(pred_L))
}

angle_calc <- function(length){
  0.0167 * exp(9.14 - 2.4 * log(length) + 0.229 * log(length)^2)
}

predLData <- function(){
  read.csv('./lmb_stb_combined.csv') %>%
    mutate(max_prey_length = prey_conv(0.443, 0.774, length_mm),
           safety = cumulative_proportion - proportion_of_total,
           angle = angle_calc(length_mm),
           reaction_dist = max_prey_length / (2 * tan(angle / 2)),
           max_prey_length = max_prey_length / 10,
           variable = "length") %>%
    dplyr::rename(x = max_prey_length, unitless_value = safety) %>%
    dplyr::select(variable, x, unitless_value)
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
           unitless_value = fraction/max(fraction, na.rm = T)*maxSurvival) %>%
    dplyr::rename(x = value) %>%
    dplyr::select(x, variable, unitless_value)
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
           unitless_value = fraction/max(fraction, na.rm = T)*maxSurvival) %>%
    dplyr::rename(x = value) %>%
    dplyr::select(variable, x, unitless_value)
}

full_raw_data  <- function(){
  df <- bind_rows(predTData(), predLData(), predDepthData(), predDistData())
  df %>% mutate(variable = tolower(variable))
}

