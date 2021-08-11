library("tidyverse")
library("openxlsx")

setwd(getwd())

##### Data on predation preventation from temperature (mortFishAqPredT) #####
# load the data
# This uses physological measures of predators to get the potential predation effect of T
predTData = read.xlsx(xlsxFile = "./inSALMO Fish Parameters.xlsx",
                      sheet = "mortAqByPredMet",
                      na.strings = "NA") %>% 
  group_by(author, year, journal, species) %>% 
  mutate(unitlessValue = 1-value/max(value)) %>% 
  ungroup() 

##### Data on predation preventation from length (mortFishAqPredL) #####
# load the data and convert all metrics to daily survival
predLData = read.xlsx(xlsxFile = "./inSALMO Fish Parameters.xlsx",
                      sheet = "mortFishByMort",
                      na.strings = "NA") %>% 
  filter(note != "outlier") %>% 
  mutate(dailySurvival = NA,
         dailySurvival = ifelse(units == "survival", measure^(1/time_days), dailySurvival),
         dailySurvival = ifelse(units == "daily survival", measure, dailySurvival),
         dailySurvival = ifelse(units == "relative vlun.", 1-measure, dailySurvival))

##### Data on predation preventation from depth (mortFishAqPredD) #####
# load the data
# this uses the fractional occurenec of small fish as a proxy for survival
# what is the max possible survival
maxSurvival = 0.9
predationPreData = read.xlsx(xlsxFile = "./inSALMO Fish Parameters.xlsx",
                             sheet = "mortFishByOccurence",
                             na.strings = "NA") 

predDepthData = predationPreData %>% 
  filter(fishSize_mm =="< 50",
         variable == "Depth") %>% 
  mutate(fraction = cumlitaveFraction - lag(cumlitaveFraction),
         fractionalOccurrence = fraction/max(fraction, na.rm = T)*maxSurvival)

##### Data on predation preventation from cover (mortFishAqPredH) #####
# load the data
# this uses the fractional occurenec of small fish as a proxy for survival
# what is the max possible survival
maxSurvival = 0.9
predationPreData = read.xlsx(xlsxFile = "./inSALMO Fish Parameters.xlsx",
                             sheet = "mortFishByOccurence",
                             na.strings = "NA") 

predDistData = predationPreData %>% 
  filter(fishSize_mm =="< 50",
         variable == "Dis to Cover") %>% 
  mutate(fraction = cumlitaveFraction - lag(cumlitaveFraction),
         fraction = ifelse(is.na(fraction), cumlitaveFraction, fraction),
         fractionalOccurrence = fraction/max(fraction, na.rm = T)*maxSurvival)