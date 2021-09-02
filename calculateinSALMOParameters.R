##### Description #####
# This is the file to process parameters for inSLAMO
# Each paremter gets it own section 

##### Libraries #####
library(tidyverse)
library(lubridate)
# Open and write excel files
library(openxlsx)
library(viridis)
library(broom)

##### Functions #####
# Make a function 
# functionName = function(input1){
#   return(output)
# }

##### Habitat drift concentration (habDriftConc) #####
# first make the conversion for number to dry biomass
biomassConv = read.xlsx(xlsxFile = "./inSALMO Hab Parameters.xlsx",
                      sheet = "countPerBiomass",
                      na.strings = "NA") %>% 
  select(number_per_m3, dryBiomass_g_per_m3) %>%
  mutate(massConv = dryBiomass_g_per_m3/number_per_m3) %>% 
  summarize(massConv = mean(massConv)) %>% 
  .$massConv

# 0.4433 is to convert wet to dry biomass from 
# sabo 2002 Journal of the North American Benthological Society
wetToDry = 0.4433

# Now calculate drift
habDriftConc = read.xlsx(xlsxFile = "./inSALMO Hab Parameters.xlsx",
                           sheet = "habDriftConc",
                           na.strings = "NA") %>% 
  select(number_per_m3) %>%
  # Calculate density and convert from m^3 to cm^3
  mutate(wetBiomass = number_per_m3*biomassConv/wetToDry/1E6) %>% 
  summarise(wetBiomass = mean(wetBiomass, na.rm = TRUE)) %>% 
  .$wetBiomass
  
##### Habitat prey energy density (habPreyEnergyDensity) #####
# load the data and do calculations to get the energy density of food
habPreyEnergyDensity = read.xlsx(xlsxFile = "./inSALMO Hab Parameters.xlsx",
                        sheet = "habPreyEnergyDensity",
                        na.strings = "NA") %>% 
  select(year, author, scaledWeightOfSample, digestability, energyDensity_j_per_gWetMass) %>%
  # calculate the scaled weight of each digeatability measure
  mutate(digestableMatterPer = scaledWeightOfSample * digestability,
         wetMassPer = scaledWeightOfSample * energyDensity_j_per_gWetMass) %>% 
  group_by(year, author) %>% 
  # get the totals for each study
  summarize(digestableMatter = sum(digestableMatterPer),
            wetMass = sum(wetMassPer),
            weight = sum(scaledWeightOfSample)) %>% 
  ungroup() %>% 
  # divide by the weights to get the averages from each study
  mutate(digestableMatterSacled = digestableMatter/weight,
         wetMassScaled = wetMass/weight) %>% 
  # get averages of all studies
  summarise(digestableMatterTot = mean(digestableMatterSacled),
            wetMassTot = mean(wetMassScaled)) %>% 
  # Convert to usuable energy
  mutate(energyDensity = digestableMatterTot * wetMassTot) %>% 
  .$energyDensity

##### Habitat search concentration (habSearchProd) #####
# first make the conversion for number to dry biomass
biomassConv = read.xlsx(xlsxFile = "./inSALMO Hab Parameters.xlsx",
                        sheet = "countPerBiomass",
                        na.strings = "NA") %>% 
  select(number_per_m3, dryBiomass_g_per_m3) %>%
  # Calculate daily survival
  mutate(massConv = dryBiomass_g_per_m3/number_per_m3) %>% 
  summarize(massConv = mean(massConv)) %>% 
  .$massConv

# 0.4433 is to convert wet to dry biomass from 
# sabo 2002 Journal of the North American Benthological Society
wetToDry = 0.4433

# what fraction of it search biomass do it get back every hour
# 0.01/24 is 1 percent per day
fractionPerHour = 0.01/24

# Now calculate drift
habSearchProd = read.xlsx(xlsxFile = "./inSALMO Hab Parameters.xlsx",
                         sheet = "habSearchProd",
                         na.strings = "NA") %>% 
  select(number_per_cm2) %>%
  # calculate the production rate in g/cm^2/h using the above assumed rate constant
  mutate(wetBiomass = number_per_cm2*biomassConv/wetToDry,
         productionRate = wetBiomass*fractionPerHour) %>% 
  .$productionRate

##### Habitat sheer stress (habShear) #####
# elevation (feet) and river mile of 2 locations on the river
# 5280 feet per mile
riverMiles = c(294, 302)
riverElevation = c(444, 470)/5280
# calculate slope in m
slope = (riverElevation[1]-riverElevation[2])/(riverMiles[1]-riverMiles[2])
# density of sedminet in g/m^3 fomr inSALMO manaual
sedDensity = 2.7*1e6
# density of water g/m^3
h2oDensity = 1e6
# gravel size [m] from Sacramento River Ecological Flow Study: Gravel Study Final Report
gravelSize = 0.06

shearData = read.xlsx(xlsxFile = "./inSALMO Hab Parameters.xlsx",
                          sheet = "habShear",
                          na.strings = "NA") %>%
  mutate(flow_cms = flow_cfs*(30.48^3)/1e6,
         hydroRadius_m = hydroRadius_ft*30.48/100,
         sheerStress = slope*h2oDensity*hydroRadius_m/((sedDensity-h2oDensity)*gravelSize),
         lnFlow = log(flow_cms),
         lnSS = log(sheerStress))

# do a lm
shearModel = lm(shearData$lnSS ~ shearData$lnFlow,
                     data=shearData)

# Calculate the inslamo parameters
habShearParamA = exp(shearModel$coefficients[1])
habShearParamB = shearModel$coefficients[2]

# Made data for a graph
shearDataWFit = shearData %>% 
  mutate(predict = predict(shearModel, type = "response")) %>% 
  arrange(lnSS)

# Make a plot

ggplot(shearDataWFit, aes(x = lnFlow)) +
  theme_classic(base_size = 30) +
  labs(y = "ln(SS)", x = "ln(Flow)") +
  geom_point(aes(y = lnSS)) +
  geom_path(aes(y = predict), color = "red")


##### Spawner velocity sutibality (fishSpawnVSuit) #####
velocityCurveData = read.xlsx(xlsxFile = "./inSALMO Fish Parameters.xlsx",
                              sheet = "fishSpawnVSuit",
                              na.strings = "NA")  

# enter the data to match the curve by eye  
# scale value reduces the magnitude of the higher values to make the spawners
# distribute in a simmilar fashion to the redd locations
fishSpawnVSuitV = c(0.00, 0.60, 1.30, 1.90, 3.50, 4.00)
fishSpawnVSuitS = c(0.48, 0.85, 1.00, 0.85, 0.05, 0.00)
sacleValue = 0.15

matchVData = data.frame(velocity = fishSpawnVSuitV,
                        value = fishSpawnVSuitS) %>% 
  mutate(zeroCheck = (value == 0)) %>%
  group_by(zeroCheck) %>% 
  mutate(scaledValue = ifelse(value == min(value), value, (value-min(value))*sacleValue + min(value))) %>% 
  ungroup()

# Plot the data

ggplot(velocityCurveData, aes(x = velocity_m_per_s)) +
  theme_classic(base_size = 30) +
  labs(y = "RSF", x = "Velocity (m/s)") +
  geom_path(aes(y = value), color = "black", alpha = 0.2, size = 3) +
  geom_point(data = matchVData, aes(x = velocity, y = value), shape = 1, size = 3, stroke = 2, color = "red") +
  geom_path(data = matchVData, aes(x = velocity, y = value), color = "red", size = 1 ) +
  geom_point(data = matchVData, aes(x = velocity, y = scaledValue), shape = 1, size = 3, stroke = 2, color = "blue") +
  geom_path(data = matchVData, aes(x = velocity, y = scaledValue), color = "blue", size = 1 )

##### Spawner depth sutibality (fishSpawnDSuit) #####
depthCurveData = read.xlsx(xlsxFile = "./inSALMO Fish Parameters.xlsx",
                           sheet = "fishSpawnDSuit",
                           na.strings = "NA")  

# enter the data to match the curve by eye  
fishSpawnDSuitD = c(0.00, 3.20, 5.50, 9.40, 9.50)
fishSpawnDSuitS = c(0.25, 0.95, 0.95, 0.14, 0.00)
sacleValue = 0.15

matchVData = data.frame(depth = fishSpawnDSuitD,
                        value = fishSpawnDSuitS) %>% 
  mutate(zeroCheck = (value == 0)) %>%
  group_by(zeroCheck) %>% 
  mutate(scaledValue = ifelse(value == min(value), value, (value-min(value))*sacleValue + min(value))) %>% 
  ungroup()

# Plot the data

ggplot(depthCurveData, aes(x = depth_m)) +
  theme_classic(base_size = 30) +
  labs(y = "RSF", x = "Depth (m)") +
  geom_path(aes(y = value), color = "black", alpha = 0.2, size = 3) +
  geom_point(data = matchVData, aes(x = depth, y = value), shape = 1, size = 3, stroke = 2, color = "red") +
  geom_path(data = matchVData, aes(x = depth, y = value), color = "red", size = 1 ) +
  geom_point(data = matchVData, aes(x = depth, y = scaledValue), shape = 1, size = 3, stroke = 2, color = "blue") +
  geom_path(data = matchVData, aes(x = depth, y = scaledValue), color = "blue", size = 1 )

##### The fecundity parameters (fishFecundParam) #####
# Load in the fecundity data
fecundityData = read.xlsx(xlsxFile = "./inSALMO Fish Parameters.xlsx",
                          sheet = "fishFecundParam",
                          na.strings = "NA") %>% 
  mutate(ln_length = log(length_cm),
         ln_eggs = log(eggs))

# do a lm
fecundityModel = lm(fecundityData$ln_eggs ~ fecundityData$ln_length,
                    data=fecundityData)

# Calculate the inslamo parameters
fishFecundParamA = exp(fecundityModel$coefficients[1])
fishFecundParamB = fecundityModel$coefficients[2]

# Made data for a graph
fecundityDataWFit = fecundityData %>% 
  mutate(predict = predict(fecundityModel, type = "response")) %>% 
  arrange(ln_length)

# Make a plot

ggplot(fecundityDataWFit, aes(x = ln_length)) +
  theme_classic(base_size = 30) +
  labs(y = "ln(eggs)", x = "ln(length)") +
  geom_point(aes(y = ln_eggs)) +
  geom_path(aes(y = predict), color = "red")


##### Egg mortality from high temperature (mortReddHiTT)  #####
# This function is to  filter all but the outter most points of the data
findUpperBound = function(dataFrame, xName, yName, dataPointX, dataPointY){
  newData = dataFrame %>% 
    mutate(strike = ifelse(({{xName}} < dataPointX & {{yName}} < dataPointY),0,1))
}

hiTempEggData = read.xlsx(xlsxFile = "./inSALMO Redd Parameters.xlsx",
                           sheet = "mortReddHiTT",
                           na.strings = "NA") %>% 
  select(Temp_C, singleDaySurvival) %>% 
  mutate(scaledSur = singleDaySurvival/max(singleDaySurvival))

# filter out the non outter bounds data points
hiTempFilteredData = map2_df(hiTempEggData$Temp_C, hiTempEggData$scaledSur, ~findUpperBound(hiTempEggData, Temp_C, scaledSur, .x, .y)) %>% 
  group_by(Temp_C, scaledSur) %>% 
  summarise(flag = mean(strike)) %>% 
  ungroup() %>% 
  filter(flag == 1)  

# do a logistic fit
hiTempEggModel = glm(hiTempFilteredData$scaledSur ~ hiTempFilteredData$Temp_C,
                      family=binomial(logit),
                      data=hiTempFilteredData)

# add in predictions for plotting
hiTempFilteredData = hiTempFilteredData %>% 
  mutate(predict = predict.glm(hiTempEggModel, type = "response")) %>% 
  arrange(Temp_C)

# solve for inSALMO Parameters (prevent form being negtive)
mortReddHiTT1 = -(log(1/0.1-1)+hiTempEggModel[[1]][1])/hiTempEggModel[[1]][2]
mortReddHiTT9 = -(log(1/0.9-1)+hiTempEggModel[[1]][1])/hiTempEggModel[[1]][2]

# Make a plot

ggplot(hiTempFilteredData, aes(x = Temp_C)) +
  theme_classic(base_size = 30) +
  labs(y = "Survival", x = "Temperature (C)") +
  geom_point(aes(y = scaledSur)) +
  geom_point(data = hiTempEggData, aes(x = Temp_C, y = scaledSur), shape = 1, size = 3) +
  geom_path(aes(y = predict), color = "red")


##### Egg mortality from low temperature (mortReddLoTT)  #####
# load and filter data
lowTempEggData = read.xlsx(xlsxFile = "./inSALMO Redd Parameters.xlsx",
                           sheet = "mortReddLoTT",
                           na.strings = "NA") %>% 
  select(Temp_C, singleDaySurvival) %>% 
  # Filter out the upper high survival points
  filter(Temp_C<2.6)

# do a logistic fit
lowTempEggModel = glm(lowTempEggData$singleDaySurvival ~ lowTempEggData$Temp_C,
                      family=binomial(logit),
                      data=lowTempEggData)

# add in predictions for plotting
lowTempEggData = lowTempEggData %>% 
 mutate(predict = predict.glm(lowTempEggModel, type = "response")) %>% 
  arrange(Temp_C)

# solve for inSALMO Parameters (prevent form being negtive)
mortReddLoTT1 = max(-(log(1/0.1-1)+lowTempEggModel[[1]][1])/lowTempEggModel[[1]][2],0)
mortReddLoTT9 = -(log(1/0.9-1)+lowTempEggModel[[1]][1])/lowTempEggModel[[1]][2]

# Make a plot

ggplot(lowTempEggData, aes(x = Temp_C)) +
  theme_classic(base_size = 30) +
  labs(y = "Survival", x = "Temperature (C)") +
  geom_point(aes(y = singleDaySurvival)) +
  geom_path(aes(y = predict), color = "red")

  
##### Egg mortality from dewatering (mortReddDewaterSurv) #####
dewaterEggData = read.xlsx(xlsxFile = "./inSALMO Redd Parameters.xlsx",
                           sheet = "mortReddDewaterSurv",
                           na.strings = "NA") %>% 
  select(timeDewatered_h, Survival) %>%
  # Calculate daily survival
  mutate(dailySurvival = (Survival)^(24/timeDewatered_h)) 

# Calculate the parameters
mortReddDewaterSurv = mean(dewaterEggData$dailySurvival)

##### Egg mortality from scour (mortReddScourDepth) #####
# Do the work for the first part
scourData = read.xlsx(xlsxFile = "./inSALMO Redd Parameters.xlsx",
                           sheet = "mortReddScourDepth",
                           na.strings = "NA") %>% 
  select(N, depth_cm) %>%
  # Calculate a weighted average
  summarise(parameter =  weighted.mean(depth_cm, N))

# Calculate the parameters
mortReddScourDepth = scourData$parameter

##### Redd size (reddSize) #####
# Do the work for the first part
reddSizeData = read.xlsx(xlsxFile = "./inSALMO Redd Parameters.xlsx",
                      sheet = "reddSize",
                      na.strings = "NA") %>% 
  select(N, area_cm2) %>%
  # Calculate a weighted average
  summarise(parameter =  weighted.mean(area_cm2, N))

# Calculate the parameters
reddSize = reddSizeData$parameter

##### Length of newly emerged fry (reddNewLength) #####
reddNewLengthData = read.xlsx(xlsxFile = "./inSALMO Redd Parameters.xlsx",
                         sheet = "reddNewLength",
                         na.strings = "NA") %>% 
  # Total Length to Fork Length Relationships of Juvenile Hatchery-Reared Coho and
  # Chinook Salmon Laurel J. Ramseyer The Progressive Fish-Culturist Vol. 57, Iss. 3, 1995
  mutate(FL_cm = (totalLength_cm + 0.03535)/1.14)

# Calculate the parameters
reddNewLengthMin = min(reddNewLengthData$FL_cm)
reddNewLengthMax = max(reddNewLengthData$FL_cm)
# Reduce max for wild Chinook 
# inSALMO max size was too large
reddNewLengthMax = 3.4
 
##### Fish length weight parameters (fishWeightParam) #####
# Read in any raw data
fishRawWtData = read.xlsx(xlsxFile = "./inSALMO Fish Parameters.xlsx",
                          sheet = "fishWeightRaw",
                          na.strings = "NA") %>% 
  mutate(ln_length = log(length_cm),
         ln_weight = log(weight_g)) %>%
  group_by(author) %>% 
  do(weightFit = tidy(lm(ln_weight ~ ln_length, data = .))) %>% 
  unnest(weightFit) %>%
  select(author, term, estimate) %>% 
  pivot_wider(names_from = term, values_from = estimate) %>% 
  rename(intercept = "(Intercept)",
         slope = ln_length) %>% 
  mutate(intercept = exp(intercept)) %>% 
  select(-author)

# Read in already calculated parameters
fishParaWtData = read.xlsx(xlsxFile = "./inSALMO Fish Parameters.xlsx",
                          sheet = "fishWeightParam",
                          na.strings = "NA") %>% 
  select(intercept, slope) %>% 
  bind_rows(fishRawWtData) %>% 
  summarise_all(mean)

fishWeightParamA = fishParaWtData$intercept
fishWeightParamB = fishParaWtData$slope


##### Fish spawn weight loss (fishSpawnWtLossFraction) #####
# Load some parameters for calcualtions
# These will need to be updated is they change 
# in other sections of this script
fishWeightParamATemp =	0.0176
fishWeightParamBTemp =	2.98
fishFecundParamATemp =	6.66
fishFecundParamBTemp =	1.542

# Enter some fish masses in g to do some calculations)
fishMasses = seq(17000, 19000, 500)

# get the data for the mass of the egg
singleEggMass = read.xlsx(xlsxFile = "./inSALMO Fish Parameters.xlsx",
                          sheet = "fishSpawnWtLossFraction",
                          na.strings = "NA") %>% 
  summarize(meanMass = mean(egg_mass_g)) %>%
  .$meanMass

# Calculate the mean fraction weight loss
fishLenghts = (fishMasses/fishWeightParamATemp)^(1/fishWeightParamBTemp)
fishEggs = fishFecundParamATemp*fishLenghts^fishFecundParamBTemp
eggMass = fishEggs*singleEggMass
fishSpawnWtLossFraction = mean(eggMass/fishMasses)

##### Fish respiration parameters (fishRespParam) #####

metData = read.xlsx(xlsxFile = "./inSALMO Fish Parameters.xlsx",
                          sheet = "fishRespParam",
                          na.strings = "NA") %>% 
  mutate(mass = exp(ln_mass_g),
         met_j_per_day = exp(ln_MRmgO2_per_h)*24*19.3*22.4/32)

# Build the fitting function 
metEqun = function(m, v, t, A, B, C, D) {
  A*m^B*exp(C*t)*exp(D*v)
}
# fit the fitting function
metModel = nls(met_j_per_day ~ metEqun(mass, swimSpeed_cm_per_s, temperature_C, A, B, C, D),
                data = metData,
                weights = 1/met_j_per_day^2,
                start = list(A = 70, B = 0.8, C = 0.01 , D = 0.01 ), 
                trace = T)

fishRespParamA = coef(metModel)[[1]]
fishRespParamB = coef(metModel)[[2]]
fishRespParamC = coef(metModel)[[3]]
fishRespParamD = coef(metModel)[[4]]

##### Distance a fish explorers (fishMoveDistParam) #####
# Load the data
moveData = read.xlsx(xlsxFile = "./inSALMO Fish Parameters.xlsx",
                    sheet = "fishMoveDistParam",
                    na.strings = "NA") %>% 
  filter(!is.na(year)) %>% 
  mutate(ln_size = log(size_cm),
         ln_distance = log(distance_cm))

# make a linear model
moveModel = lm(moveData$ln_distance ~ moveData$ln_size,
                    data=moveData)

# Calculate the parameters
fishMoveDistParamA = exp(moveModel$coefficients[1])
fishMoveDistParamB = moveModel$coefficients[2]

# Enter the fit by eye overides
fishMoveDistParamAOveride = 1225
fishMoveDistParamBOveride = 1.2

# make ploting data
plotMoveData = data.frame(length = c(seq(0.5,100,0.5))) %>%
  mutate(linearModel = fishMoveDistParamA*length^fishMoveDistParamB,
         eyeFit = fishMoveDistParamAOveride*length^fishMoveDistParamBOveride)  

# plot the data

ggplot(plotMoveData, aes(x = length)) +
  scale_y_log10() +
  theme_classic(base_size = 30) +
  labs(y = "move", x = "length") +
  geom_point(data = moveData, aes(x = size_cm, y = distance_cm)) +
  geom_path(aes(y = eyeFit), color = "red") +
  geom_path(aes(y = linearModel))

##### Fish search food value (fishSearch) ######
# Load the data
fishSearch = read.xlsx(xlsxFile = "./inSALMO Fish Parameters.xlsx",
                     sheet = "fishSearch",
                     na.strings = "NA") %>% 
  # Just look at Tubifex bacause mot common in Chinook diet
  filter(prey == "Tubifex") %>% 
  mutate(area_covered = feedingRate_g_per_s/density_per_m2/weight_g*3600*24*1e4) %>% 
  group_by(author, year) %>% 
  summarize(area = sum(area_covered)) %>% 
  ungroup() %>% 
  summarise(area = mean(area)) %>% 
  .$area

##### Fish detection distance parameter (fishDetectDistParam) #####
# Load in the fecundity data
detectionData = read.xlsx(xlsxFile = "./inSALMO Fish Parameters.xlsx",
                          sheet = "fishDetectDistParam",
                          na.strings = "NA") 
# do a lm with a 0 intercept (thats the -1 part)
# do this because otherwise the slope is (-)
fishDetectDistA = 0
detectionModel = lm(detectionData$reactionDistance_cm ~ detectionData$fLength_cm - 1,
                    data=detectionData)

# Calculate the inslamo parameters
fishDetectDistB = detectionModel$coefficients[1]

# Made data for a graph
detectionDataWFit = detectionData %>% 
  mutate(predict = predict(detectionModel, type = "response")) %>% 
  arrange(fLength_cm)

# Make a plot

ggplot(detectionDataWFit, aes(x = fLength_cm)) +
  theme_classic(base_size = 30) +
  labs(y = "Reaction Dist", x = "Length") +
  geom_point(aes(y = reactionDistance_cm)) +
  geom_path(aes(y = predict), color = "red")


##### The turbidity function (turbidityFunction) #####
# Set the max turbidity for analysis
maxTurb = 50

# Load the data
turbidityData = read.xlsx(xlsxFile = "./inSALMO Fish Parameters.xlsx",
                          sheet = "turbidityFunction",
                          na.strings = "NA") %>% 
  # Filter only the ones with Chinook on Plankton
  filter(species == "Chinook",
         prey == "Plankton") %>% 
  # subtract off the first non zero turbidity value
  mutate(subtractTurbidity = turbidity_NTU - min(turbidity_NTU[(which(turbidity_NTU>0))]),
         #scale the distances
         scaledDistance = reactionDistance_cm/max(reactionDistance_cm),
         ln_dist = log(scaledDistance),
         ln_turb = log(subtractTurbidity))

# get the minimum non zero turbidity that dosen affect distance, this is the threshold
fishTurbidThreshold = min(turbidityData$turbidity_NTU[(which(turbidityData$turbidity_NTU>0))])

# get the minimum scaled distance, this is the Minimum value of the turbidity function
fishTurbidMin = min(turbidityData$scaledDistance)

# Get the data to fit the fishTurbidExp
turbidityFitData = turbidityData %>% 
  filter(turbidity_NTU>fishTurbidThreshold, 
         subtractTurbidity < maxTurb)

turbidityModel = lm(turbidityFitData$ln_dist ~ turbidityFitData$subtractTurbidity - 1,
                    data=turbidityFitData)

# Calculate the inslamo parameters
fishTurbidExp = turbidityModel$coefficients[1]

# Made data for a graph
turbDataWFit = turbidityFitData %>% 
  mutate(predict = predict(turbidityModel, type = "response")) %>% 
  arrange(subtractTurbidity)

# Make a plot

ggplot(turbDataWFit, aes(x = subtractTurbidity)) +
  theme_classic(base_size = 30) +
  labs(y = "Dist. (cm)", x = "Subtracted Turb. (NTU)") +
  geom_point(aes(y = ln_dist)) +
  geom_path(aes(y = predict), color = "red")


##### Probablity of fish capture (fishCaptureParam) #####
# load the data
captureData = read.xlsx(xlsxFile = "./inSALMO Fish Parameters.xlsx",
                        sheet = "fishCaptureParam",
                        na.strings = "NA") %>% 
  mutate(ratio_V_per_Max = velocity_cm_per_s/maxSwimSpeed_cm_per_s)  
  
# do a logistic fit
captureModel = glm(captureData$prob ~ captureData$ratio_V_per_Max,
                   #family=binomial(logit),
                   family=quasibinomial(logit),
                   data=captureData)

# add in predictions for plotting
captureWFitData = captureData %>% 
  mutate(predict = predict.glm(captureModel, type = "response")) %>% 
  arrange(ratio_V_per_Max)

# solve for inSALMO Parameters 
fishCaptureParam1 = -(log(1/0.1-1)+captureModel[[1]][1])/captureModel[[1]][2]
fishCaptureParam9 = -(log(1/0.9-1)+captureModel[[1]][1])/captureModel[[1]][2]

# Plot

ggplot(captureWFitData, aes(x = ratio_V_per_Max)) +
  theme_classic(base_size = 30) +
  labs(y = "Capture Prob", x = "Relative Vel.") +
  geom_point(aes(y = prob)) +
  geom_path(aes(y = predict), color = "red")


##### Maximum consumption (fishCmaxParam) #####
# Enter parameters for calculation from Plumb & Moffitt Trans Am Fish 2015
CQ =	4.97
CTO	= 20.93
CTM	= 20.93
CTL	= 24.05
CK1	= 0.09
CK4	= 0.53

# Load the data
cmaxData = read.xlsx(xlsxFile = "./inSALMO Fish Parameters.xlsx",
                        sheet = "fishCmaxParam",
                        na.strings = "NA") %>% 
  # Do calculations in Plumb & Moffitt Trans Am Fish 2015
  mutate(averageWt_g = initWt_g*(exp(days*growthRate_percent_per_day)-1)/growthRate_percent_per_day/days,
         ln_avWt = log(averageWt_g),
         dailyFood_g_per_day = food_g/days,
         G1 = 1/(CTO-CQ)*log(0.98*(1-CK1)/CK1/0.02),
         G2 = 1/(CTL-CTM)*log(0.98*(1-CK4)/CK4/0.02),
         L1 = exp(G1*(temperature_C-CQ)),
         L2 = exp(G2*(CTL-temperature_C)),
         KA = CK1*L1/(1+CK1*(L1-1)), 
         KB = CK4*L2/(1+CK4*(L2-1)),
         F1 = KB*KA,
         CmaxOverCorr = dailyFood_g_per_day/F1,
         ln_g_per_d = log(CmaxOverCorr))
         
cmaxModel = lm(cmaxData$ln_g_per_d ~ cmaxData$ln_avWt,
                    data=cmaxData)

# Calculate the inslamo parameters
# old parameters 1.99 and -0.59 because of day error 
fishCmaxParamA = exp(cmaxModel$coefficients[1])
fishCmaxParamB = cmaxModel$coefficients[2]-1

# Made data for a graph
cmaxDataWFit = cmaxData %>% 
  mutate(predict = predict(cmaxModel, type = "response")) %>% 
  arrange(ln_avWt)

# Make a plot

ggplot(cmaxDataWFit, aes(x = ln_avWt)) +
  theme_classic(base_size = 30) +
  labs(y = "ln(CMax)", x = "ln(weight)") +
  geom_point(aes(y = ln_g_per_d)) +
  geom_path(aes(y = predict), color = "red")



##### Maximum consumption vs T (fishCmaxTemp) #####
cmaxCurveData = read.xlsx(xlsxFile = "./inSALMO Fish Parameters.xlsx",
                              sheet = "fishCmaxTemp",
                              na.strings = "NA") %>% 
  mutate(value = value/max(value))

# enter the data to match the curve by eye  
fishCmaxTempF = c(0.015, 0.135, 0.933, 1.000, 0.078, 0.000, 0.000)
fishCmaxTempT = c(0.072, 5.921, 15.90, 21.60, 26.23, 30.00, 100.0)


matchCmaxData = data.frame(temperature = fishCmaxTempT,
                        value = fishCmaxTempF) 

# Plot the data

ggplot(cmaxCurveData, aes(x = temperature)) +
  theme_classic(base_size = 30) +
  labs(y = "RSF", x = "Velocity (m/s)") +
  scale_x_continuous(limits = c(0, 30)) +
  geom_path(aes(y = value), color = "black", alpha = 0.2, size = 3) +
  geom_point(data = matchCmaxData, aes(x = temperature, y = value), shape = 1, size = 3, stroke = 2, color = "red") +
  geom_path(data = matchCmaxData, aes(x = temperature, y = value), color = "red", size = 1 ) 


##### Condition survival (mortFishCondition) #####
# load the data
conSurvivalData = read.xlsx(xlsxFile = "./inSALMO Fish Parameters.xlsx",
                        sheet = "mortFishCondition",
                        na.strings = "NA") %>%
  na.omit()

# do a logistic fit
conSurvivalModel = glm(conSurvivalData$dailySurvival ~ conSurvivalData$conFactor,
                   #family=binomial(logit),
                   family=quasibinomial(logit),
                   data=conSurvivalData)

# add in predictions for plotting
conSurvivalDataWFitData = conSurvivalData %>% 
  mutate(predict = predict.glm(conSurvivalModel, type = "response")) %>% 
  arrange(conFactor)

# solve for inSALMO Parameters 
mortFishCondition1 = -(log(1/0.1-1)+conSurvivalModel[[1]][1])/conSurvivalModel[[1]][2]
mortFishCondition9 = -(log(1/0.9-1)+conSurvivalModel[[1]][1])/conSurvivalModel[[1]][2]

# Plot

ggplot(conSurvivalDataWFitData, aes(x = conFactor)) +
  theme_classic(base_size = 30) +
  labs(y = "Capture Prob", x = "Relative Vel.") +
  geom_point(aes(y = dailySurvival)) +
  geom_path(aes(y = predict), color = "red")


##### High temperature mortality (mortFishHiTT) #####
# load the data
tempSurvivalData = read.xlsx(xlsxFile = "./inSALMO Fish Parameters.xlsx",
                            sheet = "mortFishHiTT",
                            na.strings = "NA") %>% 
  filter(expSetup == "Lab") 

# do a logistic fit
tempSurvivalModel = glm(tempSurvivalData$dailySurvival ~ tempSurvivalData$temperature,
                       #family=binomial(logit),
                       family=quasibinomial(logit),
                       data=tempSurvivalData)

# add in predictions for plotting
tempSurvivalWFitData = tempSurvivalData %>% 
  mutate(predict = predict.glm(tempSurvivalModel, type = "response")) %>% 
  arrange(temperature)

# solve for inSALMO Parameters 
mortFishHiTT1 = -(log(1/0.1-1)+tempSurvivalModel[[1]][1])/tempSurvivalModel[[1]][2]
mortFishHiTT9 = -(log(1/0.9-1)+tempSurvivalModel[[1]][1])/tempSurvivalModel[[1]][2]

# Plot

ggplot(tempSurvivalWFitData, aes(x = temperature)) +
  theme_classic(base_size = 30) +
  labs(y = "Survival", x = "Temperature (C)") +
  geom_point(aes(y = dailySurvival)) +
  geom_path(aes(y = predict), color = "red")


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

# do a logistic fit
predDepthModel = glm(predDepthData$fractionalOccurrence ~ predDepthData$value,
                        family=quasibinomial(logit),
                        data=predDepthData)

# add in predictions for plotting
predDepthWFitData = predDepthData %>% 
  filter(!is.na(fractionalOccurrence)) %>% 
  mutate(predict = predict.glm(predDepthModel, type = "response")) %>% 
  arrange(value)

# solve for inSALMO Parameters 
# convert form m to cm
mortFishAqPredD1 = -(log(1/0.1-1)+predDepthModel[[1]][1])/predDepthModel[[1]][2]*100
mortFishAqPredD9 = -(log(1/0.9-1)+predDepthModel[[1]][1])/predDepthModel[[1]][2]*100

# Plot

ggplot(predDepthWFitData, aes(x = value)) +
  theme_classic(base_size = 30) +
  labs(y = "Fraction present", x = "Depth (m)") +
  geom_point(aes(y = fractionalOccurrence)) +
  geom_path(aes(y = predict), color = "red")


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

# do a logistic fit
predDistModel = glm(predDistData$fractionalOccurrence ~ predDistData$value,
                     family=quasibinomial(logit),
                     data=predDistData)

# add in predictions for plotting
predDistWFitData = predDistData %>% 
  filter(!is.na(fractionalOccurrence)) %>% 
  mutate(predict = predict.glm(predDistModel, type = "response")) %>% 
  arrange(value)

# solve for inSALMO Parameters 
# convert form m to cm
mortFishAqPredD1 = -(log(1/0.1-1)+predDistModel[[1]][1])/predDistModel[[1]][2]*100
mortFishAqPredD9 = -(log(1/0.9-1)+predDistModel[[1]][1])/predDistModel[[1]][2]*100

# Plot

ggplot(predDistWFitData, aes(x = value)) +
  theme_classic(base_size = 30) +
  labs(y = "Fraction present", x = "Dist (m)") +
  geom_point(aes(y = fractionalOccurrence)) +
  geom_path(aes(y = predict), color = "red")



##### Data on predation preventation from temperature (mortFishAqPredT) #####
# load the data
# This uses physological measures of predators to get the potential predation effect of T
predTData = read.xlsx(xlsxFile = "./inSALMO Fish Parameters.xlsx",
                             sheet = "mortAqByPredMet",
                             na.strings = "NA") %>% 
  group_by(author, year, journal) %>% 
  mutate(unitlessValue = value/max(value)) %>% 
  ungroup() 

# do a logistic fit
predTModel = glm(predTData$unitlessValue ~ predTData$x,
                    family=quasibinomial(logit),
                    data=predTData)

# add in predictions for plotting
predTWFitData = predTData %>% 
  mutate(predict = predict.glm(predTModel, type = "response")) %>% 
  arrange(x)

# solve for inSALMO Parameters 
# convert form m to cm
mortFishAqPredT1 = -(log(1/0.1-1)+predTModel[[1]][1])/predTModel[[1]][2]
mortFishAqPredT9 = -(log(1/0.9-1)+predTModel[[1]][1])/predTModel[[1]][2]

# Plot

ggplot(predTWFitData, aes(x = x)) +
  theme_classic(base_size = 30) +
  labs(y = "Fraction present", x = "T (C)") +
  geom_point(aes(y = unitlessValue)) +
  geom_path(aes(y = predict), color = "red")




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
         #dailySurvival = ifelse(units == "not eaten", measure, dailySurvival))
  # filter(units == "not eaten") %>%
  # mutate(dailySurvival = measure)

# do a logistic fit
predLModel = glm(predLData$dailySurvival ~ predLData$length_cm,
                 family=quasibinomial(logit),
                 data=predLData)

# add in predictions for plotting
predLWFitData = predLData %>% 
  mutate(predict = predict.glm(predLModel, type = "response")) %>% 
  arrange(length_cm)

# solve for inSALMO Parameters 
# convert form m to cm
mortFishAqPredL1 = -(log(1/0.1-1)+predLModel[[1]][1])/predLModel[[1]][2]
mortFishAqPredL9 = -(log(1/0.9-1)+predLModel[[1]][1])/predLModel[[1]][2]

# we are using as a min survival so want to pass under the data not through it
mortFishAqPredL1 = 2
mortFishAqPredL9 = 8
LogistB	= log((0.9/0.1)^2)/(mortFishAqPredL9-mortFishAqPredL1)
LogistA	= log(0.1/0.9)-LogistB*(mortFishAqPredL1)

predLWFitData = predLWFitData %>% 
  mutate(overide = exp(LogistA+LogistB*length_cm)/(1+exp(LogistA+LogistB*length_cm)))

# Plot

ggplot(predLWFitData, aes(x = length_cm)) +
  theme_classic(base_size = 30) +
  labs(y = "Fraction present", x = "Length (cm)") +
  geom_point(aes(y = dailySurvival)) +
  geom_path(aes(y = predict), color = "red")+
  geom_path(aes(y = overide), color = "blue")+
  ggtitle("Using mortFishByMort data")


##### Data on predation preventation from turbidity (mortFishAqPredU) #####
# load the data
predLData = read.xlsx(xlsxFile = "./inSALMO Fish Parameters.xlsx",
                      sheet = "mortFishByMort",
                      na.strings = "NA") %>% 
  filter(note != "outlier",
         !is.na(turb_NTU)) %>% 
  mutate(dailySurvival = NA,
         dailySurvival = ifelse(units == "survival", measure^(1/time_days), dailySurvival),
         dailySurvival = ifelse(units == "daily survival", measure, dailySurvival),
         dailySurvival = ifelse(units == "relative vlun.", 1-measure, dailySurvival),
         dailySurvival = dailySurvival-min(dailySurvival),
         dailySurvival = dailySurvival/max(dailySurvival)) 

# do a logistic fit
predLModel = glm(predLData$dailySurvival ~ predLData$turb_NTU,
                 family=quasibinomial(logit),
                 data=predLData)

# add in predictions for plotting
predLWFitData = predLData %>% 
  mutate(predict = predict.glm(predLModel, type = "response")) %>% 
  arrange(turb_NTU)

# solve for inSALMO Parameters 
# convert form m to cm
mortFishAqPredL1 = -(log(1/0.1-1)+predLModel[[1]][1])/predLModel[[1]][2]
mortFishAqPredL9 = -(log(1/0.9-1)+predLModel[[1]][1])/predLModel[[1]][2]

# overide probably with new scaling should take out
mortFishAqPredL1 = -50
mortFishAqPredL9 = 35
LogistB	= log((0.9/0.1)^2)/(mortFishAqPredL9-mortFishAqPredL1)
LogistA	= log(0.1/0.9)-LogistB*(mortFishAqPredL1)

predLWFitData = predLWFitData %>% 
  mutate(overide = exp(LogistA+LogistB*turb_NTU)/(1+exp(LogistA+LogistB*turb_NTU)))

# Plot

ggplot(predLWFitData, aes(x = turb_NTU)) +
  theme_classic(base_size = 30) +
  labs(y = "Fraction present", x = "T (C)") +
  geom_point(aes(y = dailySurvival)) +
  geom_path(aes(y = predict), color = "red")+
  geom_path(aes(y = overide), color = "blue")



