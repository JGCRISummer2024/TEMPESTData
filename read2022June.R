#read in the tempest and gcw data
library(readr)



dfC <- read_csv("C:\\Users\\jung708\\OneDrive - PNNL\\Documents\\GitHub\\TEMPESTData\\finalWideDfC.csv")
dfS <- read_csv("C:\\Users\\jung708\\OneDrive - PNNL\\Documents\\GitHub\\TEMPESTData\\finalWideDfS.csv")
dfF <- read_csv("C:\\Users\\jung708\\OneDrive - PNNL\\Documents\\GitHub\\TEMPESTData\\finalWideDfF.csv")

library(dplyr)
dfCS <- rbind(dfC, dfS)
dfCSF <- rbind(dfCS, dfF)

dfCSFFinal <- dfCSF[-c(1:2)]
write.csv(dfCSF, file = "wideCSFdataWithIndexes.csv")


dfSpecies <- read_csv("C:\\Users\\jung708\\OneDrive - PNNL\\Documents\\GitHub\\TEMPESTData\\sapflux-species-mapping.csv")
#basically we want to see dif between species
dfSpeciesMap <- dfSpecies[-c(1, 3, 5)]
dfSpeciesMap

#First, divide by plots: C, S, F
#Within each plot, divide by species; create new df for each species
#Create a linear regression model for each

#For Plot C
dfCACRU <- dfC %>% filter(Sensor_ID == "C7" | Sensor_ID == "C8" | Sensor_ID == "C10" | Sensor_ID == "C11" | Sensor_ID == "C9" | Sensor_ID == "C12")
dfCACRU
dfCLITU <- dfC %>% filter(Sensor_ID == "C3" | Sensor_ID == "C6" | Sensor_ID == "C1" | Sensor_ID == "C2" | Sensor_ID == "C4" | Sensor_ID == "C5")
dfCLITU
dfCFAGR <- dfC %>% filter(Sensor_ID == "C16" | Sensor_ID == "C18" | Sensor_ID == "C13" | Sensor_ID == "C14" | Sensor_ID == "C15" | Sensor_ID == "C17")
dfFAGR

#ACRU
modelCACRU = lm(sapflow_2.5cm ~ (wx_par_den15 + soil_vwc_15cm + wx_tempavg15 + wx_windspeed15), data = dfCACRU)
summary(modelCACRU)
ggplot(modelCACRU, aes(x = wx_par_den15, y = sapflow_2.5cm)) +geom_point() + ggtitle("Plot C ACRU Sapflow vs Par") + stat_smooth(method = "lm", col = "dodgerblue3")

#LITU
modelCLITU = lm(sapflow_2.5cm ~ (wx_par_den15 + soil_vwc_15cm + wx_tempavg15 + wx_windspeed15), data = dfCLITU)
summary(modelCLITU)
ggplot(modelCLITU, aes(x = wx_par_den15, y = sapflow_2.5cm)) +geom_point() + ggtitle("Plot C LITU Sapflow vs Par") + stat_smooth(method = "lm", col = "dodgerblue3")

#FAGR
modelCFAGR = lm(sapflow_2.5cm ~ (wx_par_den15 + soil_vwc_15cm + wx_tempavg15 + wx_windspeed15), data = dfCFAGR)
summary(modelCFAGR)
ggplot(modelCFAGR, aes(x = wx_par_den15, y = sapflow_2.5cm)) +geom_point() + ggtitle("Plot C FAGR Sapflow vs Par") + stat_smooth(method = "lm", col = "dodgerblue3")

#For Plot S
dfSACRU <- dfS %>% filter(Sensor_ID == "S7" | Sensor_ID == "S8" | Sensor_ID == "S10" | Sensor_ID == "S11" | Sensor_ID == "S9" | Sensor_ID == "S12")
dfSACRU
dfSLITU <- dfS %>% filter(Sensor_ID == "S3" | Sensor_ID == "S6" | Sensor_ID == "S1" | Sensor_ID == "S2" | Sensor_ID == "S4" | Sensor_ID == "S5")
dfSLITU
dfSFAGR <- dfS %>% filter(Sensor_ID == "S16" | Sensor_ID == "S18" | Sensor_ID == "S13" | Sensor_ID == "S14" | Sensor_ID == "S15" | Sensor_ID == "S17")
dfSFAGR

#ACRU
modelSACRU = lm(sapflow_2.5cm ~ (wx_par_den15 + soil_vwc_15cm + wx_tempavg15 + wx_windspeed15), data = dfSACRU)
summary(modelSACRU)
ggplot(modelSACRU, aes(x = wx_par_den15, y = sapflow_2.5cm)) +geom_point() + ggtitle("Plot S ACRU Sapflow vs Par") + stat_smooth(method = "lm", col = "dodgerblue3")

#LITU
modelSLITU = lm(sapflow_2.5cm ~ (wx_par_den15 + soil_vwc_15cm + wx_tempavg15 + wx_windspeed15), data = dfSLITU)
summary(modelSLITU)
ggplot(modelSLITU, aes(x = wx_par_den15, y = sapflow_2.5cm)) +geom_point() + ggtitle("Plot S LITU Sapflow vs Par") + stat_smooth(method = "lm", col = "dodgerblue3")

#FAGR
modelSFAGR = lm(sapflow_2.5cm ~ (wx_par_den15 + soil_vwc_15cm + wx_tempavg15 + wx_windspeed15), data = dfSFAGR)
summary(modelSFAGR)
ggplot(modelSFAGR, aes(x = wx_par_den15, y = sapflow_2.5cm)) +geom_point() + ggtitle("Plot S FAGR Sapflow vs Par") + stat_smooth(method = "lm", col = "dodgerblue3")


#For Plot F
dfFACRU <- dfF %>% filter(Sensor_ID == "F7" | Sensor_ID == "F8" | Sensor_ID == "F10" | Sensor_ID == "F11" | Sensor_ID == "F9" | Sensor_ID == "F12")
dfFACRU
dfFLITU <- dfF %>% filter(Sensor_ID == "F3" | Sensor_ID == "F6" | Sensor_ID == "F1" | Sensor_ID == "F2" | Sensor_ID == "F4" | Sensor_ID == "F5")
dfFLITU
dfFFAGR <- dfF %>% filter(Sensor_ID == "F16" | Sensor_ID == "F18" | Sensor_ID == "F13" | Sensor_ID == "F14" | Sensor_ID == "F15" | Sensor_ID == "F17")
dfFFAGR

#ACRU
modelFACRU = lm(sapflow_2.5cm ~ (wx_par_den15 + soil_vwc_15cm + wx_tempavg15 + wx_windspeed15), data = dfFACRU)
summary(modelFACRU)
ggplot(modelFACRU, aes(x = wx_par_den15, y = sapflow_2.5cm)) +geom_point() + ggtitle("Plot F ACRU Sapflow vs Par") + stat_smooth(method = "lm", col = "dodgerblue3")

#LITU
modelFLITU = lm(sapflow_2.5cm ~ (wx_par_den15 + soil_vwc_15cm + wx_tempavg15 + wx_windspeed15), data = dfFLITU)
summary(modelFLITU)
ggplot(modelFLITU, aes(x = wx_par_den15, y = sapflow_2.5cm)) +geom_point() + ggtitle("Plot F LITU Sapflow vs Par") + stat_smooth(method = "lm", col = "dodgerblue3")

#FAGR
modelFFAGR = lm(sapflow_2.5cm ~ (wx_par_den15 + soil_vwc_15cm + wx_tempavg15 + wx_windspeed15), data = dfFFAGR)
summary(modelFFAGR)
ggplot(modelFFAGR, aes(x = wx_par_den15, y = sapflow_2.5cm)) +geom_point() + ggtitle("Plot F FAGR Sapflow vs Par") + stat_smooth(method = "lm", col = "dodgerblue3")

#Now let's try an lstm model
# Define the LSTM model
library(tensorflow)
library(keras)
lstmmodel <- keras_model_sequential()
lstmmodel %>%
  layer_lstm(units = 50, input_shape = c(10, 10)) %>%  # units = number of LSTM cells
  layer_dense(units = 10, activation = 'softmax')

# Compile the model
lstmmodel %>% compile(
  loss = 'categorical_crossentropy',  # Loss function appropriate for your task
  optimizer = optimizer_adam(),  # Optimizer like Adam
  metrics = c('accuracy')  # Metrics to monitor during training
)
summary(lstmmodel)

#-------------------------------------------------------------------------

write.csv(dfCSFFinal, file = "wideCSFdataFinal.csv")

tempestDf <- read_csv("C:\\Users\\jung708\\OneDrive - PNNL\\Documents\\GitHub\\TEMPESTData\\TMP_F_20220601-20220630_L1_v1-0.csv")
weatherDf <- read_csv("C:\\Users\\jung708\\OneDrive - PNNL\\Documents\\GitHub\\TEMPESTData\\GCW_W_20220601-20220630_L1_v1-0.csv")

#TreesTempestDf <- tempestDf %>% filter(Sensor_ID == "C1" | Sensor_ID == "C2" | Sensor_ID == "C3" | Sensor_ID == "C4" | Sensor_ID == "C5" | Sensor_ID == "C6" | Sensor_ID == "C7" | Sensor_ID == "C8" | Sensor_ID == "C9" | Sensor_ID == "C10" | Sensor_ID == "C11" | Sensor_ID == "C12" | Sensor_ID == "C13" | Sensor_ID == "C14" | Sensor_ID == "C15" | Sensor_ID == "C16" | Sensor_ID == "C17" | Sensor_ID == "C18")
#TreesTempestDf %>%
  #select(research_name)

#bind the two datasets
library(dplyr)
df <- rbind(tempestDf, weatherDf)
df

#filteredDf <- df %>% filter(research_name == "sapflow_2.5cm" | research_name == "wx_tempavg15" | research_name == "wx_windspeed15" | research_name == "wx_par_den15" | research_name == "soil_vwc_15cm")
#filteredDf
#filter the data one by one via sapflow and its potential predictors
filteredDf0 <- df %>% filter(research_name == "sapflow_2.5cm")
filteredDfa <- filteredDf0 %>% filter(Value >= 0) #make sure the value is not negative
#filteredDf1 <- filteredDfa %>% filter(Sensor_ID == "C1" | Sensor_ID == "C2" | Sensor_ID == "C3" | Sensor_ID == "C4" | Sensor_ID == "C5" | Sensor_ID == "C6" | Sensor_ID == "C7" | Sensor_ID == "C8" | Sensor_ID == "C9" | Sensor_ID == "C10" | Sensor_ID == "C11" | Sensor_ID == "C12" | Sensor_ID == "C13" | Sensor_ID == "C14" | Sensor_ID == "C15" | Sensor_ID == "C16" | Sensor_ID == "C17" | Sensor_ID == "C18")
filteredDf1 <- filteredDfa %>% filter(Sensor_ID == "F1" | Sensor_ID == "F2" | Sensor_ID == "F3" | Sensor_ID == "F4" | Sensor_ID == "F5" | Sensor_ID == "F6" | Sensor_ID == "F7" | Sensor_ID == "F8" | Sensor_ID == "F9" | Sensor_ID == "F10" | Sensor_ID == "F11" | Sensor_ID == "F12" | Sensor_ID == "F13" | Sensor_ID == "F14" | Sensor_ID == "F15" | Sensor_ID == "F16" | Sensor_ID == "F17" | Sensor_ID == "F18")

filteredDf1

filteredDf2 <- df %>% filter(research_name == "wx_tempavg15")
filteredDf2
sum(is.na(filteredDf2))
write.csv(filteredDf2, file = "filteredDf2.csv")
filteredDf3 <- df %>% filter(research_name == "wx_windspeed15")
filteredDf3
sum(is.na(filteredDf3))
write.csv(filteredDf3, file = "filteredDf3.csv")
filteredDf4 <- df %>% filter(research_name == "wx_par_den15")
filteredDf4
sum(is.na(filteredDf4))
filteredDf5 <- df %>% filter(research_name == "soil_vwc_15cm")
filteredDf5
sum(is.na(filteredDf5))

#bind everything together
fDf12 <- rbind(filteredDf1, filteredDf2)
fDf12
fDf123 <- rbind(fDf12, filteredDf3)
fDf123
fDf1234 <- rbind(fDf123, filteredDf4)
fDf1234
finallldf <- rbind(fDf1234, filteredDf5)

finallldf
sum(is.na(finallldf)) #check if there are NA values
which(is.na(finallldf))

#finallldf %>%
 # drop_na() ->
  #finalllldf

#sum(is.na(finalllldf))
#write.csv(finalllldf, file = "testingfinallldf.csv")
#filteredDf5

library(lubridate)

finallldf %>% #daytime is sunrise to sunset 6:00AM to 9:00PM
  #select(TIMESTAMP) %>%
  mutate(day = date(TIMESTAMP), 
         time = hms::as_hms(TIMESTAMP), #extract day and time
         daytime = time >= hms("06:00:00") & time <= hms("21:00:00")) %>% 
  filter(daytime) %>%
  group_by(day, Sensor_ID, research_name) %>% #add the sensor ID, then we get the NA issue
  summarize(value = mean(Value)) -> 
  data_daily
data_daily
write.csv(data_daily, file = "data_daily.csv")


data_daily_sapflow <- data_daily %>% filter(Sensor_ID == "F1" | Sensor_ID == "F2" | Sensor_ID == "F3" | Sensor_ID == "F4" | Sensor_ID == "F5" | Sensor_ID == "F6" | Sensor_ID == "F7" | Sensor_ID == "F8" | Sensor_ID == "F9" | Sensor_ID == "F10" | Sensor_ID == "F11" | Sensor_ID == "F12" | Sensor_ID == "F13" | Sensor_ID == "F14" | Sensor_ID == "F15" | Sensor_ID == "F16" | Sensor_ID == "F17" | Sensor_ID == "F18")
data_daily_sapflow
write.csv(data_daily_sapflow, file = "data_daily_sapflow.csv")
data_daily_No_sapflow <- data_daily %>% filter(research_name == "soil_vwc_15cm" | research_name == "wx_par_den15" | research_name == "wx_tempavg15" | research_name == "wx_windspeed15")
data_daily_No_sapflow
write.csv(data_daily_No_sapflow, file = "data_daily_no_sapflow.csv")

library(dplyr)
library(tidyr)

#data_daily_No_sapflow %>%
  #drop_na() ->
  #data_daily_No_sapflow_no_nas

data_daily_No_sapflow_id = subset(data_daily_No_sapflow, select = -c(Sensor_ID))
data_daily_No_sapflow_id
write.csv(data_daily_No_sapflow_id, file = "data_daily_No_sapflow_id.csv")

library(dplyr)
data_daily_No_sapflow_id %>%
  drop_na() ->
  data_daily_No_sapflow_id_nas
data_daily_No_sapflow_id_nas %>%
  pivot_wider(names_from = research_name, values_from = value, values_fn = mean) ->
  wide_data_daily_No_sapflow_id
wide_data_daily_No_sapflow_id

data_daily_sapflow %>%
  pivot_wider(names_from = research_name, values_from = value, values_fn = mean) ->
  wide_data_daily_sapflow
wide_data_daily_sapflow

#bind the two datasets
library(dplyr)
# Assuming 'day' is the common key
finalWideDf <- left_join(wide_data_daily_No_sapflow_id, 
                         wide_data_daily_sapflow %>% select(day, Sensor_ID, sapflow_2.5cm),
                         by = "day")

#finalWideDf <- rbind(wide_data_daily_No_sapflow_id, wide_data_daily_sapflow)
finalWideDf
write.csv(finalWideDf, file = "finalWideDfF.csv")




#research_name == "soil_vwc_15cm" | research_name == "wx_par_den15" | research_name == "wx_tempavg15" | research_name == "wx_windspeed15")

#Let's investigate through each tree

#testing C1
finalWideDfC1 <- finalWideDf %>% filter(Sensor_ID == "C1")
finalWideDfC1
library(ggplot2)
modelC1 = lm(sapflow_2.5cm ~ (wx_par_den15 + soil_vwc_15cm + wx_tempavg15 + wx_windspeed15), data = finalWideDfC1)
summary(modelC1)
ggplot(finalWideDfC1, aes(x = wx_par_den15, y = sapflow_2.5cm)) +geom_point()

#Testing C2
finalWideDfC2 <- finalWideDf %>% filter(Sensor_ID == "C2")
finalWideDfC2
modelC2 = lm(sapflow_2.5cm ~ (wx_par_den15 + soil_vwc_15cm + wx_tempavg15 + wx_windspeed15), data = finalWideDfC2)
summary(modelC2)
ggplot(finalWideDfC2, aes(x = wx_par_den15, y = sapflow_2.5cm)) +geom_point()


#Testing C3
finalWideDfC3 <- finalWideDf %>% filter(Sensor_ID == "C3")
finalWideDfC3
modelC3 = lm(sapflow_2.5cm ~ (wx_par_den15 + soil_vwc_15cm + wx_tempavg15 + wx_windspeed15), data = finalWideDfC3)
summary(modelC3)
ggplot(finalWideDfC3, aes(x = wx_par_den15, y = sapflow_2.5cm)) +geom_point()


#Testing C4
finalWideDfC4 <- finalWideDf %>% filter(Sensor_ID == "C4")
finalWideDfC4
modelC4 = lm(sapflow_2.5cm ~ (wx_par_den15 + soil_vwc_15cm + wx_tempavg15 + wx_windspeed15), data = finalWideDfC4)
summary(modelC4)
ggplot(finalWideDfC4, aes(x = wx_par_den15, y = sapflow_2.5cm)) +geom_point()


#Testing C5
finalWideDfC5 <- finalWideDf %>% filter(Sensor_ID == "C5")
finalWideDfC5
modelC5 = lm(sapflow_2.5cm ~ (wx_par_den15 + soil_vwc_15cm + wx_tempavg15 + wx_windspeed15), data = finalWideDfC5)
summary(modelC5)
ggplot(finalWideDfC5, aes(x = wx_par_den15, y = sapflow_2.5cm)) +geom_point()


#Testing C6
finalWideDfC6 <- finalWideDf %>% filter(Sensor_ID == "C6")
finalWideDfC6
modelC6 = lm(sapflow_2.5cm ~ (wx_par_den15 + soil_vwc_15cm + wx_tempavg15 + wx_windspeed15), data = finalWideDfC6)
summary(modelC6)
ggplot(finalWideDfC6, aes(x = wx_par_den15, y = sapflow_2.5cm)) +geom_point()


#Testing C7
finalWideDfC7 <- finalWideDf %>% filter(Sensor_ID == "C7")
finalWideDfC7
modelC7 = lm(sapflow_2.5cm ~ (wx_par_den15 + soil_vwc_15cm + wx_tempavg15 + wx_windspeed15), data = finalWideDfC7)
summary(modelC7)
ggplot(finalWideDfC7, aes(x = wx_par_den15, y = sapflow_2.5cm)) +geom_point()


#Testing C8
finalWideDfC8 <- finalWideDf %>% filter(Sensor_ID == "C8")
finalWideDfC8
modelC8 = lm(sapflow_2.5cm ~ (wx_par_den15 + soil_vwc_15cm + wx_tempavg15 + wx_windspeed15), data = finalWideDfC8)
summary(modelC8)
ggplot(finalWideDfC8, aes(x = wx_par_den15, y = sapflow_2.5cm)) +geom_point()


#Testing C9
finalWideDfC9 <- finalWideDf %>% filter(Sensor_ID == "C9")
finalWideDfC9
modelC9 = lm(sapflow_2.5cm ~ (wx_par_den15 + soil_vwc_15cm + wx_tempavg15 + wx_windspeed15), data = finalWideDfC9)
summary(modelC9)
ggplot(finalWideDfC9, aes(x = wx_par_den15, y = sapflow_2.5cm)) +geom_point()


#Testing C10
finalWideDfC10 <- finalWideDf %>% filter(Sensor_ID == "C10")
finalWideDfC10
modelC10 = lm(sapflow_2.5cm ~ (wx_par_den15 + soil_vwc_15cm + wx_tempavg15 + wx_windspeed15), data = finalWideDfC10)
summary(modelC10)
ggplot(finalWideDfC10, aes(x = wx_par_den15, y = sapflow_2.5cm)) +geom_point()


#Testing C11
finalWideDfC11 <- finalWideDf %>% filter(Sensor_ID == "C11")
finalWideDfC11
modelC11 = lm(sapflow_2.5cm ~ (wx_par_den15 + soil_vwc_15cm + wx_tempavg15 + wx_windspeed15), data = finalWideDfC11)
summary(modelC11)
ggplot(finalWideDfC11, aes(x = wx_par_den15, y = sapflow_2.5cm)) +geom_point()


#Testing C12
finalWideDfC12 <- finalWideDf %>% filter(Sensor_ID == "C12")
finalWideDfC12
modelC12 = lm(sapflow_2.5cm ~ (wx_par_den15 + soil_vwc_15cm + wx_tempavg15 + wx_windspeed15), data = finalWideDfC12)
summary(modelC12)
ggplot(finalWideDfC12, aes(x = wx_par_den15, y = sapflow_2.5cm)) +geom_point()


#Testing C13
finalWideDfC13 <- finalWideDf %>% filter(Sensor_ID == "C13")
finalWideDfC13
modelC13 = lm(sapflow_2.5cm ~ (wx_par_den15 + soil_vwc_15cm + wx_tempavg15 + wx_windspeed15), data = finalWideDfC13)
summary(modelC13)
ggplot(finalWideDfC13, aes(x = wx_par_den15, y = sapflow_2.5cm)) +geom_point()

#Testing C14
finalWideDfC14 <- finalWideDf %>% filter(Sensor_ID == "C14")
finalWideDfC14
modelC14 = lm(sapflow_2.5cm ~ (wx_par_den15 + soil_vwc_15cm + wx_tempavg15 + wx_windspeed15), data = finalWideDfC14)
summary(modelC14)
ggplot(finalWideDfC14, aes(x = wx_par_den15, y = sapflow_2.5cm)) +geom_point()

#Testing C15
finalWideDfC15 <- finalWideDf %>% filter(Sensor_ID == "C15")
finalWideDfC15
modelC15 = lm(sapflow_2.5cm ~ (wx_par_den15 + soil_vwc_15cm + wx_tempavg15 + wx_windspeed15), data = finalWideDfC15)
summary(modelC15)
ggplot(finalWideDfC15, aes(x = wx_par_den15, y = sapflow_2.5cm)) +geom_point()

#Testing C16
finalWideDfC16 <- finalWideDf %>% filter(Sensor_ID == "C16")
finalWideDfC16
modelC16 = lm(sapflow_2.5cm ~ (wx_par_den15 + soil_vwc_15cm + wx_tempavg15 + wx_windspeed15), data = finalWideDfC16)
summary(modelC16)
ggplot(finalWideDfC16, aes(x = wx_par_den15, y = sapflow_2.5cm)) +geom_point()

#Testing C17
finalWideDfC17 <- finalWideDf %>% filter(Sensor_ID == "C17")
finalWideDfC17
modelC17 = lm(sapflow_2.5cm ~ (wx_par_den15 + soil_vwc_15cm + wx_tempavg15 + wx_windspeed15), data = finalWideDfC17)
summary(modelC17)
ggplot(finalWideDfC17, aes(x = wx_par_den15, y = sapflow_2.5cm)) +geom_point()

#Testing C18
finalWideDfC18 <- finalWideDf %>% filter(Sensor_ID == "C18")
finalWideDfC18
modelC18 = lm(sapflow_2.5cm ~ (wx_par_den15 + soil_vwc_15cm + wx_tempavg15 + wx_windspeed15), data = finalWideDfC18)
summary(modelC18)
ggplot(finalWideDfC18, aes(x = wx_par_den15, y = sapflow_2.5cm)) +geom_point()






library(ggplot2)
model1 = lm(sapflow_2.5cm ~ (wx_par_den15 + soil_vwc_15cm + wx_tempavg15 + wx_windspeed15), data = finalWideDf)
summary(model1)
ggplot(finalWideDf, aes(x = wx_par_den15, y = sapflow_2.5cm)) +geom_point()




write.csv(data_daily, file = "IndividTrees_VWCadded_filtered_daily_daytimeavg_TEMPEST_Weather_062022_data.csv")

data_daily %>%
  pivot_wider(names_from = research_name, values_from = value) ->
  wide_data_daily
wide_data_daily
write.csv(wide_data_daily, file = "secondTry_Wide_Trees_Four_predictors_June2022.csv")

newData <- read_csv("C:\\Users\\jung708\\OneDrive - PNNL\\Documents\\GitHub\\TEMPESTData\\secondTry_Wide_Trees_Four_predictors_June2022.csv")

#let's reorganize the data now such that we only have the column for research name for the trees and not the weather
newData %>%
  
#subsetdf <- df %>% filter(research_name == "sapflow_2.5cm" & research_name == "wx_par_den15" & research_name == "wx_tempavg15" & research_name == "wx_windspeed15")
#subsetdf
#stdf <- rbind(sapdf, temperdf)

#stwdf <- rbind(stdf, winddf)

#stwvdf <- rbind(stwdf, naRemovedVwcdf)


#finaldf <- rbind(stwvdf, pardendf)
#finaldf



filteredDf %>%
  drop_na() ->
  noNaFilteredDf
sum(is.na(noNaFilteredDf))


noNaFilteredDf %>% #daytime is sunrise to sunset 6:00AM to 9:00PM
  #select(TIMESTAMP) %>%
  mutate(day = date(TIMESTAMP), 
         time = hms::as_hms(TIMESTAMP), #extract day and time
         daytime = time >= hms("06:00:00") & time <= hms("21:00:00")) %>% 
  filter(daytime) %>%
  group_by(day, Sensor_ID, research_name) %>%
  summarize(value = mean(Value)) -> 
  data_daily
data_daily

write.csv(data_daily, file = "IndividTrees_VWCadded_filtered_daily_daytimeavg_TEMPEST_Weather_062022_data.csv")

data_daily %>%
  pivot_wider(names_from = research_name, values_from = value) ->
  wide_data_daily
wide_data_daily
write.csv(wide_data_daily, file = "wide_correct_four_individTrees_daily_daytimeavg_TEMPEST_Weather_062022_data.csv")


#sapdf <- df %>% filter(research_name == "sapflow_2.5cm")
#sapdf
#temperdf <- df %>% filter(research_name == "wx_tempavg15")
#temperdf
#winddf <- df %>% filter(research_name == "wx_windspeed15")
#winddf
#pardendf <- df %>% filter(research_name == "wx_par_den15")
#pardendf
#vwcdf <- df %>% filter(research_name == "soil_vwc_15cm")
#vwcdf
#sum(is.na(vwcdf))

#for now, ignore the empty
#vwcdf %>%
  #drop_na() ->
  #naRemovedVwcdf
#naRemovedVwcdf
#sum(is.na(naRemovedVwcdf))
  
#subsetdf <- df %>% filter(research_name == "sapflow_2.5cm" & research_name == "wx_par_den15" & research_name == "wx_tempavg15" & research_name == "wx_windspeed15")
#subsetdf
#stdf <- rbind(sapdf, temperdf)

#stwdf <- rbind(stdf, winddf)

#stwvdf <- rbind(stwdf, naRemovedVwcdf)


#finaldf <- rbind(stwvdf, pardendf)
#finaldf


library(tidyverse)
library(lubridate)



library(dplyr)
library(lubridate)
#finaldf %>%
  #mutate(day = date(TIMESTAMP))
#finaldf

#ymd_hms(TIMESTAMP) %>% date())
  
#finaldf

finaldf %>% #daytime is sunrise to sunset 6:00AM to 9:00PM
  #select(TIMESTAMP) %>%
  mutate(day = date(TIMESTAMP), 
         time = hms::as_hms(TIMESTAMP), #extract day and time
         daytime = time >= hms("06:00:00") & time <= hms("21:00:00")) %>% 
  filter(daytime) %>%
  group_by(day, Sensor_ID, research_name) %>%
  summarize(value = mean(Value)) -> 
  data_daily
data_daily

write.csv(data_daily, file = "filtered_daily_daytimeavg_TEMPEST_Weather_062022_data.csv")

data_daily %>%
  pivot_wider(names_from = research_name, values_from = value) ->
  wide_data_daily
wide_data_daily
write.csv(wide_data_daily, file = "wide_four_individTrees_daily_daytimeavg_TEMPEST_Weather_062022_data.csv")


#mod <- lm(speed ~ dist, data = cars)


