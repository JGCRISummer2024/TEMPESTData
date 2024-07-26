#read in the tempest and gcw data
library(readr)
tempestDf <- read_csv("C:\\Users\\jung708\\OneDrive - PNNL\\Documents\\GitHub\\TEMPESTData\\TMP_C_20220601-20220630_L1_v1-0.csv")
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
filteredDf1 <- filteredDfa %>% filter(Sensor_ID == "C1" | Sensor_ID == "C2" | Sensor_ID == "C3" | Sensor_ID == "C4" | Sensor_ID == "C5" | Sensor_ID == "C6" | Sensor_ID == "C7" | Sensor_ID == "C8" | Sensor_ID == "C9" | Sensor_ID == "C10" | Sensor_ID == "C11" | Sensor_ID == "C12" | Sensor_ID == "C13" | Sensor_ID == "C14" | Sensor_ID == "C15" | Sensor_ID == "C16" | Sensor_ID == "C17" | Sensor_ID == "C18")
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
  #drop_na() ->
  #finalllldf

#sum(is.na(finalllldf))
#write.csv(finalllldf, file = "testingfinallldf.csv")
#filteredDf5


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

data_daily_sapflow <- data_daily %>% filter(Sensor_ID == "C1" | Sensor_ID == "C2" | Sensor_ID == "C3" | Sensor_ID == "C4" | Sensor_ID == "C5" | Sensor_ID == "C6" | Sensor_ID == "C7" | Sensor_ID == "C8" | Sensor_ID == "C9" | Sensor_ID == "C10" | Sensor_ID == "C11" | Sensor_ID == "C12" | Sensor_ID == "C13" | Sensor_ID == "C14" | Sensor_ID == "C15" | Sensor_ID == "C16" | Sensor_ID == "C17" | Sensor_ID == "C18")
data_daily_sapflow
data_daily_No_sapflow <- data_daily %>% filter(Sensor_ID != "C1" & Sensor_ID != "C2" & Sensor_ID != "C3" & Sensor_ID != "C4" & Sensor_ID != "C5" & Sensor_ID != "C6" & Sensor_ID != "C7" & Sensor_ID != "C8" & Sensor_ID != "C9" & Sensor_ID != "C10" & Sensor_ID != "C11" & Sensor_ID != "C12" & Sensor_ID != "C13" & Sensor_ID != "C14" & Sensor_ID != "C15" & Sensor_ID != "C16" & Sensor_ID != "C17" & Sensor_ID != "C18")
data_daily_No_sapflow
data_daily_No_sapflow %>%
  drop_na() ->
  data_daily_No_sapflow_no_nas

data_daily_No_sapflow_id = subset(data_daily_No_sapflow_no_nas, select = -c(Sensor_ID))
data_daily_No_sapflow_id
write.csv(data_daily_No_sapflow_id, file = "data_daily_No_sapflow_id.csv")

library(dplyr)
data_daily_No_sapflow_id %>%
  pivot_wider(names_from = research_name, values_from = value) ->
  wide_data_daily_No_sapflow_id
wide_data_daily_No_sapflow_id






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


