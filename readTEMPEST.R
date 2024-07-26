library(readr)
dataC <- read_csv("C:\\Users\\jung708\\OneDrive - PNNL\\Documents\\TEMPEST\\TEMPEST2020\\TMP_2020\\TMP_C_20200301-20200331_L1_v1-0.csv", col_names = TRUE)

library(dplyr)
filteredDataC <- dataC %>% filter(research_name == "sapflow_2.5cm")

library(ggplot2)
#ggplot(filteredDataC, aes(x = TIMESTAMP, y = Value)) +
 # geom_line() +
  #geom_point() +  # Adds points to the line graph for better visualization
  #labs(title = "Feb Line Graph of C with extraneous Values",
   #    x = "Time",
    #   y = "Value") +
  #theme_minimal()

library(readr)
dataF <- read_csv("C:\\Users\\jung708\\OneDrive - PNNL\\Documents\\TEMPEST\\TEMPEST2020\\TMP_2020\\TMP_F_20200301-20200331_L1_v1-0.csv")

library(dplyr)
FilteredDataF <- dataF %>% filter(research_name == "sapflow_2.5cm")
#ggplot(filteredDataF, aes(x = TIMESTAMP, y = Value)) + geom_line() + geom_point() + labs(title = "January Line Graph of F with extraneous", x = "Time", y = "sap flow") + theme_minimal()

library(readr)
dataS <- read.csv("C:\\Users\\jung708\\OneDrive - PNNL\\Documents\\TEMPEST\\TEMPEST2020\\TMP_2020\\TMP_S_20200301-20200331_L1_v1-0.csv")

library(dplyr)
filteredDataS <- dataS %>% filter(research_name == "sapflow_2.5cm")
#ggplot(filteredDataS, aes(x = TIMESTAMP, y = Value)) + geom_line() + geom_point() + labs(title = "January Line Graph of S with extraneous", x = "Time", y = "sap flow") + theme_minimal()
#ggplot(filtereddfS, aes(x = TIMESTAMP, y = Value)) + geom_line() + geom_point() + labs(title = "Line Graph of S", x = "Time", y = "sap flow") + theme_minimal()

library(readr)
#ggplot(filteredDataS, aes(x = TIMESTAMP, y = Value)) + geom_line() + geom_point() + labs(title = "Line Graph of S", x = "Time", y = "sap flow") + theme_minimal()
subsetFilteredDataC <- filteredDataC[1:120, ]
ggplot(subsetFilteredDataC, aes(x = TIMESTAMP, y = Value)) + geom_line() + geom_point() + labs(title = "Line Graph of C in First Hour", x = "Time", y = "sap flow") + theme_minimal()
subsetFilteredDataS <- filteredDataS[1:120, ]
ggplot(subsetFilteredDataS, aes(x = TIMESTAMP, y = Value)) + geom_line() + geom_point() + labs(title = "Line Graph of S in First Hour", x = "Time", y = "sap flow") + theme_minimal()
subsetFilteredDataF <- filtereddfF[1:120, ]
ggplot(subsetFilteredDataF, aes(x = TIMESTAMP, y = Value)) + geom_line() + geom_point() + labs(title = "Line Graph of F in First Hour", x = "Time", y = "sap flow") + theme_minimal()
             
BoundedFilteredDataC <- filteredDataC %>% filter(Value > 0 & Value <= 1)
ggplot(BoundedFilteredDataC, aes(x = TIMESTAMP, y = Value)) + geom_line() + geom_point() + labs(title = "Line Graph of C with all Values between 0 and 1", x = "Time", y = "sap flow") + theme_minimal()
BoundedFilteredDataS <- filteredDataS %>% filter(Value > 0 & Value <= 1)
ggplot(BoundedFilteredDataS, aes(x = TIMESTAMP, y = Value)) + geom_line() + geom_point() + labs(title = "Line Graph of S with all Values between 0 and 1", x = "Time", y = "sap flow") + theme_minimal()
BoundedFilteredDataF <- filtereddfF %>% filter(Value > 0 & Value <= 1)
ggplot(BoundedFilteredDataF, aes(x = TIMESTAMP, y = Value)) + geom_line() + geom_point() + labs(title = "Line Graph of F with all Values between 0 and 1", x = "Time", y = "sap flow") + theme_minimal()

subsetBoundedFilteredDataC <- BoundedFilteredDataC[1:96, ]
ggplot(subsetBoundedFilteredDataC, aes(x = TIMESTAMP, y = Value)) + geom_line() + geom_point() + labs(title = "Line Graph of C with all Values between 0 and 1 in first hour", x = "Time", y = "sap flow") + theme_minimal()
subsetBoundedFilteredDataS <- BoundedFilteredDataS[1:96, ]
ggplot(subsetBoundedFilteredDataS, aes(x = TIMESTAMP, y = Value)) + geom_line() + geom_point() + labs(title = "Line Graph of S with all Values between 0 and 1 in first hour", x = "Time", y = "sap flow") + theme_minimal()
subsetBoundedFilteredDataF <- BoundedFilteredDataF[1:96, ]
ggplot(subsetBoundedFilteredDataF, aes(x = TIMESTAMP, y = Value)) + geom_line() + geom_point() + labs(title = "Line Graph of F with all Values between 0 and 1 in first hour", x = "Time", y = "sap flow") + theme_minimal()

#Look at the values with Device 15
BoundedFilteredDevicesDataS <- BoundedFilteredDataS %>% filter(Sensor_ID == "S15")
ggplot(BoundedFilteredDevicesDataS, aes(x = TIMESTAMP, y = Value)) + geom_line() + geom_point() + labs(title = "Line Graph of S with all Values between 0 and 1 in third month on Device S15", x = "Time", y = "sap flow") + theme_minimal()

#subsetBoundedFilteredDeviceDataC <- subsetBoundedFilteredDataC %>% filter(Sensor_ID == "C15")
BoundedFilteredDevicesDataC <- BoundedFilteredDataC %>% filter(Sensor_ID == "C15")
ggplot(BoundedFilteredDevicesDataC, aes(x = TIMESTAMP, y = Value)) + geom_line() + geom_point() + labs(title = "Line Graph of C with all Values between 0 and 1 in third month on Device C15", x = "Time", y = "sap flow") + theme_minimal()
BoundedFilteredDevicesDataF <- BoundedFilteredDataF %>% filter(Sensor_ID == "F15")
ggplot(BoundedFilteredDevicesDataF, aes(x = TIMESTAMP, y = Value)) + geom_line() + geom_point() + labs(title = "Line Graph of F with all Values between 0 and 1 in third month on Device F15", x = "Time", y = "sap flow") + theme_minimal()

subsetBoundedFilteredDevicesDataF <- BoundedFilteredDevicesDataF[1:96, ]
ggplot(subsetBoundedFilteredDevicesDataF, aes(x = TIMESTAMP, y = Value)) + geom_line() + geom_point() + labs(title = "Line Graph of F with all Values between 0 and 1 in first hour on Device F15", x = "Time", y = "sap flow") + theme_minimal()
             
             
#mergedDataCSFGood <- merge(mergedDataCSGood, BoundedFilteredDevicesDataF, by = "Value")
#mergedDataCSFGoodFinal <- merge(mergedDataCSGood, BoundedFilteredDevicesDataF, by = "Sensor_ID")
#mergedDataCSFGoodFinal <- merge(mergedDataCSGood, BoundedFilteredDevicesDataF, by = "research_name")
             
plot(BoundedFilteredDevicesDataC$TIMESTAMP, BoundedFilteredDevicesDataC$Value, type="l", main="Line Graph of C, S, F in March", xlab="Time", ylab="Sapflow", ylim = range(c(0,1)))
lines(BoundedFilteredDevicesDataS$TIMESTAMP, BoundedFilteredDevicesDataS$Value, lty = 2, lwd = 2, col="green")
lines(BoundedFilteredDevicesDataF$TIMESTAMP, BoundedFilteredDevicesDataF$Value, lty=3, lwd = 2, col="blue")
#plot(BoundedFilteredDevicesDataC$TIMESTAMP, BoundedFilteredDevicesDataC$Value, type="l", main="Line Graph", xlab="Time", ylab="Sapflow", ylim =range(c(0,1)))
#plot(BoundedFilteredDevicesDataC$TIMESTAMP, BoundedFilteredDevicesDataS$Value, type="l", main="Line Graph", xlab="Time", ylab="Sapflow")
#plot(BoundedFilteredDevicesDataF$TIMESTAMP, BoundedFilteredDevicesDataF$Value, type="l", main="Line Graph", xlab="Time", ylab="Sapflow")
#ggplot(BoundedFilteredDevicesDataF, aes(x = TIMESTAMP, y = Value)) + geom_line() + geom_point() + labs(title = "Line Graph of F between 0 and 1 Device 15", x = "Time", y = "sap flow") + theme_minimal()

