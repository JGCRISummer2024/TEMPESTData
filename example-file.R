library(dplyr)
library(lubridate)
library(tidyr)
library(readr)
library(keras)
library(tensorflow)
#### Read data and daytime means ####

# Find all the June 2022 TEMPEST and weather station files
june_files <- list.files("data/", pattern = "20220601", full.names = TRUE)
june_files <- june_files[grep("csv$", june_files)]
june_files
# ...and read them in
dat <- lapply(june_files, read_csv, col_types = "ccTccccdccii")

dat %>% 
  bind_rows() %>% 
  # drop any out-of-bounds observations
  replace_na(list(F_OOB = 0)) %>% 
  filter(F_OOB == 0) %>% 
  # isolate the variables we want
  filter(research_name %in% c("sapflow_2.5cm", 
                              "soil_vwc_15cm",
                              "wx_par_den15", 
                              "wx_tempavg15",
                              "wx_windspeed15")) %>% 
  # KJ code!
  # daytime is sunrise to sunset 6:00AM to 9:00PM
  mutate(day = date(TIMESTAMP), 
         time = hms::as_hms(TIMESTAMP), #extract day and time
         time = hms::as_hms(TIMESTAMP), # extract day and time
         daytime = time >= hms("06:00:00") & time <= hms("21:00:00")) %>% 
  filter(daytime) %>%
  # compute mean daytime values
  group_by(day, Sensor_ID, research_name) %>% #add the sensor ID, then we get the NA issue
  group_by(day, Sensor_ID, research_name) %>%
  summarize(value = mean(Value), .groups = "drop") -> 
  data_daily

# Diagnostic plot to ensure things look good
library(ggplot2)
p <- ggplot(data_daily, aes(day, value)) + geom_point() + 
  facet_wrap(~research_name, scales = "free")
print(p)

#### Reshaping data ####

# Compute daily averages for all the non-sapflow data
data_daily %>% 
  filter(research_name != "sapflow_2.5cm") %>% 
  group_by(day, research_name) %>% 
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>% 
  # pivot to wide form
  pivot_wider(names_from = "research_name") ->
  dd_drivers
# Isolate the sapflow data
data_daily %>% 
  filter(research_name == "sapflow_2.5cm") %>% 
  select(-research_name) %>% 
  rename(sapflow_2.5cm = value) %>% 
  # merge with the driver variables computed above
  # ...and merge with the driver variable averages computed above
  left_join(dd_drivers, by = "day") ->
  dd_sf

# We need to know what trees correspond to which species
# We need to know what trees correspond to which species;
# the sapflux-species-mapping file has this information
species <- read_csv("metadata/sapflux-species-mapping.csv")

dd_sf %>% 
  left_join(species, by = c("Sensor_ID" = "Sapflux_ID")) ->
  dd_sf

dd_sf
write.csv(dd_sf, file = "data_for_model.csv")
# WE ARE READY TO BUILD A PREDICTIVE MODEL
#### Predictive models ####
mod <- lm(sapflow_2.5cm ~ soil_vwc_15cm + 
            wx_par_den15 * Species + 
            wx_tempavg15 + 
            wx_windspeed15, data = dd_sf)
print(summary(mod))

n_training <- 9


# Use a variable number of trees to predict the others

#### Predictive models with Linear Regression ####

for(sp in unique(dd_sf$Species)) { #go through each species
  dat_species <- dd_sf %>% filter(Species == sp)
  #Separate the species-specific data into training and validation 
  trees <- unique(dat_species$Sensor_ID)
  
  training_trees <- sample(trees, n_training, replace = FALSE)
  training_data <- dd_sf %>% filter(Sensor_ID %in% training_trees)
  validation_data <- dd_sf %>% filter(!Sensor_ID %in% training_trees)
  
  
  # Fit a model to the training data
  limod <- lm(sapflow_2.5cm ~ soil_vwc_15cm + 
                wx_par_den15 + 
                wx_tempavg15 + 
                wx_windspeed15, data = training_data)
  summary(limod)
  # Predict sapflow for the validation data
  validation_data$predicted_sapflow <- predict(limod, newdata = validation_data)
  
  # Plot the actual vs predicted sapflow
  ggplot(validation_data, aes(x = day)) +
    geom_line(aes(y = sapflow_2.5cm, color = "Actual")) +
    geom_line(aes(y = predicted_sapflow, color = "Predicted")) +
    labs(title = paste("Actual vs Predicted Sapflow for Species:", sp),
    #labs(title = paste("Actual vs Predicted Sapflow"),
         x = "Day",
         y = "Sapflow (2.5cm)",
         color = "Legend") +
    theme_minimal()
  
}
#-------------------------------------------------------------------
#Get each species one by one

#ACRU
dat_species <- dd_sf %>% filter(Species == "ACRU")
#Separate the species-specific data into training and validation 
trees <- unique(dat_species$Sensor_ID)

training_trees <- sample(trees, n_training, replace = FALSE)
training_data <- dd_sf %>% filter(Sensor_ID %in% training_trees)
validation_data <- dd_sf %>% filter(!Sensor_ID %in% training_trees)


# Fit a model to the training data
limod <- lm(sapflow_2.5cm ~ soil_vwc_15cm + 
              wx_par_den15 + 
              wx_tempavg15 + 
              wx_windspeed15, data = training_data)
summary(limod)
# Predict sapflow for the validation data
validation_data$predicted_sapflow <- predict(limod, newdata = validation_data)

# Calculate Mean Absolute Error (MAE)
mae <- mean(abs(validation_data$sapflow_2.5cm - validation_data$predicted_sapflow))

# Calculate R-squared (R²)
ss_total <- sum((validation_data$sapflow_2.5cm - mean(validation_data$sapflow_2.5cm))^2)
ss_residual <- sum((validation_data$sapflow_2.5cm - validation_data$predicted_sapflow)^2)
r_squared <- 1 - (ss_residual / ss_total)

# Print the accuracy metrics
print("ACRU")
print(paste("Mean Absolute Error (MAE):", round(mae, 2)))
print(paste("R-squared (R²):", round(r_squared, 2)))

# Plot the actual vs predicted sapflow
ggplot(validation_data, aes(x = day)) +
  geom_line(aes(y = sapflow_2.5cm, color = "Actual")) +
  geom_line(aes(y = predicted_sapflow, color = "Predicted")) +
  labs(title = paste("Actual vs Predicted Sapflow for Species: ACRU"),
       #labs(title = paste("Actual vs Predicted Sapflow"),
       x = "Day",
       y = "Sapflow (2.5cm)",
       color = "Legend") + theme_minimal()



#FAGR
dat_species <- dd_sf %>% filter(Species == "FAGR")
#Separate the species-specific data into training and validation 
trees <- unique(dat_species$Sensor_ID)

training_trees <- sample(trees, n_training, replace = FALSE)
training_data <- dd_sf %>% filter(Sensor_ID %in% training_trees)
validation_data <- dd_sf %>% filter(!Sensor_ID %in% training_trees)


# Fit a model to the training data
limod <- lm(sapflow_2.5cm ~ soil_vwc_15cm + 
              wx_par_den15 + 
              wx_tempavg15 + 
              wx_windspeed15, data = training_data)
summary(limod)
# Predict sapflow for the validation data
validation_data$predicted_sapflow <- predict(limod, newdata = validation_data)

# Plot the actual vs predicted sapflow
ggplot(validation_data, aes(x = day)) +
  geom_line(aes(y = sapflow_2.5cm, color = "Actual")) +
  geom_line(aes(y = predicted_sapflow, color = "Predicted")) +
  labs(title = paste("Actual vs Predicted Sapflow for Species: FAGR"),
       #labs(title = paste("Actual vs Predicted Sapflow"),
       x = "Day",
       y = "Sapflow (2.5cm)",
       color = "Legend") + theme_minimal()


#LITU
dat_species <- dd_sf %>% filter(Species == "LITU")
#Separate the species-specific data into training and validation 
trees <- unique(dat_species$Sensor_ID)

training_trees <- sample(trees, n_training, replace = FALSE)
training_data <- dd_sf %>% filter(Sensor_ID %in% training_trees)
validation_data <- dd_sf %>% filter(!Sensor_ID %in% training_trees)


# Fit a model to the training data
limod <- lm(sapflow_2.5cm ~ soil_vwc_15cm + 
              wx_par_den15 + 
              wx_tempavg15 + 
              wx_windspeed15, data = training_data)
summary(limod)
# Predict sapflow for the validation data
validation_data$predicted_sapflow <- predict(limod, newdata = validation_data)

# Plot the actual vs predicted sapflow
ggplot(validation_data, aes(x = day)) +
  geom_line(aes(y = sapflow_2.5cm, color = "Actual")) +
  geom_line(aes(y = predicted_sapflow, color = "Predicted")) +
  labs(title = paste("Actual vs Predicted Sapflow for Species: LITU"),
       #labs(title = paste("Actual vs Predicted Sapflow"),
       x = "Day",
       y = "Sapflow (2.5cm)",
       color = "Legend") + theme_minimal()



#Time for an LSTM Model
