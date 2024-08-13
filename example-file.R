# June TEMPEST data analysis
# Kathryn Jung, summer 2024

library(readr)
library(dplyr)
library(lubridate)
library(tidyr)

#### Read data and daytime means ####

# Find all the June 2022 TEMPEST and weather station files
june_files <- list.files("data/", pattern = "20220601", full.names = TRUE)
june_files <- june_files[grep("csv$", june_files)]

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
         daytime = time >= hms("06:00:00") & time <= hms("21:00:00")) %>% 
  filter(daytime) %>%
  # compute mean daytime values
  group_by(day, Sensor_ID, research_name) %>%
  summarize(value = mean(Value), .groups = "drop") %>% 
  filter(value > 0) -> 
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
  # ...and merge with the driver variable averages computed above
  left_join(dd_drivers, by = "day") ->
  dd_sf


# We need to know what trees correspond to which species;
# the sapflux-species-mapping file has this information
species <- read_csv("metadata/sapflux-species-mapping.csv")

dd_sf %>% 
  left_join(species, by = c("Sensor_ID" = "Sapflux_ID")) %>% 
  filter(!is.na(DBH_2021)) %>%
  # water supply to the canopy is through the "sapwood", which is a function
  # of diameter squared
  mutate(BA = DBH_2021 ^ 2) ->
  dd_sf

#### Predictive models ####

# Overall model
# We use stats::step() to performance stepwise removal of non-significant terms
mod <- step(
  lm(sapflow_2.5cm ~ (soil_vwc_15cm + 
                        wx_par_den15 * Species + 
                        wx_tempavg15 + 
                        wx_windspeed15) *
       (BA + Species), 
     data = dd_sf),
  direction = "both")
print(summary(mod))


# Use a variable number of trees to predict the others

#### Predictive models with Linear Regression ####

# Use a variable number of trees to predict the others
n_training <- 9
results <- list()
for(sp in unique(dd_sf$Species)) {
  dat_species <- dd_sf %>% filter(Species == sp)
  
  # Separate the species-specific data into training and validation 
  trees <- unique(dat_species$Sensor_ID)
  training_trees <- sample(trees, n_training, replace = FALSE)
  training_data <- dat_species %>% filter(Sensor_ID %in% training_trees)
  validation_data <- dat_species %>% filter(!Sensor_ID %in% training_trees)
  
  # Fit a linear model to the training data
  limod <- step(
    lm(sapflow_2.5cm ~ (soil_vwc_15cm + 
                          wx_par_den15 + 
                          wx_tempavg15 + 
                          wx_windspeed15) * 
         BA, 
       data = training_data), direction = "both")
  summary(limod)
  # Predict sapflow for the validation data
  validation_data$predicted_sapflow <- predict(limod, newdata = validation_data)
  
  # Performance metrics
  # Calculate Mean Absolute Error (MAE)
  mae <- mean(abs(validation_data$sapflow_2.5cm - validation_data$predicted_sapflow), na.rm = TRUE)
  
  # Calculate R-squared (R²)
  r_squared <- summary(lm(predicted_sapflow ~ sapflow_2.5cm, data = validation_data))$adj.r.squared
  
  # Plot the actual vs predicted sapflow
  p <- ggplot(validation_data, aes(x = day, group=Sensor_ID)) +
    geom_line(aes(y = sapflow_2.5cm, color = "Actual")) +
    geom_line(aes(y = predicted_sapflow, color = "Predicted")) +
    labs(title = paste("Actual vs Predicted Sapflow for Species:", sp),
         #labs(title = paste("Actual vs Predicted Sapflow"),
         x = "Day",
         y = "Sapflow (2.5cm)",
         color = "Legend") +
    theme_minimal()
  print(p)
  
  results[[sp]] <- tibble(
    Model_R2 = summary(limod)$adj.r.squared,
    Pred_mae = mae,
    Pred_R2 = r_squared,
    Model_terms = paste(names(limod$coefficients), collapse = ", ")
  )
}

# Bring all the results together into a single data frame
results <- bind_rows(results, .id = "Species")
message("All done:")
print(results)
