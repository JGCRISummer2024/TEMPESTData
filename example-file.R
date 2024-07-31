# June TEMPEST data analysis
# Kathryn Jung, summer 2024

library(readr)
library(dplyr)
library(lubridate)

june_files <- list.files("data/", pattern = "20220601", full.names = TRUE)
june_files <- june_files[grep("csv$", june_files)]

dat <- lapply(june_files, read_csv, col_types = "ccTccccdccii")
dat %>% 
  bind_rows() %>% 
  # drop any out-of-bounds observations
  replace_na(list(F_OOB = 0)) %>% 
  filter(F_OOB == 0) %>% 
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
  group_by(day, Sensor_ID, research_name) %>% #add the sensor ID, then we get the NA issue
  summarize(value = mean(Value), .groups = "drop") -> 
  data_daily

library(ggplot2)
p <- ggplot(data_daily, aes(day, value)) + geom_point() + 
  facet_wrap(~research_name, scales = "free")
print(p)

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
  left_join(dd_drivers, by = "day") ->
  dd_sf

# We need to know what trees correspond to which species
species <- read_csv("metadata/sapflux-species-mapping.csv")

dd_sf %>% 
  left_join(species, by = c("Sensor_ID" = "Sapflux_ID")) ->
  dd_sf

# WE ARE READY TO BUILD A PREDICTIVE MODEL

mod <- lm(sapflow_2.5cm ~ soil_vwc_15cm + 
            wx_par_den15 * Species + 
            wx_tempavg15 + 
            wx_windspeed15, data = dd_sf)
print(summary(mod))

# Use a variable number of trees to predict the others
n_training <- 9

for(sp in unique(dd_sf$Species)) {
  dat_species <- dd_sf %>% filter(Species == sp)
  
  # Separate the species-specific data into training and validation 
  trees <- unique(dat_species$Sensor_ID)
  training_trees <- sample(trees, n_training, replace = FALSE)
  training_data <- dd_sf %>% filter(Sensor_ID %in% training_trees)
  validation_data <- dd_sf %>% filter(!Sensor_ID %in% training_trees)
  
  # Fit a model to the training data
  # Predict sapflow for the validation data
  # Plot!
}

message("All done!")
