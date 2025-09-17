################################################################################
# This script attempts to perform some regression of the mean proportion of
# ideal sap tapping days vs the total maple syrup production in gallons
# using example maple syrup data.
################################################################################

library(terra)
library(tidyverse)
library(sf)

# Read in file
sap_day_prop <- rast("Data_Clean/sap_day_prop.tif")

# Get US states
us_states <- read_sf("Data_Clean/US_State_Lines/cb_2018_us_state_500k.shp")
us_states_vect <- vect(us_states)

# Extract mean proportions by state, combine with us_states table
mean_prop_by_state <- terra::extract(sap_day_prop, us_states_vect, fun = mean, na.rm = TRUE)
mean_props <- cbind(us_states, mean_prop_by_state) |>
  filter(NAME %in% c("New Hampshire", "New York", "Vermont")) |>
  select(NAME, 11:40) |>
  rename_with(~ as.character(1991:2020), 2:31) |>
  rename_with(~ "State", NAME) |>
  pivot_longer(cols = 2:31, names_to = "Year", values_to = "mean_sap_day_prop") |>
  mutate(Year = as.integer(Year))

# Read in example maple syrup data
example_maple <- read_csv("Data_Clean/example_maple_syrup_data.csv") |>
  select(-c(1, 3, 5, 6)) |>
  mutate(State = str_to_title(State))

# Join tables together
maple_props <- inner_join(mean_props, example_maple, by = c("State", "Year"))

# Make some plots
options(scipen = 10000)
ggplot(maple_props, aes(mean_sap_day_prop, ProductionInGallons, color = State)) +
  geom_point() +
  geom_smooth()
ggplot(maple_props, aes(Year, mean_sap_day_prop, color = State)) +
  geom_line()
ggplot(maple_props, aes(Year, ProductionInGallons, color = State)) +
  geom_line()
ggplot(maple_props, aes(ProductionInGallons)) +
  geom_histogram() # Definitely not normal
ggplot(maple_props, aes(mean_sap_day_prop)) +
  geom_histogram() # Looks normal

# Add log term
maple_props <- maple_props |>
  mutate(ProductionInGallons_log = log(ProductionInGallons))

# Regression
model <- lm(ProductionInGallons_log ~ mean_sap_day_prop, maple_props)
summary(model)
