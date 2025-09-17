################################################################################
# This script finds the proportion of days that are ideal for maple syrup
# tapping across the US using daily PRISM data from January to April, 1981 to
# 2020.
################################################################################

library(terra)
library(tidyverse)
library(sf)

# Check ordering of year folders
year_folders <- list.files("Data_Raw/PRISM_Sap_Seasons_Data/", full.names = TRUE)
years <- as.integer(str_extract(year_folders, "[[:digit:]]{4}"))
if (!all(sort(years) == years)) {
  year_folders <- year_folders[order(years)]
}

# Within each folder:
# - Check ordering of dates
# - Load in two rasters
# - Make logical statement with desired temps
# - Use app() with mean() to find proportion
# - Load that into ith entry of the list
sap_prop_list <- list()
for (i in seq_along(years)) {
  # Read in files
  files <- list.files(year_folders[i], full.names = TRUE)
  t_max_files <- files[str_detect(files, "tmax.*bil$")]
  t_min_files <- files[str_detect(files, "tmin.*bil$")]

  # Check ordering of dates
  t_max_dates <- str_extract(t_max_files, "[[:digit:]]{8}") |>
    as_date()
  if (!all(sort(t_max_dates) == t_max_dates)) {
    t_max_files <- t_max_files[order(t_max_dates)]
  }
  t_min_dates <- str_extract(t_min_files, "[[:digit:]]{8}") |>
    as_date()
  if (!all(sort(t_min_dates) == t_min_dates)) {
    t_min_files <- t_min_files[order(t_min_dates)]
  }

  # Load in rasters
  t_max_rast <- rast(t_max_files)
  t_min_rast <- rast(t_min_files)

  # Logical statement with desired temps
  sap_day <- t_max_rast > 2.2 & t_min_rast < -1.1

  # Use app()
  sap_prop <- app(sap_day, mean)

  # Load into list
  sap_prop_list[i] <- sap_prop
}

# Combine all rasters into one raster with 30 layers
sap_day_prop <- rast(sap_prop_list)

plot(sap_day_prop)

# Write it to a file for my laptop
writeRaster(sap_day_prop, "Data_Clean/sap_day_prop.tif", overwrite = TRUE)

# Read in file now that I'm on my laptop
sap_day_prop <- rast("Data_Clean/sap_day_prop.tif")

# Plot first year (1981)
us_states <- read_sf("Data_Clean/US_State_Lines/cb_2018_us_state_500k.shp")
plot(sap_day_prop[[1]])
title(main = "Proportion of Ideal Maple Syrup Tapping Days, Jan-Apr 1981")
lines(us_states)
