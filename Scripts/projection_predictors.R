library(tidyverse)
library(terra)

# Load in rasters
rasty <- loca_t_rast("F:/Data/LOCA2/ACCESS-CM2/0p0625deg/r1i1p1f1/")

# Extract tmin and tmax
tmax_rast <- rasty$tmax
tmin_rast <- rasty$tmin

# Extract months and years
dates <- terra::time(tmax_rast)
months <- month(dates)

# Subset by April
april_tmax <- tmax_rast[[which(months == 4)]]
april_tmin <- tmin_rast[[which(months == 4)]]

# Compute mean annual April temperature
april_mean_temp <- mean_temp_projection(april_tmax, april_tmin)

# Compute trend for temperatures
april_historical <- april_mean_temp[[which(terra::names(april_mean_temp) %in% 1950:2013)]]
april_future <- april_mean_temp[[which(terra::names(april_mean_temp) %in% 2014:2100)]]
april_historical_trend <- sens_slope_rast(april_historical)
april_future_trend <- sens_slope_rast(april_future)
