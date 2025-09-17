################################################################################
# This script finds the sum of temperature sign changes across the US in 2017
# using daily PRISM data.
################################################################################

library(terra)
library(tidyverse)

# Download data using GitHub function
# download_prism(data = c("tmin", "tmax"), start_date = as.Date("2017-01-01"),
#                end_date = as.Date("2017-12-31"), t_res = "daily",
#                out_dir = paste0(getwd(), "/PRISM_Data_2017"))

# Create indices for desired data
files <- list.files("Data_Raw/PRISM_Data_2017_Daily/", full.names = TRUE)
t_max_files <- files[stringr::str_detect(files, "tmax.*bil$")]
t_min_files <- files[stringr::str_detect(files, "tmin.*bil$")]

# Make sure this is selecting the correct files (should be 365 each)
length(t_max_files)
length(t_min_files)

# Check ordering of dates for t_max
t_max_dates <- stringr::str_extract(t_max_files, "[[:digit:]]{8}") |>
  lubridate::as_date()
if (!all(sort(t_max_dates) == t_max_dates)) {
  t_max_files <- t_max_files[order(t_max_dates)]
}

# Check ordering of dates for t_min
t_min_dates <- stringr::str_extract(t_min_files, "[[:digit:]]{8}") |>
  lubridate::as_date()
if (!all(sort(t_min_dates) == t_min_dates)) {
  t_min_files <- t_min_files[order(t_min_dates)]
}

# Create layers of raster images
t_max_rast <- terra::rast(t_max_files)
t_min_rast <- terra::rast(t_min_files)

# Count number of sign changes (ChatGPT helped me realize that it was a lot
# simpler than I thought it would be)
sign_changes <- t_max_rast > 0 & t_min_rast < 0
freezing_change_sum <- terra::app(sign_changes, sum)

# Plot
plot(sign_changes)
plot(freezing_change_sum)
