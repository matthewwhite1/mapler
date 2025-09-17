################################################################################
# This script recreates Table 2 from the research proposal, but this time
# coarses the grid first with terra::aggregate().
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

# Only consider years after 1991 to fit with Table 2
year_folders <- year_folders[years >= 1991]
years <- years[years >= 1991]

# Get US states
us_states <- st_read("Data_Clean/US_State_Lines/cb_2018_us_state_500k.shp")
us_states_vect <- vect(us_states)
table_states <- c("Utah", "Idaho", "Montana", "South Dakota", "Iowa",
                  "Minnesota", "New York", "Kentucky", "Vermont", "Virginia")

# ChatGPT helped create this function
compute_monthly_avg <- function(raster_files, states_vect) {
  raster_stack <- rast(raster_files) |>
    aggregate()
  monthly_avg_raster <- mean(raster_stack, na.rm = TRUE)
  state_avg <- terra::extract(monthly_avg_raster, states_vect, fun = mean, na.rm = TRUE)
  state_avg <- cbind(us_states, state_avg)
  return(state_avg)
}

# Find means for each year
tmax_means_df <- data.frame(NAME = table_states)
tmin_means_df <- data.frame(NAME = table_states)
for (i in seq_along(year_folders)) {
  files <- list.files(year_folders[i], full.names = TRUE)
  for (j in 1:4) {
    tmax_files <- files[str_detect(files, paste0("tmax.*", years[i], "0", j, ".*.bil$"))]
    mean_name <- paste0("mean", j)
    tmax_state_avg <- compute_monthly_avg(tmax_files, us_states_vect) |>
      filter(NAME %in% table_states) |>
      as.data.frame() |>
      select(NAME, mean) |>
      rename({{mean_name}} := mean)
    tmax_means_df <- full_join(tmax_means_df, tmax_state_avg, by = "NAME")

    tmin_files <- files[str_detect(files, paste0("tmin.*", years[i], "0", j, ".*.bil$"))]
    tmin_state_avg <- compute_monthly_avg(tmin_files, us_states_vect) |>
      filter(NAME %in% table_states) |>
      as.data.frame() |>
      select(NAME, mean) |>
      rename({{mean_name}} := mean)
    tmin_means_df <- full_join(tmin_means_df, tmin_state_avg, by = "NAME")
  }
}


# Create final table
tmax_means_df_final <- tmax_means_df %>%
  mutate(Jan_tmax = rowMeans(select(., seq(2, length(tmax_means_df) - 3, by = 4))),
         Feb_tmax = rowMeans(select(., seq(3, length(tmax_means_df) - 2, by = 4))),
         Mar_tmax = rowMeans(select(., seq(4, length(tmax_means_df) - 1, by = 4))),
         Apr_tmax = rowMeans(select(., seq(5, length(tmax_means_df), by = 4)))) |>
  select(NAME, Jan_tmax, Feb_tmax, Mar_tmax, Apr_tmax)
tmin_means_df_final <- tmin_means_df %>%
  mutate(Jan_tmin = rowMeans(select(., seq(2, length(tmin_means_df) - 3, by = 4))),
         Feb_tmin = rowMeans(select(., seq(3, length(tmin_means_df) - 2, by = 4))),
         Mar_tmin = rowMeans(select(., seq(4, length(tmin_means_df) - 1, by = 4))),
         Apr_tmin = rowMeans(select(., seq(5, length(tmin_means_df), by = 4)))) |>
  select(NAME, Jan_tmin, Feb_tmin, Mar_tmin, Apr_tmin)
table_2 <- full_join(tmin_means_df_final, tmax_means_df_final, by = "NAME")

# Export table
write_csv(table_2, "Data_Clean/Table_2_coarse")

# Import table
table_2_coarse <- read_csv("Data_Clean/Table_2_coarse")
