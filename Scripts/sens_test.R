library(tidyverse)
library(terra)
library(sf)
library(Metrics)

### Test speed on maple farms ###
# Load in raster stack
sap_day_access <- terra::rast("F:/Data/LOCA2/sugar_sap_days/ACCESS-CM2_run1_ssp585_prop.tif") |>
  terra::shift(dx = -360)

# Read in farm coordinates
farms_sf <- sf::st_as_sf(farms_coords, coords = c("lon", "lat"), crs = 4326)

# Extract sap day proportions at farm locations
farms_sap_access <- terra::extract(sap_day_access, farms_sf) |>
  tidyr::drop_na()

# Define empty vectors
n_farms <- nrow(farms_sap_access)
years <- ncol(farms_sap_access)
trend_median <- rep(NA, n_farms)
mapler_median <- rep(NA, n_farms)
trend_mem_alloc <- rep(NA, n_farms)
mapler_mem_alloc <- rep(NA, n_farms)
trend_estimates <- rep(NA, n_farms)
mapler_estimates <- rep(NA, n_farms)
trend_p_values <- rep(NA, n_farms)
mapler_p_values <- rep(NA, n_farms)

# For every farm...
for (i in seq_len(n_farms)) {
  # Get time series of sap day proportion
  sap_prop <- as.numeric(farms_sap_access[i, 2:years])

  # Run benchmark
  benchmark <- bench::mark(trend::sens.slope(sap_prop),
                           sens_slope(sap_prop),
                           check = FALSE)

  # Save results
  trend_median[i] <- benchmark$median[1]
  mapler_median[i] <- benchmark$median[2]
  trend_mem_alloc[i] <- benchmark$mem_alloc[1]
  mapler_mem_alloc[i] <- benchmark$mem_alloc[2]

  # Calculate trend again to get estimates and p-values
  trend_sens <- trend::sens.slope(sap_prop)
  trend_estimates[i] <- trend_sens$estimates
  trend_p_values[i] <- trend_sens$p.value

  # Calculate mapler again to get estimates and p-values
  mapler_sens <- sens_slope(sap_prop)
  mapler_estimates[i] <- mapler_sens$estimates
  mapler_p_values[i] <- mapler_sens$p.value

  # Print progress
  print(i)
}

# Get median in ms
trend_median_ms <- trend_median * 1000
mapler_median_ms <- mapler_median * 1000

# Get memory allocation in MB
trend_mem_alloc_mb <- trend_mem_alloc / 1000000
mapler_mem_alloc_mb <- mapler_mem_alloc / 1000000

# Create dataframe
sens_test <- data.frame(func = c(rep("trend::sens.slope()", n_farms),
                                 rep("mapler::sens_slope()", n_farms)),
                        median_time_ms = c(trend_median_ms, mapler_median_ms),
                        mem_alloc_mb = c(trend_mem_alloc_mb, mapler_mem_alloc_mb),
                        estimate = c(trend_estimates, mapler_estimates),
                        p_value = c(trend_p_values, mapler_p_values))

# Calculate averages
sens_test_summary <- sens_test |>
  group_by(func) |>
  summarize(mean_median_time_ms = mean(median_time_ms),
            var_median_time_ms = var(median_time_ms),
            mean_mem_alloc_mb = mean(mem_alloc_mb),
            var_mem_alloc_mb = var(mem_alloc_mb),
            mean_estimate = mean(estimate),
            var_estimate = var(estimate),
            mean_p_value = mean(p_value),
            var_p_value = var(p_value)) |>
  arrange(-mean_median_time_ms)

# How close is trend to mapler?
cor(trend_estimates, mapler_estimates)
cor(trend_p_values, mapler_p_values)

Metrics::mae(trend_estimates, mapler_estimates)
Metrics::mae(trend_p_values, mapler_p_values)

mean(mapler_p_values > 0.05)


### Test speed on raster data ###
# Read in aggregated raster
test_loca_file <- system.file("extdata", "test_loca_sap_day.tif",
                              package = "mapler")
sap_prop <- terra::rast(test_loca_file)

# Make trend helper functions
sens.slope_rast_helper <- function(t_pixel) {
  if (all(is.na(t_pixel))) {
    return(rep(NA, 5))
  }
  t_pixel_no_na <- t_pixel[!is.na(t_pixel)]
  sens_slope <- trend::sens.slope(t_pixel_no_na)
  conf.low <- sens_slope$conf.int[1]
  conf.high <- sens_slope$conf.int[2]
  as.numeric(c(sens_slope[c("estimates", "statistic", "p.value")], conf.low, conf.high))
}
sens.slope_rast <- function(t_rast) {
  t_rast_slope <- terra::app(t_rast, sens.slope_rast_helper)
  names(t_rast_slope) <- c("estimates", "statistic", "p.value",
                           "conf.low", "conf.high")
  t_rast_slope
}

# Benchmark
bench::mark(sens.slope_rast(sap_prop), sens_slope_rast(sap_prop), check = FALSE)
