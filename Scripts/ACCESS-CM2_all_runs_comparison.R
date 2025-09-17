library(terra)
library(tidyverse)

model_runs <- vector("list", 3)
k_upper <- 2.2 + 273.15
k_lower <- -1.1 + 273.15
for (i in 1:3) {
  path <- "D:/Data/LOCA2/ACCESS-CM2/0p0625deg/r"
  loca_rast <- loca_t_rast(paste0(path, i, "i1p1f1"))
  model_runs[[i]] <- sap_day(loca_rast$tmax, loca_rast$tmin, k_upper, k_lower)
}

# Export rasters
for (i in 1:3) {
  propname <- paste0("ACCESS-CM2_run_", i, "_proportion.tif")
  terra::writeRaster(model_runs[[i]]$proportion, propname)
  sumname <- paste0("ACCESS-CM2_run_", i, "_sum.tif")
  terra::writeRaster(model_runs[[i]]$sum, sumname)
}

# Import rasters
model_runs <- vector("list", 3)
for (i in 1:3) {
  propname <- paste0("D:/Data/ACCESS-CM2_run_", i, "_proportion.tif")
  sumname <- paste0("D:/Data/ACCESS-CM2_run_", i, "_sum.tif")
  model_runs[[i]] <- list(proportion = terra::rast(propname), sum = terra::rast(sumname))
}

# Extract years for subsetting
years <- as.integer(terra::names(model_runs[[1]]$proportion))

# Initialize empty lists
prop_mean_list <- vector("list", length(years))
prop_var_list <- vector("list", length(years))

# For each year
for (i in seq_along(years)) {
  # Combine into one raster
  combined <- c(model_runs[[1]]$proportion[[i]],
                model_runs[[2]]$proportion[[i]],
                model_runs[[3]]$proportion[[i]])

  # Calculate mean and variance
  prop_mean_list[[i]] <- terra::app(combined, mean)
  prop_var_list[[i]] <- terra::app(combined, var)

  # Print progress
  print(paste0("Completed calculation for year ", years[i]))
}

# Create raster stacks
prop_mean <- terra::rast(prop_mean_list)
names(prop_mean) <- years
prop_var <- terra::rast(prop_var_list)
names(prop_var) <- years

### Variance over time
# Initialize empty vectors
yearly_var <- rep(0, length(years))
yearly_var_upper <- rep(0, length(years))
yearly_var_lower <- rep(0, length(years))

# For each year
for (i in seq_along(years)) {
  # Extract only non-NA values
  var_vals <- terra::values(prop_var[[i]])[!is.na(terra::values(prop_var[[i]]))]

  # Calculate mean and confidence interval
  mean_var <- mean(var_vals)
  yearly_var[i] <- mean_var
  se_var <- sd(var_vals) / sqrt(length(var_vals))
  yearly_var_upper[i] <- mean_var + 1.96 * se_var
  yearly_var_lower[i] <- mean_var - 1.96 * se_var
}

# Create data frame
yearly_var_df <- data.frame(Year = years,
                            Variance = yearly_var,
                            Upper = yearly_var_upper,
                            Lower = yearly_var_lower,
                            Period = c(rep("Historical", 65), rep("Future", 86)))

# Plot
ggplot(yearly_var_df, aes(Year, Variance, color = Period)) +
  geom_line() +
  theme_bw() +
  geom_ribbon(aes(ymin = Lower, ymax = Upper, fill = Period), alpha = 0.2, color = NA) +
  scale_x_continuous(breaks = seq(1950, 2100, by = 25)) +
  scale_y_continuous(breaks = seq(0.0003, 0.0015, by = 0.0003), limits = c(0.0003, 0.0015)) +
  scale_color_manual(values = c("blue", "red"), breaks = c("Historical", "Future")) +
  scale_fill_manual(values = c("blue", "red"), breaks = c("Historical", "Future")) +
  ggtitle("Yearly Variance Over Time Between ACCESS-CM2 Runs") +
  theme(plot.title = element_text(hjust = 0.5))
