library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(terra)
library(foreach)
library(doParallel)
library(gridExtra)

# Get LOCA files
loca_files <- list.files("F:/Data/LOCA2/sugar_sap_days", full.names = TRUE)
loca_files <- loca_files[stringr::str_detect(loca_files, "prop")]
loca_files_short <- list.files("F:/Data/LOCA2/sugar_sap_days", full.names = FALSE)
loca_files_short <- loca_files_short[stringr::str_detect(loca_files_short, "prop")]

# Define function for use in parallelization
loca_vs_prism <- function(loca_file) {
  # Load in PRISM sap day proportion raster
  prism_rast <- terra::rast("F:/Data/PRISM_sugar_prop.tif")

  # Define US
  us <- ne_countries(scale = "medium", returnclass = "sf", country = "United States of America")
  us_vect <- vect(us)

  # Get PRISM
  us_prism <- project(us_vect, crs(prism_rast))
  prism_us <- mask(crop(prism_rast, us_prism), us_prism)

  # Create short version of file name
  pattern <- "F:/Data/LOCA2/sugar_sap_days/"
  loca_file_short <- stringr::str_remove(loca_file, pattern)

  # Load in raster
  loca_rast <- terra::rast(loca_file) |>
    terra::shift(dx = -360)

  # Subset LOCA2 raster by PRISM years
  loca_rast <- loca_rast[[which(names(loca_rast) %in% names(prism_rast))]]

  # Mask rasters to be just the CONUS
  us_loca <- project(us_vect, crs(loca_rast))
  loca_us <- mask(crop(loca_rast, us_loca), us_loca)
  loca_us <- crop(loca_us, ext(prism_us))

  # Convert PRISM 4km resolution to 6km resolution to match LOCA
  prism_us <- resample(prism_us, loca_us)
  prism_vals <- terra::values(prism_us)
  prism_vars <- app(prism_us, var)

  ### KS Distance
  # Extract raster values
  loca_vals <- terra::values(loca_us)
  dists <- rep(NA, nrow(prism_vals))
  p_vals <- rep(NA, nrow(prism_vals))

  # For each pixel...
  for (j in 1:nrow(prism_vals)) {
    # Conduct Kolmogorov-Smirnov test
    if (all(!is.na(prism_vals[j, ])) && all(!is.na(loca_vals[j, ]))) {
      ks_results <- ks.test(prism_vals[j, ], loca_vals[j, ])
      dists[j] <- ks_results$statistic
      p_vals[j] <- ks_results$p.value
    }
  }

  ### Variance
  # Calculate variance of each pixel
  loca_vars <- app(loca_us, var)

  # Return TRUE if LOCA overestimates PRISM variance
  var_comps <- loca_vars > prism_vars

  # Extract stuff from file name
  model <- stringr::str_extract(loca_file_short, "^[^_]+_") |>
    stringr::str_remove("_")
  scenario <- stringr::str_extract(loca_file_short, "ssp\\d{3}")
  run <- stringr::str_extract(loca_file_short, "run\\d{1}") |>
    stringr::str_remove("run")

  # Return output list
  list(
    model = model,
    scenario = scenario,
    run = run,
    mean_distance = mean(dists, na.rm = TRUE),
    mean_p_value = mean(p_vals, na.rm = TRUE),
    prop_overest_variance = mean(terra::values(var_comps), na.rm = TRUE)
  )
}

# Run function with parallelization
cl <- makeCluster(6)
registerDoParallel(cl)
models_comparison_list <- foreach(
  lf = loca_files,
  .packages = c(
    "terra",
    "sf",
    "rnaturalearth",
    "rnaturalearthdata",
    "tidyverse"
  )
) %dopar% loca_vs_prism(lf)
stopCluster(cl)




# Create data frame
models_comparison <- dplyr::bind_rows(models_comparison_list)
write_csv(models_comparison, "Data_Clean/models_comparison.csv")

# Read in models comparison
models_comparison <- read_csv("Data_Clean/models_comparison.csv")

# Find out which models are better
comp <- models_comparison |>
  group_by(model) |>
  summarize(prop_overest_variance_mean = mean(prop_overest_variance),
            mean_p_value_mean = mean(mean_p_value),
            mean_distance_mean = mean(mean_distance)) |>
  arrange(mean_distance_mean) |>
  select(model, mean_distance_mean, mean_p_value_mean, prop_overest_variance_mean)

# Make plots
g1 <- ggplot(models_comparison, aes(mean_distance)) +
  geom_histogram(breaks = seq(0.3, 0.55, by = 0.025),
                 color = "black") +
  theme_bw() +
  xlab("Mean Kolmogorov-Smirnov Distance") +
  scale_y_continuous("Count", breaks = seq(0, 50, by = 10),
                     limits = c(0, 50)) +
  theme(text = element_text(size = 14))

g2 <- ggplot(models_comparison, aes(mean_p_value)) +
  geom_histogram(breaks = seq(0.05, 0.3, by = 0.025),
                 color = "black") +
  theme_bw() +
  scale_x_continuous("Mean P-value", breaks = seq(0.05, 0.3, by = 0.05)) +
  scale_y_continuous("Count", breaks = seq(0, 50, by = 10), limits = c(0, 50)) +
  theme(text = element_text(size = 14))

g3 <- ggplot(models_comparison, aes(prop_overest_variance)) +
  geom_histogram(breaks = seq(0.3, 0.75, by = 0.05),
                 color = "black") +
  theme_bw() +
  scale_x_continuous("Mean Proportion of Pixels Where Model Overestimates PRISM Variance",
                     breaks = seq(0.3, 0.75, by = 0.05)) +
  scale_y_continuous("Count", breaks = seq(0, 40, by = 10),
                     limits = c(0, 40)) +
  theme(text = element_text(size = 14))


jpeg("figures/models_comparison_hists.jpg", width = 7, height = 6, units = "in", res = 600)
grid.arrange(g1, g2, g3, nrow = 3)
dev.off()

jpeg("figures/models_comparison_best_models.jpg", width = 7, height = 5, units = "in", res = 600)
models_comparison |>
  group_by(model) |>
  summarize(mean_distance_mean = mean(mean_distance)) |>
  arrange(mean_distance_mean) |>
  head(10) |>
  mutate(model = as.factor(model)) |>
  ggplot(aes(reorder(model, mean_distance_mean), mean_distance_mean)) +
  geom_bar(stat = "identity") +
  xlab("LOCA2 Model") +
  scale_y_continuous("Mean Kolmogorov-Smirnov Distance",
                     breaks = seq(0, 0.4, by = 0.1),
                     limits = c(0, 0.4)) +
  theme_bw() +
  theme(text = element_text(size = 14),
        axis.text.x = element_text(angle = 45, vjust = 0.55))
dev.off()

models_comparison |>
  group_by(model) |>
  summarize(mean_distance_mean = mean(mean_distance)) |>
  arrange(-mean_distance_mean) |>
  head(10) |>
  mutate(model = as.factor(model)) |>
  ggplot(aes(reorder(model, -mean_distance_mean), mean_distance_mean)) +
  geom_bar(stat = "identity") +
  xlab("LOCA2 Model") +
  ylab("Mean Kolmogorov-Smirnov Distance") +
  theme_bw() +
  theme(text = element_text(size = 16),
        axis.text.x = element_text(angle = 45, vjust = 0.55))




ggplot(comp, aes(x = mean_distance_mean, y = prop_overest_variance_mean)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw()

summary(lm(prop_overest_variance_mean ~ mean_distance_mean, data = comp))
