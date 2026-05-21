library(tidyverse)
library(sf)
library(terra)
library(rnaturalearth)
library(rnaturalearthdata)
library(RColorBrewer)

### Basic plotting ###
# Read in data
western_farms_coords <- read_csv("Data_Clean/western_farms_coords.csv")
western_farms_sf <- sf::st_as_sf(western_farms_coords,
                                 coords = c("longitude", "latitude"), crs = 4326)

# Get North America map
crop_lims <- c(xmin = -118, ymin = 32, xmax = -94, ymax = 50)
world <- ne_countries(scale = "medium", returnclass = "sf")
north_america <- world |>
  filter(region_un == "Americas", name %in% c("United States of America", "Canada")) |>
  st_crop(crop_lims)
us_states <- ne_states(country = "United States of America", returnclass = "sf") |>
  st_crop(crop_lims)

# Plot farm locations
ggplot() +
  geom_sf(data = north_america, fill = "grey95", color = "black", size = 0.2) +
  geom_sf(data = us_states, fill = NA, color = "darkgray", size = 0.3) +
  geom_sf(data = canada_provinces, fill = NA, color = "darkgray", size = 0.3) +
  geom_sf(data = western_farms_sf, mapping = aes(color = species), size = 1, alpha = 1) +
  scale_color_brewer(palette = "Dark2") +
  scale_y_continuous(limits = c(34, 48.3)) +
  theme_minimal() +
  xlab("Longitude") +
  ylab("Latitude") +
  theme(text = element_text(size = 16))
ggsave("figures/western_farms_locations.pdf", width = 7, height = 9)


### Just one LOCA2 model ###
# Get raster stack
sap_day_access <- terra::rast("F:/Data/LOCA2/sugar_sap_days/ACCESS-CM2_run1_ssp585_prop.tif") |>
  terra::shift(dx = -360)

# Get Sen's slope for each farm
western_farms_sens <- get_sens_farms(western_farms_sf, sap_day_access)

# Plot
ggplot() +
  geom_sf(data = north_america, fill = "grey95", color = "black", size = 0.2) +
  geom_sf(data = us_states, fill = NA, color = "darkgray", size = 0.3) +
  geom_sf(data = canada_provinces, fill = NA, color = "darkgray", size = 0.3) +
  geom_sf(data = western_farms_sens, mapping = aes(color = sens_estimate), size = 2) +
  scale_color_viridis_c() +
  scale_y_continuous(limits = c(34, 48.3)) +
  theme_minimal() +
  xlab("Longitude") +
  ylab("Latitude") +
  theme(text = element_text(size = 16))


### All LOCA2 models ###
# Define scenarios and threshold types
scenarios <- c("ssp245", "ssp370", "ssp585")
threshes <- c("sugar", "boxelder", "norway")
df_list <- list()

for (l in 1:3) {
  # Get file paths
  sap_rasts <- list.files(paste0("F:/Data/LOCA2/", threshes[l], "_sap_days/"), full.names = TRUE)
  sap_rasts <- sap_rasts[stringr::str_detect(sap_rasts, "prop")]

  # Define empty significants list
  new_significants <- list()
  new_estimates <- list()

  # For each scenario...
  for (i in seq_along(scenarios)) {
    # Define empty significants vector
    significants <- c()
    estimates <- c()

    # Subset rasters by scenario
    scenario_rasts <- sap_rasts[stringr::str_detect(sap_rasts, scenarios[i])]

    # For each raster...
    for (j in seq_along(scenario_rasts)) {
      # Load in raster
      sap_rast <- terra::rast(scenario_rasts[j]) |>
        terra::shift(dx = -360)

      # Get sens significant values at each farm
      sap_farms <- get_sens_farms(western_farms_sf, sap_rast)

      # Append to significants vector
      significants <- c(significants, sap_farms$sens_significant)
      estimates <- c(estimates, sap_farms$sens_estimate)

      # Print progress
      print(j)
    }

    # Calculate new significant value as mean of significants for each farm
    my_len <- nrow(sap_farms)
    new_significant <- rep(0, my_len)
    new_estimate <- rep(0, my_len)
    for (k in seq_len(my_len)) {
      new_significant[k] <- mean(significants[seq(k, (length(scenario_rasts) * my_len), by = my_len)])
      new_estimate[k] <- mean(estimates[seq(k, (length(scenario_rasts) * my_len), by = my_len)])
    }

    # Append to significants list
    new_significants[[i]] <- new_significant
    new_estimates[[i]] <- new_estimate
  }

  # Make joined scenario dataframes
  ssp245 <- western_farms_sf |>
    dplyr::mutate(sens_significant = new_significants[[1]],
                  sens_estimate = new_estimates[[1]]) |>
    dplyr::mutate(threshold = threshes[l],
                  scenario = "ssp245")
  ssp370 <- western_farms_sf |>
    dplyr::mutate(sens_significant = new_significants[[2]],
                  sens_estimate = new_estimates[[2]]) |>
    dplyr::mutate(threshold = threshes[l],
                  scenario = "ssp370")
  ssp585 <- western_farms_sf |>
    dplyr::mutate(sens_significant = new_significants[[3]],
                  sens_estimate = new_estimates[[3]]) |>
    dplyr::mutate(threshold = threshes[l],
                  scenario = "ssp585")
  df_list[[l]] <- rbind(ssp245, ssp370, ssp585)
}
final_df <- rbind(df_list[[1]], df_list[[2]], df_list[[3]])

# Create discrete breaks
significant_cuts <- seq(-1, -0.2, by = 0.2)
final_df <- final_df |>
  mutate(significant_bins = cut(sens_significant, significant_cuts, include.lowest = TRUE)) |>
  mutate(threshold = factor(threshold, levels = c("sugar", "boxelder", "norway"))) |>
  mutate(threshold = recode(threshold,
                            sugar = "Sugar",
                            boxelder = "Boxelder",
                            norway = "Norway"))

# Plot
ggplot() +
  geom_sf(data = north_america, fill = "grey95", color = "black", size = 0.2) +
  geom_sf(data = us_states, fill = NA, color = "darkgray", size = 0.3) +
  geom_sf(data = canada_provinces, fill = NA, color = "darkgray", size = 0.3) +
  geom_sf(data = final_df, mapping = aes(color = significant_bins), size = 1) +
  facet_grid(scenario ~ threshold) +
  scale_color_manual("Significance Average", values = brewer.pal(8, "PuOr")[8:5]) +
  scale_y_continuous(limits = c(34, 48.3)) +
  theme_minimal() +
  xlab("") +
  ylab("") +
  theme(text = element_text(size = 16),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        axis.text = element_blank(),
        axis.ticks = element_blank())
ggsave("figures/western_sens_significance.pdf", width = 7, height = 9)
