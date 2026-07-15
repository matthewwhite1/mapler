library(tidyverse)
library(terra)
library(tidyterra)
library(RColorBrewer)
library(rnaturalearth)
library(rnaturalearthdata)
library(patchwork)
library(sf)

# Read in files
sap_files <- list.files("F:/Data/LOCA2/sugar_sap_days/", full.names = TRUE)
sap_files <- sap_files[str_detect(sap_files, "prop")]

# Define stuff
scenarios <- c("ssp245", "ssp370", "ssp585")
current_period <- 1980:2014
future_period <- 2070:2100
sap_diffs <- list()

# For each scenario...
for (i in 1:3) {
  # Subset by scenario
  scenario_sap_files <- sap_files[str_detect(sap_files, scenarios[i])]

  # Define empty vectors
  sap_current_means <- list()
  sap_future_means <- list()

  # For each file...
  for (j in seq_along(scenario_sap_files)) {
    # Read in raster
    sap_rast <- rast(scenario_sap_files[j]) |>
      shift(dx = -360)

    # Subset
    sap_current <- sap_rast[[names(sap_rast) %in% current_period]]
    sap_future <- sap_rast[[names(sap_rast) %in% future_period]]

    # Get normal raster within each period
    sap_current_means[[j]] <- app(sap_current, mean)
    sap_future_means[[j]] <- app(sap_future, mean)

    # Print progress
    print(j)
  }

  # Take the average of the means
  sap_current_mean <- app(rast(sap_current_means), mean)
  sap_future_mean <- app(rast(sap_future_means), mean)

  # Find difference between current and future
  sap_diffs[[i]] <- sap_future_mean - sap_current_mean

  # Print progress
  print(paste0("Done with scenario ", scenarios[i]))
}

# Get North America map
world <- ne_countries(scale = "medium", returnclass = "sf")
us_states <- ne_states(country = "United States of America", returnclass = "sf")
canada_provinces <- ne_states(country = "Canada", returnclass = "sf")

# Plot
gs <- list()
for (i in 1:3) {
  sap_diff <- sap_diffs[[i]] |>
    mutate(cuts = cut(mean, round(seq(-0.3, 0.05, by = 0.05), 2), include.lowest = TRUE))
  gs[[i]] <- ggplot() +
    geom_spatraster(data = sap_diff, aes(fill = cuts), alpha = 0.8, show.legend = TRUE) +
    geom_sf(data = us_states, fill = NA, color = "darkgray", size = 0.3) +
    geom_sf(data = canada_provinces, fill = NA, color = "darkgray", size = 0.3) +
    scale_fill_manual("Difference", values = rev(brewer.pal(7, "YlGn")),
                      na.translate = FALSE, drop = FALSE) +
    coord_sf(xlim = c(-125.5, -66.5), ylim = c(23.875, 53.5), expand = FALSE) +
    theme_minimal() +
    xlab("") +
    ylab("") +
    ggtitle(scenarios[i]) +
    theme(text = element_text(size = 10),
          legend.text = element_text(size = 10),
          legend.title = element_text(size = 10),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          plot.title = element_text(hjust = 0.5))
}

jpeg("figures/sap_diff.jpg", width = 9, height = 7, units = "in", res = 600)
(gs[[1]] + gs[[2]] + gs[[3]]) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
dev.off()


########## Western farms ##########
# Read in farms data
western_farms_coords <- read_csv("Data_Clean/western_farms_coords.csv")
western_farms_sf <- sf::st_as_sf(western_farms_coords,
                                 coords = c("longitude", "latitude"), crs = 4326)
western_farms_sf <- western_farms_sf |>
  mutate(species = str_remove(species, " maple"))

# Read in elevation data
elevation <- rast("Data_Clean/elevation.LOCA_2016-04-02.nc")
elevation_farms_west <- extract(elevation, western_farms_sf)

# Extract sap diff for each scenario
sap_diff_farms_west_1 <- extract(sap_diffs[[1]], western_farms_sf)
sap_diff_farms_west_2 <- extract(sap_diffs[[2]], western_farms_sf)
sap_diff_farms_west_3 <- extract(sap_diffs[[3]], western_farms_sf)

# Combine into one dataframe
sap_diff_farms_west <- data.frame(ssp245 = sap_diff_farms_west_1$mean,
                                  ssp370 = sap_diff_farms_west_2$mean,
                                  ssp585 = sap_diff_farms_west_3$mean,
                                  elevation = elevation_farms_west$Elevation,
                                  species = western_farms_sf$species)

# Make longer for ggplot
sap_diff_farms_west_long <- sap_diff_farms_west |>
  pivot_longer(cols = starts_with("ssp"),
               names_to = "scenario",
               values_to = "diff")

# Plot
jpeg("figures/sap_diff_farms_west.jpg", width = 9, height = 4, units = "in", res = 600)
ggplot(sap_diff_farms_west_long, aes(x = elevation, y = diff, color = species)) +
  geom_point() +
  facet_wrap(~ scenario) +
  scale_x_continuous(breaks = seq(0, 2500, by = 500), limits = c(0, 2500)) +
  scale_y_continuous(breaks = seq(-0.15, 0, by = 0.025), limits = c(-0.15, 0)) +
  theme_bw() +
  xlab("Elevation") +
  ylab("Sap Difference") +
  labs(color = "Species") +
  theme(text = element_text(size = 14))
dev.off()


######## Eastern farms
# Read in farms data
eastern_farms_sf <- sf::st_as_sf(farms_coords, coords = c("lon", "lat"), crs = 4326)

# Read in elevation data
elevation <- rast("Data_Clean/elevation.LOCA_2016-04-02.nc")
elevation_farms_east <- extract(elevation, eastern_farms_sf)

# Extract sap diff for each scenario
sap_diff_farms_east_1 <- extract(sap_diffs[[1]], eastern_farms_sf)
sap_diff_farms_east_2 <- extract(sap_diffs[[2]], eastern_farms_sf)
sap_diff_farms_east_3 <- extract(sap_diffs[[3]], eastern_farms_sf)

# Combine into one dataframe
sap_diff_farms_east <- data.frame(ssp245 = sap_diff_farms_east_1$mean,
                                  ssp370 = sap_diff_farms_east_2$mean,
                                  ssp585 = sap_diff_farms_east_3$mean,
                                  elevation = elevation_farms_east$Elevation,
                                  state = eastern_farms_sf$state) |>
  drop_na()

# Make longer for ggplot
sap_diff_farms_east_long <- sap_diff_farms_east |>
  pivot_longer(cols = starts_with("ssp"),
               names_to = "scenario",
               values_to = "diff")

# Plot
jpeg("figures/sap_diff_farms_east.jpg", width = 9, height = 4, units = "in", res = 600)
ggplot(sap_diff_farms_east_long, aes(x = elevation, y = diff, color = state)) +
  geom_point() +
  facet_wrap(~ scenario) +
  scale_x_continuous(breaks = seq(0, 2000, by = 500), limits = c(0, 2000)) +
  scale_y_continuous(breaks = seq(-0.14, 0.02, by = 0.02), limits = c(-0.14, 0.02)) +
  theme_bw() +
  xlab("Elevation") +
  ylab("Sap Difference") +
  labs(color = "State") +
  theme(text = element_text(size = 14))
dev.off()
