library(tidyverse)
library(terra)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(patchwork)
library(RColorBrewer)

# Define scenarios and threshold types
scenarios <- c("ssp245", "ssp370", "ssp585")
threshes <- c("sugar", "boxelder", "norway")
df_list <- list()

# Read in eco regions shape file
shapefile <- sf::read_sf("Data_Clean/NA_Eco_Level3/NA_CEC_Eco_Level3.shp")
variable <- names(shapefile)[2]

for (l in 1:3) {
  # Get file paths and sf farm coordinates
  sap_rasts <- list.files(paste0("F:/Data/LOCA2/", threshes[l], "_sap_days/"), full.names = TRUE)
  sap_rasts <- sap_rasts[stringr::str_detect(sap_rasts, "prop")]
  farms_sf <- sf::st_as_sf(farms_coords, coords = c("lon", "lat"), crs = 4326)

  # Define empty significants list
  new_significants <- list()

  # For each scenario...
  for (i in seq_along(scenarios)) {
    # Define empty significants vector
    significants <- c()

    # Subset rasters by scenario
    scenario_rasts <- sap_rasts[stringr::str_detect(sap_rasts, scenarios[i])]

    # For each raster...
    for (j in seq_along(scenario_rasts)) {
      # Load in raster
      sap_rast <- terra::rast(scenario_rasts[j]) |>
        terra::shift(dx = -360)

      # Get sens significant values at each farm
      sap_farms <- get_sens_farms(farms_sf, sap_rast)

      # Append to significants vector
      significants <- c(significants, sap_farms$sens_significant)

      # Print progress
      print(j)
    }

    # Calculate new significant value as mean of significants for each farm
    my_len <- nrow(sap_farms)
    new_significant <- rep(0, my_len)
    for (k in seq_len(my_len)) {
      new_significant[k] <- mean(significants[seq(k, (length(scenario_rasts) * my_len), by = my_len)])
    }

    # Append to significants list
    new_significants[[i]] <- new_significant
  }

  # Make joined scenario dataframes
  ssp245 <- sap_farms |>
    dplyr::mutate(sens_significant = new_significants[[1]]) |>
    get_sens_joined(shapefile, variable) |>
    dplyr::mutate(threshold = threshes[l],
                  scenario = "ssp245")
  ssp370 <- sap_farms |>
    dplyr::mutate(sens_significant = new_significants[[2]]) |>
    get_sens_joined(shapefile, variable) |>
    dplyr::mutate(threshold = threshes[l],
                  scenario = "ssp370")
  ssp585 <- sap_farms |>
    dplyr::mutate(sens_significant = new_significants[[3]]) |>
    get_sens_joined(shapefile, variable) |>
    dplyr::mutate(threshold = threshes[l],
                  scenario = "ssp585")
  df_list[[l]] <- rbind(ssp245, ssp370, ssp585)
}

# Get North America map
world <- ne_countries(scale = "medium", returnclass = "sf")
north_america <- world |>
  filter(region_un == "Americas", name %in% c("United States of America", "Canada"))
us_states <- ne_states(country = "United States of America", returnclass = "sf")
canada_provinces <- ne_states(country = "Canada", returnclass = "sf")

# Make plots
final_df <- rbind(df_list[[1]], df_list[[2]], df_list[[3]])

# Prepare colors
breaks <- seq(-1, 1, by = 0.2)
final_df$sig_mean_class <- cut(final_df$sig_mean, breaks, include.lowest = TRUE)
final_df <- final_df |>
  filter(n_farms >= 5) |>
  mutate(threshold = factor(threshold, levels = c("sugar", "boxelder", "norway"))) |>
  mutate(threshold = recode(threshold,
                            sugar = "Sugar",
                            boxelder = "Boxelder",
                            norway = "Norway"))
# sig_mean_color <- brewer.pal(10, "RdBu")[scenario_df$sig_mean_class]

# Ecoregions plot
jpeg("figures/sens_all_loca.jpg", width = 7, height = 9, units = "in", res = 600)
ggplot() +
  geom_sf(data = north_america, fill = "grey85", color = "black", size = 0.2) +
  geom_sf(data = us_states, fill = NA, color = "grey40", size = 0.3) +
  geom_sf(data = canada_provinces, fill = NA, color = "grey40", size = 0.3) +
  geom_sf(data = final_df, mapping = aes(fill = sig_mean_class)) +
  # geom_sf(data = sap_farms, color = "black", shape = 1, size = 0.4, stroke = 0.3) +
  facet_grid(scenario ~ threshold) +
  coord_sf(xlim = c(-99, -59), ylim = c(32, 51.5), expand = FALSE) +
  scale_fill_manual("Significance Proportion", values = rev(brewer.pal(10, "PuOr"))) +
  theme_minimal() +
  labs(
    # title = scenarios[i],
    x = "Longitude",
    y = "Latitude"
  ) +
  theme(text = element_text(size = 16),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        axis.text = element_blank(),
        axis.ticks = element_blank())
dev.off()

ggsave("figures/sens_all_loca.pdf", width = 7, height = 9) # ChatGPT suggestion

# gs[[1]] <- gs[[1]] +
#   theme(legend.position = "none")
# gs[[2]] <- gs[[2]] +
#   theme(legend.position = "none")
# plots <- gs[[1]] /
#   gs[[2]] /
#   gs[[3]]
# plots + plot_annotation(title = "LOCA2 Sap Day Projections at Temperature Threshold -1.1 to 2.2") +
#   plot_layout(guides = "collect") &
#   theme(plot.title = element_text(hjust = 0.5))
