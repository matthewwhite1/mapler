library(tidyverse)
library(terra)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(tidyterra)
library(RColorBrewer)
library(patchwork)

# Define scenarios
scenarios <- c("ssp245", "ssp370", "ssp585")
scenario_rasts <- list()
files <- list.files("F:/Data/LOCA2/mean_temp_projections/", full.names = TRUE)
count <- 1

# Year ranges:
#   1950 - 1979
#   1980 - 2014
#   2015 - 2039
#   2040 - 2069
#   2070 - 2100
range1 <- 1950:1979
range2 <- 1980:2014
range3 <- 2015:2039
range4 <- 2040:2069
range5 <- 2070:2100
ranges <- list(range1, range2, range3, range4, range5)

# For each scenario
for (l in 1:3) {
  # Subset by scenario
  scenario_files <- files[stringr::str_detect(files, scenarios[l])]
  mean_temp_ranges <- list()

  # For each file
  for (k in seq_along(scenario_files)) {
    # Read in raster and convert to Celsius
    mean_temp_rast <- terra::rast(scenario_files[k]) |>
      terra::shift(dx = -360)
    mean_temp_rast <- mean_temp_rast - 273.15

    # Calculate mean temperature raster for every range
    for (i in 1:5) {
      mean_temp_range <- mean_temp_rast[[which(names(mean_temp_rast) %in% as.character(ranges[[i]]))]]
      if (k == 1) {
        mean_temp_ranges[[i]] <- terra::app(mean_temp_range, mean)
      } else {
        mean_temp_ranges[[i]] <- c(mean_temp_ranges[[i]], terra::app(mean_temp_range, mean))
      }
    }

    # Print progress
    print(paste0("Done with file ", k))
  }

  # Calculate the mean of all files
  for (i in 1:5) {
    scenario_rasts[[count]] <- terra::app(mean_temp_ranges[[i]], mean)
    count <- count + 1
  }

  # Print progress
  print(paste0("Done with scenario ", scenarios[l]))
}

# Get North America map
world <- ne_countries(scale = "medium", returnclass = "sf")
us_states <- ne_states(country = "United States of America", returnclass = "sf")
canada_provinces <- ne_states(country = "Canada", returnclass = "sf")

# Get discrete breaks
breaks <- seq(-10, 35, by = 5)
for (i in 1:15) {
  scenario_rasts[[i]] <- scenario_rasts[[i]] |>
    mutate(cuts = cut(mean, breaks, include.lowest = TRUE))
}

# Plot all
gs <- list()
range_names <- c("1950 - 1979", "1980 - 2014", "2015 - 2039",
            "2040 - 2069", "2070 - 2100")
titles <- paste(rep(scenarios, each = 5), rep(range_names, times = 5))
my_palette <- c(rev(brewer.pal(3, "Blues")), brewer.pal(7, "OrRd"))
for (i in 1:15) {
  gs[[i]] <- ggplot() +
    geom_spatraster(data = scenario_rasts[[i]], aes(fill = cuts), show.legend = TRUE) +
    geom_sf(data = us_states, fill = NA, color = "darkgray", size = 0.3) +
    geom_sf(data = canada_provinces, fill = NA, color = "darkgray", size = 0.3) +
    coord_sf(xlim = c(-125.5, -66.5), ylim = c(23.875, 53.5), expand = FALSE) +
    scale_fill_manual("Mean temperature", values = my_palette,
                      na.translate = FALSE, drop = FALSE) +
    ggtitle(titles[i]) +
    theme_minimal() +
    theme(text = element_text(size = 10),
          legend.text = element_text(size = 10),
          legend.title = element_text(size = 10),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          plot.title = element_text(hjust = 0.5))
}
jpeg("figures/mean_temp_conus_all_loca.jpg", width = 7, height = 9, units = "in", res = 600)
(gs[[1]] + gs[[6]] + gs[[11]]) /
  (gs[[2]] + gs[[7]] + gs[[12]]) +
  (gs[[3]] + gs[[8]] + gs[[13]]) +
  (gs[[4]] + gs[[9]] + gs[[14]]) +
  (gs[[5]] + gs[[10]] + gs[[15]]) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
dev.off()

wrap_plots(gs, ncol = 5, guides = "collect")
ggsave("figures/mean_temp_conus_all_loca_wide.pdf", width = 10, height = 7)
