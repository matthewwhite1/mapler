library(tidyverse)
library(terra)
library(sf)
library(tidyterra)
library(rnaturalearth)
library(rnaturalearthdata)
library(RColorBrewer)
library(patchwork)

# Define scenarios and threshold types
scenarios <- c("ssp245", "ssp370", "ssp585")
threshes <- c("sugar", "boxelder", "norway")
sig_rast_list <- list()
count <- 1

# Create helper function
sens_helper <- function(x) {
  if (any(is.na(x))) {
    return(NA)
  } else if (x[1] > 0 && x[2] <= 0.05) {
    return(1)
  } else if (x[1] < 0 && x[2] <= 0.05) {
    return(-1)
  } else {
    return(0)
  }
}

for (l in 1:3) {
  # Get file paths
  sap_rasts <- list.files(paste0("F:/Data/LOCA2/", threshes[l], "_sap_days/"), full.names = TRUE)
  sap_rasts <- sap_rasts[stringr::str_detect(sap_rasts, "prop")]

  # For each scenario...
  for (i in seq_along(scenarios)) {
    # Subset rasters by scenario
    scenario_rasts <- sap_rasts[stringr::str_detect(sap_rasts, scenarios[i])]

    # For each raster...
    for (j in seq_along(scenario_rasts)) {
      # Load in raster
      sap_rast <- terra::rast(scenario_rasts[j]) |>
        terra::shift(dx = -360)

      # Calculate Sen's slope for every pixel
      sap_rast_sens <- sens_slope_rast(sap_rast)
      sap_rast_est_p <- c(sap_rast_sens$estimates, sap_rast_sens$p.value)

      # Calculate significance
      sens_significance <- terra::app(sap_rast_est_p, sens_helper)
      names(sens_significance) <- "significance"

      # Append to raster vector
      if (j == 1) {
        significants <- sens_significance
      } else {
        significants <- c(significants, sens_significance)
      }

      # Print progress
      print(j)
    }

    # Calculate average significance per scenario
    sig_rast_list[[count]] <- terra::app(significants, mean)
    count <- count + 1
  }
}

# Save rasters
count <- 1
for (i in seq_along(threshes)) {
  for (j in seq_along(scenarios)) {
    filename <- paste0("F:/Data/LOCA2/sens_all_loca/", threshes[i], "_", scenarios[j], ".tif")
    terra::writeRaster(sig_rast_list[[count]], filename, overwrite = TRUE)
    count <- count + 1
  }
}

# Read in rasters
scenarios <- c("ssp245", "ssp370", "ssp585")
threshes <- c("sugar", "boxelder", "norway")
sig_rast_list <- list()
count <- 1
for (i in seq_along(threshes)) {
  for (j in seq_along(scenarios)) {
    filename <- paste0("F:/Data/LOCA2/sens_all_loca/", threshes[i], "_", scenarios[j], ".tif")
    sig_rast_list[[count]] <- terra::rast(filename)
    count <- count + 1
  }
}

# Get North America map
world <- ne_countries(scale = "medium", returnclass = "sf")
us_states <- ne_states(country = "United States of America", returnclass = "sf")
canada_provinces <- ne_states(country = "Canada", returnclass = "sf")

# Create discrete breaks
breaks <- seq(-1, 0.6, by = 0.2)
manual_breaks <- c(
  "[-1, -0.8]",
  "(-0.8, -0.6]",
  "(-0.6, -0.4]",
  "(-0.4, -0.2]",
  "(-0.2, 0]",
  "(0, 0.2]",
  "(0.2, 0.4]",
  "(0.4, 0.6]"
)
for (i in 1:9) {
  sig_rast_list[[i]] <- sig_rast_list[[i]] |>
    mutate(cuts = cut(mean, breaks, include.lowest = TRUE, labels = manual_breaks)) |>
    mutate(cuts = factor(cuts, levels = manual_breaks))
}

# Plot
titles <- paste(rep(stringr::str_to_title(threshes), each = 3), rep(scenarios, times = 3))
gs <- list()
for (i in 1:9) {
  gs[[i]] <- ggplot() +
    geom_spatraster(data = sig_rast_list[[i]], aes(fill = cuts), show.legend = TRUE) +
    geom_sf(data = us_states, fill = NA, color = "darkgray", size = 0.3) +
    geom_sf(data = canada_provinces, fill = NA, color = "darkgray", size = 0.3) +
    scale_fill_manual("Significance mean", values = brewer.pal(10, "PuOr")[10:3],
                      na.translate = FALSE, drop = FALSE) +
    coord_sf(xlim = c(-125.5, -66.5), ylim = c(23.875, 53.5), expand = FALSE) +
    ggtitle(titles[i]) +
    theme_minimal() +
    theme(text = element_text(size = 10),
          legend.text = element_text(size = 10),
          legend.title = element_text(size = 10),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          plot.title = element_text(hjust = 0.5))
}
(gs[[1]] + gs[[4]] + gs[[7]]) /
  (gs[[2]] + gs[[5]] + gs[[8]]) /
  (gs[[3]] + gs[[6]] + gs[[9]]) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
ggsave("figures/sens_conus_all_loca.pdf", width = 7, height = 9)
