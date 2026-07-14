library(tidyverse)
library(terra)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(tidyterra)
library(RColorBrewer)
library(patchwork)

# List available folders
folders <- list.dirs("F:/Data/LOCA2/")
folders_nopath <- list.dirs("F:/Data/LOCA2/", full.names = FALSE)
my_models <- folders[which(folders_nopath %in% loca2_model_names)]
my_models_nopath <- folders_nopath[which(folders_nopath %in% loca2_model_names)]

# For each model...
for (i in seq_along(my_models)) {
  # List available runs
  runs <- list.files(paste0(my_models[i], "/0p0625deg/"), full.names = TRUE)
  runs_nopath <- list.files(paste0(my_models[i], "/0p0625deg/"))
  run_nums <- stringr::str_extract(runs_nopath, "^.{2}")

  # For each run...
  for (j in seq_along(runs)) {
    # List available scenarios
    scenarios <- list.files(runs[j], full.names = TRUE)
    scenarios <- scenarios[!stringr::str_detect(scenarios, "historical")]
    scenarios_nopath <- list.files(runs[j])
    scenarios_nopath <- scenarios_nopath[!stringr::str_detect(scenarios_nopath, "historical")]

    # For each scenario...
    for (k in seq_along(scenarios)) {
      # Only load and calculate historical once
      if (k == 1) {
        hist_rast <- loca_t_rast(runs[j], scenario = "historical")
        hist_april_tmax <- hist_rast$tmax[[which(lubridate::month(time(hist_rast$tmax)) == 4)]]
        hist_april_tmin <- hist_rast$tmin[[which(lubridate::month(time(hist_rast$tmin)) == 4)]]
        hist_mean_temp <- mean_temp_projection(hist_april_tmax, hist_april_tmin)
      }

      # Calculate scenario mean temps
      scenario_rast <- loca_t_rast(runs[j], scenario = scenarios_nopath[k])
      scenario_april_tmax <- scenario_rast$tmax[[which(lubridate::month(time(scenario_rast$tmax)) == 4)]]
      scenario_april_tmin <- scenario_rast$tmin[[which(lubridate::month(time(scenario_rast$tmin)) == 4)]]
      scenario_mean_temp <- mean_temp_projection(scenario_april_tmax, scenario_april_tmin)

      # Combine historical and scenario
      mean_temp <- c(hist_mean_temp, scenario_mean_temp)
      filename <- paste0("F:/Data/LOCA2/april_mean_temps/", my_models_nopath[i],
                         "_", run_nums[j], "_", scenarios_nopath[k], "_april.tif")
      terra::writeRaster(mean_temp, filename)
    }
  }

  # Garbage collection
  terra::tmpFiles(current = TRUE, orphan = TRUE, old = TRUE, remove = TRUE)
  gc()
}

########## Sen's stuff ##########
# Calculate Sen's slope for every file - code taken from sens_all_loca.R
april_files <- list.files("F:/Data/LOCA2/april_mean_temps/", full.names = TRUE)
scenarios <- c("ssp245", "ssp370", "ssp585")
sig_rast_list <- list()
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
for (i in seq_along(scenarios)) {
  # Subset rasters by scenario
  scenario_rasts <- april_files[stringr::str_detect(april_files, scenarios[i])]

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
  sig_rast_list[[i]] <- terra::app(significants, mean)
}

# Write rasters
for (i in 1:3) {
  filename <- paste0("F:/Data/LOCA2/april_mean_temps/april_sens_", scenarios[i], ".tif")
  terra::writeRaster(sig_rast_list[[i]], filename)
}

# Read rasters
scenarios <- c("ssp245", "ssp370", "ssp585")
sig_rast_list <- list()
for (i in 1:3) {
  filename <- paste0("F:/Data/LOCA2/april_mean_temps/april_sens_", scenarios[i], ".tif")
  sig_rast_list[[i]] <- terra::rast(filename)
}

# Get North America map
world <- ne_countries(scale = "medium", returnclass = "sf")
us_states <- ne_states(country = "United States of America", returnclass = "sf")
canada_provinces <- ne_states(country = "Canada", returnclass = "sf")

# Create discrete breaks
breaks <- c(0.7, 0.8, 0.95, 1)
for (i in 1:3) {
  sig_rast_list[[i]] <- sig_rast_list[[i]] |>
    mutate(cuts = cut(mean, breaks, include.lowest = TRUE))
}

# Plot
gs <- list()
for (i in 1:3) {
  gs[[i]] <- ggplot() +
    geom_spatraster(data = sig_rast_list[[i]], aes(fill = cuts), show.legend = TRUE) +
    geom_sf(data = us_states, fill = NA, color = "darkgray", size = 0.3) +
    geom_sf(data = canada_provinces, fill = NA, color = "darkgray", size = 0.3) +
    scale_fill_manual("Significance mean", values = brewer.pal(7, "PuOr")[3:1],
                      na.translate = FALSE, drop = FALSE) +
    coord_sf(xlim = c(-125.5, -66.5), ylim = c(23.875, 53.5), expand = FALSE) +
    ggtitle(scenarios[i]) +
    theme_minimal() +
    theme(text = element_text(size = 10),
          legend.text = element_text(size = 10),
          legend.title = element_text(size = 10),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          plot.title = element_text(hjust = 0.5))
}
(gs[[1]] + gs[[2]] + gs[[3]]) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
ggsave("figures/april_sens.pdf", width = 7, height = 9)
########## Sen's stuff ##########


########## Mean temperature plotting ##########
# Define scenarios
scenarios <- c("ssp245", "ssp370", "ssp585")
scenario_rasts <- list()
files <- list.files("F:/Data/LOCA2/april_mean_temps/", full.names = TRUE)
files <- files[!stringr::str_detect(files, "sens")]
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
jpeg("figures/april_mean_temps.jpg", width = 7, height = 9, units = "in", res = 600)
(gs[[1]] + gs[[6]] + gs[[11]]) /
  (gs[[2]] + gs[[7]] + gs[[12]]) +
  (gs[[3]] + gs[[8]] + gs[[13]]) +
  (gs[[4]] + gs[[9]] + gs[[14]]) +
  (gs[[5]] + gs[[10]] + gs[[15]]) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
dev.off()

wrap_plots(gs, ncol = 5, guides = "collect")
ggsave("figures/april_mean_temps_wide.pdf", width = 10, height = 7)
