library(tidyverse)
library(terra)
library(tidyterra)
library(RColorBrewer)
library(rnaturalearth)
library(rnaturalearthdata)
library(patchwork)

# Read in files
tmax_files <- list.files("F:/Data/LOCA2/monthly_tmax/", full.names = TRUE)
tmin_files <- list.files("F:/Data/LOCA2/monthly_tmin/", full.names = TRUE)

# Define stuff
scenarios <- c("ssp245", "ssp370", "ssp585")
year_ind <- rep(1:151, each = 12)
current_period <- 1980:2014
future_period <- 2070:2100
tmax_diffs <- list()
tmin_diffs <- list()

# For each scenario...
for (i in 1:3) {
  # Subset by scenario
  scenario_tmax_files <- tmax_files[str_detect(tmax_files, scenarios[i])]
  scenario_tmin_files <- tmin_files[str_detect(tmin_files, scenarios[i])]

  # Define empty vectors
  tmax_coldest_current_means <- list()
  tmax_coldest_future_means <- list()
  tmin_coldest_current_means <- list()
  tmin_coldest_future_means <- list()

  # For each file...
  for (j in seq_along(scenario_tmax_files)) {
    # Load in rasters
    tmax_rast <- rast(scenario_tmax_files[j]) |>
      shift(dx = -360)
    tmin_rast <- rast(scenario_tmin_files[j]) |>
      shift(dx = -360)

    # Get coldest pixels for each year
    tmax_coldest <- tapp(tmax_rast, year_ind, min)
    names(tmax_coldest) <- 1950:2100
    tmin_coldest <- tapp(tmin_rast, year_ind, min)
    names(tmin_coldest) <- 1950:2100

    # Subset
    tmax_coldest_current <- tmax_coldest[[names(tmax_coldest) %in% current_period]]
    tmax_coldest_future <- tmax_coldest[[names(tmax_coldest) %in% future_period]]
    tmin_coldest_current <- tmin_coldest[[names(tmin_coldest) %in% current_period]]
    tmin_coldest_future <- tmin_coldest[[names(tmin_coldest) %in% future_period]]

    # Get normal rasters within each period
    tmax_coldest_current_means[[j]] <- app(tmax_coldest_current, mean)
    tmax_coldest_future_means[[j]] <- app(tmax_coldest_future, mean)
    tmin_coldest_current_means[[j]] <- app(tmin_coldest_current, mean)
    tmin_coldest_future_means[[j]] <- app(tmin_coldest_future, mean)

    # Print progress
    print(j)
  }

  # Take the average of the means
  tmax_coldest_current_mean <- app(rast(tmax_coldest_current_means), mean)
  tmax_coldest_future_mean <- app(rast(tmax_coldest_future_means), mean)
  tmin_coldest_current_mean <- app(rast(tmin_coldest_current_means), mean)
  tmin_coldest_future_mean <- app(rast(tmin_coldest_future_means), mean)

  # Find difference between current and future
  tmax_diffs[[i]] <- tmax_coldest_future_mean - tmax_coldest_current_mean
  tmin_diffs[[i]] <- tmin_coldest_future_mean - tmin_coldest_current_mean

  # Print progress
  print(paste0("Done with scenario ", scenarios[i]))
}

# Get North America map
world <- ne_countries(scale = "medium", returnclass = "sf")
us_states <- ne_states(country = "United States of America", returnclass = "sf")
canada_provinces <- ne_states(country = "Canada", returnclass = "sf")

# Plot
gs <- list()
titles <- c("tmax", "tmin")
count <- 1
for (i in 1:3) {
  tmax_diff <- tmax_diffs[[i]] |>
    mutate(cuts = cut(mean, seq(0, 16, by = 2), include.lowest = TRUE))
  tmin_diff <- tmin_diffs[[i]] |>
    mutate(cuts = cut(mean, seq(0, 16, by = 2), include.lowest = TRUE))
  diff_list <- list(tmax_diff, tmin_diff)
  for (j in 1:2) {
    gs[[count]] <- ggplot() +
      geom_spatraster(data = diff_list[[j]], aes(fill = cuts), alpha = 0.8, show.legend = TRUE) +
      geom_sf(data = us_states, fill = NA, color = "darkgray", size = 0.3) +
      geom_sf(data = canada_provinces, fill = NA, color = "darkgray", size = 0.3) +
      scale_fill_manual("Difference", values = brewer.pal(8, "YlGn"),
                        na.translate = FALSE, drop = FALSE) +
      coord_sf(xlim = c(-125.5, -66.5), ylim = c(23.875, 53.5), expand = FALSE) +
      theme_minimal() +
      xlab("") +
      ylab("") +
      ggtitle(paste(scenarios[i], titles[j])) +
      theme(text = element_text(size = 10),
            legend.text = element_text(size = 10),
            legend.title = element_text(size = 10),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            plot.title = element_text(hjust = 0.5))
    count <- count + 1
  }
}

jpeg("figures/t_diff.jpg", width = 7, height = 9, units = "in", res = 600)
(gs[[1]] + gs[[2]]) /
  (gs[[3]] + gs[[4]]) /
  (gs[[5]] + gs[[6]]) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
dev.off()
