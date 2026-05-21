library(tidyverse)
library(terra)
library(sf)

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
