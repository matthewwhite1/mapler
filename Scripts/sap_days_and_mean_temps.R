# Define stuff
models <- loca2_model_names
models <- models[10:27]

# Optimal sugar thresholds
k_upper <- 2.2 + 273.15
k_lower <- -1.1 + 273.15

# Optimal boxelder thresholds
# k_upper <- 6.2 + 273.15
# k_lower <- -6.2 + 273.15

# Optimal Norway maple thresholds
# k_upper <- 6.6 + 273.15
# k_lower <- -5.1 + 273.15

## Create sap days for all models
for (i in seq_along(models)) {
  # Break if folder doesn't exist
  if (!(models[i] %in% list.files("F:/LOCA2/"))) {
    next
  }

  # Get available runs
  runs <- list.files(paste0("F:/LOCA2/", models[i], "/0p0625deg/"))

  # For each run...
  for (j in seq_along(runs)) {
    # Calculate sap days for historical
    run_path <- paste0("F:/LOCA2/", models[i], "/0p0625deg/", runs[j])
    historical_rast <- loca_t_rast(run_path, scenario = "historical")
    historical_sap_day <- sap_day(historical_rast$tmax, historical_rast$tmin, k_upper, k_lower)

    # Get available scenarios
    scenarios <- list.files(run_path)
    scenarios <- scenarios[scenarios != "historical"]

    # For each scenario...
    for (k in seq_along(scenarios)) {
      # Load in rasters
      scenario_rast <- loca_t_rast(run_path, scenario = scenarios[k])

      # Calculate sap days for model
      scenario_sap_day <- sap_day(scenario_rast$tmax, scenario_rast$tmin, k_upper, k_lower)

      # Combine historical and scenario
      model_sap_day <- list()
      model_sap_day$proportion <- c(historical_sap_day$proportion, scenario_sap_day$proportion)
      model_sap_day$sum <- c(historical_sap_day$sum, scenario_sap_day$sum)

      # Get run num
      run_num <- stringr::str_extract(runs[j], "r\\d") |>
        stringr::str_replace("r", "run")

      # Write rasters to drive
      propname <- paste0("F:/LOCA2/", models[i], "_", run_num, "_", scenarios[k], "_prop.tif")
      terra::writeRaster(model_sap_day$proportion, propname)
      sumname <- paste0("F:/LOCA2/", models[i], "_", run_num, "_", scenarios[k], "_sum.tif")
      terra::writeRaster(model_sap_day$sum, sumname)
    }

    # Garbage collection
    terra::tmpFiles(current = TRUE, orphan = TRUE, old = TRUE, remove = TRUE)
    gc()
    rm(historical_sap_day)
    rm(scenario_sap_day)
    rm(model_sap_day)
  }
}


# Mean temperature projection
folders <- list.dirs("F:/Data/LOCA2/", full.names = TRUE, recursive = FALSE)
folders <- folders[!stringr::str_detect(folders, "mean_temp")]
model_names <- stringr::str_extract(folders, "/[^/]*$") |>
  stringr::str_remove("/")
scenarios <- c("ssp245", "ssp370", "ssp585")
loca_rast_catcher <- function(run_folder, scenario) {
  tryCatch(
    {
      loca_t_rast(run_folder, scenario = c("historical", scenario))
    },
    error = function(e) {
      print(e)
      NULL
    }
  )
}
for (i in seq_along(folders)) {
  run_folders <- list.dirs(paste0(folders[i], "/0p0625deg"), full.names = TRUE, recursive = FALSE)
  for (run_folder in run_folders) {
    run_num <- stringr::str_extract(run_folder, "r\\d") |>
      stringr::str_remove("r")
    for (scenario in scenarios) {
      filename <- paste0("F:/Data/LOCA2/mean_temp_projections/mean_temp_rast_", model_names[i], "_", scenario, "_run", run_num, ".tif")
      if (file.exists(filename)) {
        break
      }
      loca_rast <- loca_rast_catcher(run_folder, scenario)
      if (!is.null(loca_rast)) {
        mean_annual_temp <- mean_temp_projection(loca_rast$tmax, loca_rast$tmin)
        terra::writeRaster(mean_annual_temp, filename, overwrite = TRUE)
      }
    }
  }
}
