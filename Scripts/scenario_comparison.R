# Define stuff
models <- loca2_model_names
models <- models[10:27]

# Standard thresholds
# k_upper <- 2.2 + 273.15
# k_lower <- -1.1 + 273.15

# Optimal boxelder thresholds
# k_upper <- 6.2 + 273.15
# k_lower <- -6.2 + 273.15

# Optimal Norway maple thresholds
k_upper <- 6.6 + 273.15
k_lower <- -5.1 + 273.15

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



########## asdjfsa; fdkljsa;dfjk
path <- "F:/Data/LOCA2/ACCESS-CM2/0p0625deg/r1i1p1f1"
loca_rast <- loca_t_rast(path, scenario = c("historical", "ssp245"))
mean_annual_temp <- mean_temp_projection(loca_rast$tmax, loca_rast$tmin)


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





### Computing sap days
# Define stuff
models <- c("EC-Earth3", "EC-Earth3-Veg")
scenarios <- c("ssp245", "ssp370", "ssp585")
k_upper <- 2.2 + 273.15
k_lower <- -1.1 + 273.15

## Create sap days for all models
for (i in 1:2) {
  for (j in 1:3) {
    # Load in rasters
    path <- paste0("D:/Data/LOCA2/", models[i], "/0p0625deg/r1i1p1f1")
    loca_rast <- loca_t_rast(path, c("historical", scenarios[j]))

    # Calculate sap days for model
    model_sap_day <- sap_day(loca_rast$tmax, loca_rast$tmin, k_upper, k_lower)

    # Write rasters to drive
    propname <- paste0("D:/Data/LOCA2/", models[i], "_run1_", scenarios[j], "_prop.tif")
    terra::writeRaster(model_sap_day$proportion, propname, overwrite = TRUE)
    sumname <- paste0("D:/Data/LOCA2/", models[i], "_run1_", scenarios[j], "_sum.tif")
    terra::writeRaster(model_sap_day$sum, sumname, overwrite = TRUE)

    # Free up memory
    terra::tmpFiles(current = TRUE, orphan = TRUE, old = TRUE, remove = TRUE)
    gc()
    rm(loca_rast)
    rm(model_sap_day)
  }
}

###### Get PRISM historical
prism_rast <- prism_t_rast("D:/Data/PRISM/")
prism_sap <- sap_day(prism_rast$tmax, prism_rast$tmin)
terra::writeRaster(prism_sap$proportion, "D:/Data/PRISM_prop.tif")
terra::writeRaster(prism_sap$sum, "D:/Data/PRISM_sum.tif")





##### Get some more sap rasters
models <- c("ACCESS-CM2", "ACCESS-ESM1-5")
k_upper <- 2.2 + 273.15
k_lower <- -1.1 + 273.15
scenarios <- c("ssp245", "ssp370", "ssp585")

# For each model...
for (i in 1:2) {
  # For each run...
  for (j in 2:3) {
    # For each scenario...
    for (h in 1:3) {
      # Load in rasters
      path <- paste0("D:/Data/LOCA2/", models[i], "/0p0625deg/r", j, "i1p1f1")
      loca_rast <- loca_t_rast(path, c("historical", scenarios[h]))

      # Calculate sap days for model
      model_sap_day <- sap_day(loca_rast$tmax, loca_rast$tmin, k_upper, k_lower)

      # Write rasters to drive
      propname <- paste0("D:/Data/LOCA2/", models[i], "_run", j, "_", scenarios[h], "_prop.tif")
      terra::writeRaster(model_sap_day$proportion, propname, overwrite = TRUE)
      sumname <- paste0("D:/Data/LOCA2/", models[i], "_run", j, "_", scenarios[h], "_sum.tif")
      terra::writeRaster(model_sap_day$sum, sumname, overwrite = TRUE)

      # Free up memory
      terra::tmpFiles(current = TRUE, orphan = TRUE, old = TRUE, remove = TRUE)
      gc()
      rm(loca_rast)
      rm(model_sap_day)
    }
  }
}

# For each scenario...
for (i in 1:3) {
  # Load in rasters
  path <- "D:/Data/LOCA2/BCC-CSM2-MR/0p0625deg/r1i1p1f1"
  loca_rast <- loca_t_rast(path, c("historical", scenarios[i]))

  # Calculate sap days for model
  model_sap_day <- sap_day(loca_rast$tmax, loca_rast$tmin, k_upper, k_lower)

  # Write rasters to drive
  propname <- paste0("D:/Data/LOCA2/BCC-CSM2-MR_run1_", scenarios[i], "_prop.tif")
  terra::writeRaster(model_sap_day$proportion, propname, overwrite = TRUE)
  sumname <- paste0("D:/Data/LOCA2/BCC-CSM2-MR_run1_", scenarios[i], "_sum.tif")
  terra::writeRaster(model_sap_day$sum, sumname, overwrite = TRUE)

  # Free up memory
  terra::tmpFiles(current = TRUE, orphan = TRUE, old = TRUE, remove = TRUE)
  gc()
  rm(loca_rast)
  rm(model_sap_day)
}

# Load in rasters
path <- "D:/Data/LOCA2/CESM2-LENS/0p0625deg/r1i1p1f1"
loca_rast <- loca_t_rast(path, c("historical", "ssp370"))

# Calculate sap days for model
model_sap_day <- sap_day(loca_rast$tmax, loca_rast$tmin, k_upper, k_lower)

# Write rasters to drive
propname <- "D:/Data/LOCA2/CESM2-LENS_run1_ssp370_prop.tif"
terra::writeRaster(model_sap_day$proportion, propname, overwrite = TRUE)
sumname <- "D:/Data/LOCA2/CESM2-LENS_run1_ssp370_sum.tif"
terra::writeRaster(model_sap_day$sum, sumname, overwrite = TRUE)

# Free up memory
terra::tmpFiles(current = TRUE, orphan = TRUE, old = TRUE, remove = TRUE)
gc()
rm(loca_rast)
rm(model_sap_day)



## Calculate within scenario mean and variance
# Define stuff
years <- as.integer(terra::names(ssp585_sap[[1]]$proportion))
prop_mean_list <- vector("list", length(years))
prop_var_list <- vector("list", length(years))

# For each year
for (i in seq_along(years)) {
  # Combine into one raster
  combined <- c(ssp585_sap[[1]]$proportion[[i]],
                ssp585_sap[[2]]$proportion[[i]],
                ssp585_sap[[3]]$proportion[[i]])

  # Calculate mean and variance
  prop_mean_list[[i]] <- terra::app(combined, mean)
  prop_var_list[[i]] <- terra::app(combined, var)

  # Print progress
  print(paste0("Completed calculation for year ", years[i]))
}

# Combine raster stacks and write to drive
ssp585_prop_mean <- terra::rast(prop_mean_list)
names(ssp585_prop_mean) <- years
terra::writeRaster(ssp585_prop_mean, "D:/Data/LOCA2/ssp585_prop_mean.tif")

# Combine raster stacks and write to drive
ssp585_prop_var <- terra::rast(prop_var_list)
names(ssp585_prop_var) <- years
terra::writeRaster(ssp585_prop_var, "D:/Data/LOCA2/ssp585_prop_var.tif")




### Between scenarios for "ACCESS-CM2"
# Define stuff
scenarios <- c("ssp245", "ssp370")
accesscm2_sap <- vector("list", 3)
k_upper <- 2.2 + 273.15
k_lower <- -1.1 + 273.15

## Create sap days for all models
for (i in 1:2) {
  # Load in rasters
  path <- "D:/Data/LOCA2/ACCESS-CM2/0p0625deg/r1i1p1f1"
  loca_rast <- loca_t_rast(path, c("historical", scenarios[i]))

  # Calculate sap days for model
  model_sap_day <- sap_day(loca_rast$tmax, loca_rast$tmin, k_upper, k_lower)
  accesscm2_sap[[i]] <- model_sap_day

  # Write rasters to drive
  propname <- paste0("D:/Data/LOCA2/ACCESS-CM2_run1_", scenarios[i], "_prop.tif")
  terra::writeRaster(model_sap_day$proportion, propname)
  sumname <- paste0("D:/Data/LOCA2/ACCESS-CM2_run1_", scenarios[i], "_sum.tif")
  terra::writeRaster(model_sap_day$sum, sumname)
}

# Third entry in list has already been calculated
accesscm2_sap[[3]] <- ssp585_sap[[1]]

## Calculate within scenario mean and variance
# Define stuff
years <- as.integer(terra::names(accesscm2_sap[[1]]$proportion))
prop_mean_list <- vector("list", length(years))
prop_var_list <- vector("list", length(years))

# For each year
for (i in seq_along(years)) {
  # Combine into one raster
  combined <- c(accesscm2_sap[[1]]$proportion[[i]],
                accesscm2_sap[[2]]$proportion[[i]],
                accesscm2_sap[[3]]$proportion[[i]])

  # Calculate mean and variance
  prop_mean_list[[i]] <- terra::app(combined, mean)
  prop_var_list[[i]] <- terra::app(combined, var)

  # Print progress
  print(paste0("Completed calculation for year ", years[i]))
}

# Combine raster stacks and write to drive
accesscm2_sap_prop_mean <- terra::rast(prop_mean_list)
names(accesscm2_sap_prop_mean) <- years
terra::writeRaster(accesscm2_sap_prop_mean, "D:/Data/LOCA2/allscenarios_prop_mean.tif")

# Combine raster stacks and write to drive
accesscm2_sap_prop_var <- terra::rast(prop_var_list)
names(accesscm2_sap_prop_var) <- years
terra::writeRaster(accesscm2_sap_prop_var, "D:/Data/LOCA2/allscenarios_prop_var.tif")
