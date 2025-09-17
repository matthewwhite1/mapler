### Within scenario ssp585, first run of first three LOCA2 models
# Define stuff
models <- c("ACCESS-CM2", "ACCESS-ESM1-5", "AWI-CM-1-1-MR")
k_upper <- 2.2 + 273.15
k_lower <- -1.1 + 273.15

## Create sap days for all models
for (i in 1:3) {
  # Load in rasters
  path <- paste0("D:/Data/LOCA2/", models[i], "/0p0625deg/r1i1p1f1")
  loca_rast <- loca_t_rast(path)

  # Calculate sap days for model
  model_sap_day <- sap_day(loca_rast$tmax, loca_rast$tmin, k_upper, k_lower)

  # Write rasters to drive
  propname <- paste0("D:/Data/LOCA2/", models[i], "_run1_ssp585_prop.tif")
  terra::writeRaster(model_sap_day$proportion, propname)
  sumname <- paste0("D:/Data/LOCA2/", models[i], "_run1_ssp585_sum.tif")
  terra::writeRaster(model_sap_day$sum, sumname)

  # Garbage collection
  terra::tmpFiles(current = TRUE, orphan = TRUE, old = TRUE, remove = TRUE)
  gc()
  rm(loca_rast)
  rm(model_sap_day)
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
