# OLD FUNCTION
loca_sap_day <- function(filepath) {
  # List folders (historical, scenarios)
  run_folders <- list.files(filepath, full.names = TRUE)

  # Initialize output list
  final <- vector("list", length(run_folders))

  # For each folder...
  for (i in seq_along(run_folders)) {
    # List netCDF files
    tmax_files <- list.files(paste0(run_folders[i], "/tasmax"), full.names = TRUE)
    tmin_files <- list.files(paste0(run_folders[i], "/tasmin"), full.names = TRUE)

    # Check for chronological ordering of files
    tmax_file_year <- as.integer(stringr::str_extract(tmax_files, "[[:digit:]]{4}"))
    tmin_file_year <- as.integer(stringr::str_extract(tmin_files, "[[:digit:]]{4}"))
    if (!all(sort(tmax_file_year) == tmax_file_year)) {
      tmax_files <- tmax_files[order(tmax_file_year)]
    } else if (!all(sort(tmin_file_year) == tmin_file_year)) {
      tmin_files <- tmin_files[order(tmin_file_year)]
    }

    # Load in rasters
    tmax_rast <- terra::rast(tmax_files)
    tmin_rast <- terra::rast(tmin_files)

    # Logical statement with desired temps
    sap_day <- tmax_rast > 2.2 & tmin_rast < -1.1

    # Create year index
    years <- as.integer(stringr::str_extract(terra::time(sap_day), "^[[:digit:]]{4}"))
    tapp_index <- years - years[1] + 1

    # Find proportion per year
    sap_prop <- terra::tapp(sap_day, tapp_index, mean)
    names(sap_prop) <- unique(years)

    # Find sum per year
    sap_sum <- terra::tapp(sap_day, tapp_index, sum)
    names(sap_prop) <- unique(years)

    # Load into list
    final[[i]] <- list(sap_prop, sap_sum)
  }

  # Return final list
  final
}
