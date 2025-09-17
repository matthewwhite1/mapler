#' Load LOCA2 tmax and tmin files into rasters
#'
#' @param filepath Character file path where files are located. This should
#'   be the folder in which the climate scenario folders are contained (historical,
#'   ssp585, etc.). For example, "ACCESS-CM2/0p0625deg/r1i1p1f1" would be
#'   valid for this argument.
#' @param scenario A character vector of future climate scenarios. Each value
#'   in this vector must be either historical, ssp245, ssp370, or ssp585.
#'
#' @return A list of length two - the tmax raster stack and the tmin raster
#'   stack.
#'
#' @export
loca_t_rast <- function(filepath, scenario = c("historical", "ssp585")) {
  # Check if directory exists
  if (!dir.exists(filepath)) {
    stop("Given directory does not exist.")
  }

  # Get scenario file paths
  scenario_folders <- c()
  for (period in scenario) {
    scenario_folders <- c(scenario_folders, file.path(filepath, period))
    if (!dir.exists(file.path(filepath, period))) {
      stop(paste0("Given scenario folder does not exist for scenario ", period))
    }
  }

  # Initialize empty lists
  tmax_list <- vector("list", length(scenario_folders))
  tmin_list <- vector("list", length(scenario_folders))

  # For each folder...
  for (i in seq_along(scenario_folders)) {
    # List netCDF files
    tmax_files <- list.files(file.path(scenario_folders[i], "tasmax"), full.names = TRUE)
    tmin_files <- list.files(file.path(scenario_folders[i], "tasmin"), full.names = TRUE)

    # Skip if no valid files are found
    if (length(tmax_files) == 0 || length(tmin_files) == 0) {
      warning(paste0("No tmax or tmin files found in the given directory ", scenario_folders[i]))
      next
    }

    # Check for chronological ordering of files
    tmax_file_year <- as.integer(stringr::str_extract(basename(tmax_files), "[[:digit:]]{4}"))
    tmin_file_year <- as.integer(stringr::str_extract(basename(tmin_files), "[[:digit:]]{4}"))
    if (!all(sort(tmax_file_year) == tmax_file_year)) {
      tmax_files <- tmax_files[order(tmax_file_year)]
      tmax_file_year <- sort(tmax_file_year)
    }
    if (!all(sort(tmin_file_year) == tmin_file_year)) {
      tmin_files <- tmin_files[order(tmin_file_year)]
      tmin_file_year <- sort(tmin_file_year)
    }

    # Skip if tmax and tmin don't have exact same dates
    if (!all(tmax_file_year == tmin_file_year)) {
      warning(paste0("tmax and tmin don't have exact same years in given directory ", scenario_folders[i]))
      next
    }

    # Load in rasters
    tmax_run_rast <- terra::rast(tmax_files)
    tmin_run_rast <- terra::rast(tmin_files)

    # Put rasters in lists
    tmax_list[[i]] <- tmax_run_rast
    tmin_list[[i]] <- tmin_run_rast

    message(paste0("Successfully loaded rasters for scenario ", scenario_folders[i]))
  }

  # Stop if no scenarios ended up being valid
  if (all(sapply(tmax_list, is.null)) || all(sapply(tmin_list, is.null))) {
    stop("No valid scenario raster stacks were found.")
  }

  # Set raster names
  names(tmax_list) <- basename(scenario_folders)
  names(tmin_list) <- basename(scenario_folders)

  # Prepare final rasters
  tmax_rast <- terra::rast(tmax_list)
  tmin_rast <- terra::rast(tmin_list)

  # Return list of final rasters
  list(tmax = tmax_rast, tmin = tmin_rast)
}
