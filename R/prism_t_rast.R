#' Load PRISM tmax and tmin files into rasters
#'
#' @param filepath Character file path where files are located.
#' @param folders Boolean. If TRUE, the filepath contains folders that are
#'   organized by year (1981, 1982, etc.). If FALSE, the filepath must contain
#'   tmax and tmin .bil files.
#'
#' @return A list of length two - the tmax raster stack and the tmin raster
#'   stack.
#'
#' @export
prism_t_rast <- function(filepath, folders = TRUE) {
  # Check if directory exists
  if (!dir.exists(filepath)) {
    stop("Given directory does not exist.")
  }

  # Ensure folders argument is logical
  if (!is.logical(folders)) {
    stop("Folders argument must either be TRUE or FALSE.")
  }

  # Define years vector based on whether there are year folders
  if (folders) {
    # Check ordering of year folders
    year_folders <- list.files(filepath, full.names = TRUE)
    years <- as.integer(stringr::str_extract(basename(year_folders), "[[:digit:]]{4}"))

    # Stop if any folders don't have a year in their name
    if (any(is.na(years))) {
      stop("At least one folder does not have a year in its name.")
    }

    # Sort years if not sorted
    if (!all(sort(years) == years)) {
      year_folders <- year_folders[order(years)]
      years <- sort(years)
    }
  } else {
    # Read in files
    t_files <- list.files(filepath, full.names = TRUE)
    tmax_all_files <- t_files[stringr::str_detect(basename(t_files), "tmax.*bil$")]
    tmin_all_files <- t_files[stringr::str_detect(basename(t_files), "tmin.*bil$")]

    # Stop if no valid files are found
    if (length(tmax_all_files) == 0 || length(tmin_all_files) == 0) {
      stop("No tmax or tmin .bil files found in the given directory.")
    }

    # Extract years from file names
    tmax_years <- as.integer(stringr::str_extract(basename(tmax_all_files), "[[:digit:]]{4}"))
    tmax_unique_years <- sort(unique(tmax_years))
    tmin_years <- as.integer(stringr::str_extract(basename(tmin_all_files), "[[:digit:]]{4}"))
    tmin_unique_years <- sort(unique(tmin_years))
    years <- intersect(tmax_unique_years, tmin_unique_years)
  }

  # Initialize empty lists
  tmax_list <- vector("list", length(years))
  tmin_list <- vector("list", length(years))

  # For each year...
  for (i in seq_along(years)) {
    # Read in files based on whether there are year folders
    if (folders) {
      # Read in files
      t_files <- list.files(year_folders[i], full.names = TRUE)
      tmax_files <- t_files[stringr::str_detect(basename(t_files), "tmax.*bil$")]
      tmin_files <- t_files[stringr::str_detect(basename(t_files), "tmin.*bil$")]

      # Skip if no valid files are found for this year
      if (length(tmax_files) == 0 || length(tmin_files) == 0) {
        warning(paste0("No tmax or tmin .bil files found in year ", years[i]))
        next
      }
    } else {
      # Subset files by year
      tmax_files <- tmax_all_files[tmax_years == years[i]]
      tmin_files <- tmin_all_files[tmin_years == years[i]]
    }

    # Skip if tmax and tmin have different amount of files
    if (length(tmax_files) != length(tmin_files)) {
      warning(paste0("Different amount of files between tmax and tmin for year ", years[i]))
      next
    }

    # Check ordering of dates
    tmax_dates <- stringr::str_extract(basename(tmax_files), "[[:digit:]]{8}") |>
      lubridate::as_date()
    if (!all(sort(tmax_dates) == tmax_dates)) {
      tmax_files <- tmax_files[order(tmax_dates)]
      tmax_dates <- sort(tmax_dates)
    }
    tmin_dates <- stringr::str_extract(basename(tmin_files), "[[:digit:]]{8}") |>
      lubridate::as_date()
    if (!all(sort(tmin_dates) == tmin_dates)) {
      tmin_files <- tmin_files[order(tmin_dates)]
      tmin_dates <- sort(tmin_dates)
    }

    # Skip if tmax and tmin don't have exact same dates
    if (!all(tmax_dates == tmin_dates)) {
      warning(paste0("tmax and tmin don't have exact same dates for year ", years[i]))
      next
    }

    # Load in rasters and set time
    tmax_year_rast <- terra::rast(tmax_files)
    terra::time(tmax_year_rast) <- tmax_dates
    tmin_year_rast <- terra::rast(tmin_files)
    terra::time(tmin_year_rast) <- tmin_dates

    # Put rasters in lists
    tmax_list[[i]] <- tmax_year_rast
    tmin_list[[i]] <- tmin_year_rast

    message(paste0("Successfully loaded rasters for year ", years[i]))
  }

  # Stop if no years ended up being valid
  if (all(sapply(tmax_list, is.null)) || all(sapply(tmin_list, is.null))) {
    stop("No valid yearly raster stacks were found.")
  }

  # Prepare final rasters
  tmax_rast <- terra::rast(tmax_list)
  tmin_rast <- terra::rast(tmin_list)

  # Return list of final rasters
  list(tmax = tmax_rast, tmin = tmin_rast)
}
