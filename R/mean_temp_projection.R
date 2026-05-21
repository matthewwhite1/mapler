#' Calculate yearly mean temperatures from temperature rasters
#'
#' @param tmax_rast terra raster stack of tmax values that must have terra::time
#'   values containing date information to subset by year
#' @param tmin_rast terra raster stack of tmin values that must have terra::time
#'   values containing date information to subset by year
#'
#' @return raster stack of yearly mean temperatures
#'
#' @export
mean_temp_projection <- function(tmax_rast, tmin_rast) {
  # Error checking
  if (!inherits(tmax_rast, "SpatRaster") || !inherits(tmin_rast, "SpatRaster")) {
    stop("tmax_rast and tmin_rast must be terra rasters.")
  } else if (!all(terra::time(tmax_rast) == terra::time(tmin_rast))) {
    stop("tmax_rast and tmin_rast must have identical terra::time values.")
  }

  # Extract years for subsetting
  dates <- terra::time(tmax_rast)
  years <- as.integer(stringr::str_extract(dates, "[[:digit:]]{4}"))
  unique_years <- sort(unique(years))

  # Initialize empty lits
  mean_temp_list <- vector("list", length(unique_years))

  # For each year...
  for (i in seq_along(unique_years)) {
    # Subset rasters by year and compute daily mean temperature
    year_layers <- which(years == unique_years[i])
    tmax_year <- tmax_rast[[year_layers]]
    tmin_year <- tmin_rast[[year_layers]]
    mean_daily_temp_year <- (tmax_year + tmin_year) / 2

    # Calculate mean temperature by year
    mean_year_temp <- terra::app(mean_daily_temp_year, mean)

    # Put raster in list
    mean_temp_list[[i]] <- mean_year_temp

    # Print year for progress
    message(paste0("Successfully calculated mean temperatures for
                   year ", unique_years[i]))

    # Free up memory
    gc()
  }

  # Convert list to raster stack
  mean_annual_temp <- terra::rast(mean_temp_list)
  names(mean_annual_temp) <- unique_years

  # Return rasters
  mean_annual_temp
}
