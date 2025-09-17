#' @export
mean_temp_projection <- function(tmax_rast, tmin_rast) {
  # Error checking
  if (class(tmax_rast) != "SpatRaster" || class(tmin_rast) != "SpatRaster") {
    stop("tmax_rast and tmin_rast must be terra rasters.")
  } else if (!all(terra::time(tmax_rast) == terra::time(tmin_rast))) {
    stop("tmax_rast and tmin_rast must have identical terra::time values.")
  }

  # Compute daily mean temperature
  mean_daily_temp <- (tmax_rast + tmin_rast) / 2

  # Extract years for subsetting
  dates <- terra::time(tmax_rast)
  years <- as.integer(stringr::str_extract(dates, "[[:digit:]]{4}"))
  unique_years <- sort(unique(years))

  # Compute annual mean
  mean_annual_temp <- terra::tapp(mean_daily_temp, index = years, fun = mean)
  names(mean_annual_temp) <- unique_years

  # Return rasters
  mean_annual_temp
}
