#' Calculate ideal sap days from temperature rasters
#'
#' Maple sap flows the most on days where temperatures are oscillating between
#'   freezing and thawing, known as the freeze/thaw cycle. This function uses
#'   tmax and tmin rasters (potentially loaded in from [loca_t_rast()] or
#'   [prism_t_rast()]) to calculate the proportion and sum of days in each year
#'   that are ideal for maple sap tapping. Using a 30-year record of sap flow
#'   and weather observations at the Uihlein Forest, Lake Placid, NY, sap flow
#'   occurred on 80% of the days when the minimum temperature fell below
#'   -1.1 °C and maximum temperature exceeded 2.2 °C, which is where the
#'   default values for the temperature threshold come from (Skinner et al.
#'   2010).
#'
#' @param tmax_rast terra raster stack of tmax values that must have terra::time
#'   values containing date information to subset by year
#' @param tmin_rast terra raster stack of tmin values that must have terra::time
#'   values containing date information to subset by year
#' @param t_upper upper temperature value for the freeze/thaw cycle that
#'   will be used in a logical statement to find ideal sap tapping days
#' @param t_lower lower temperature value for the freeze/thaw cycle that
#'   will be used in a logical statement to find ideal sap tapping days
#' @param years integer vector of years to subset the raster stacks by. All
#'   of the years must be present in the raster stacks
#'
#' @return list of length two - a raster stack of the proportion of ideal
#'   sap tapping days per year, and a raster stack of the sum of ideal sap
#'   tapping days per year
#'
#' @examples
#' \dontrun{
#' # Load in rasters
#' loca_rast <- loca_t_rast("D:/Data/LOCA2/ACCESS-CM2/0p0625deg/r1i1p1f1/")
#'
#' # Calculate yearly sap days
#' k_upper <- 2.2 + 273.15
#' k_lower <- -1.1 + 273.15
#' loca_sap_day <- sap_day(loca_rast$tmax, loca_rast$tmin,
#'                         t_upper = k_upper, t_lower = k_lower)
#' }
#' @export
sap_day <- function(tmax_rast, tmin_rast, t_upper = 2.2, t_lower = -1.1, years = NULL) {
  # Error checking
  if (!inherits(tmax_rast, "SpatRaster") || !inherits(tmin_rast, "SpatRaster")) {
    stop("tmax_rast and tmin_rast must be terra rasters.")
  } else if (!all(terra::time(tmax_rast) == terra::time(tmin_rast))) {
    stop("tmax_rast and tmin_rast must have identical terra::time values.")
  } else if (!is.numeric(t_upper)) {
    stop("t_upper must be numeric.")
  } else if (!is.numeric(t_lower)) {
    stop("t_lower must be numeric.")
  }

  # Extract years for subsetting
  dates <- terra::time(tmax_rast)
  rast_years <- as.integer(stringr::str_extract(dates, "[[:digit:]]{4}"))
  if (!is.null(years)) {
    if (!is.numeric(years) || !is.vector(years)) {
      stop("years must be numeric vector.")
    }
    unique_years <- years
  } else {
    unique_years <- sort(unique(rast_years))
  }

  # Initialize empty lists
  sap_prop_list <- vector("list", length(unique_years))
  sap_sum_list <- vector("list", length(unique_years))

  # For each year...
  for (i in seq_along(unique_years)) {
    # Subset rasters by year
    year_layers <- which(rast_years == unique_years[i])
    tmax_year <- tmax_rast[[year_layers]]
    tmin_year <- tmin_rast[[year_layers]]

    # Logical statement with temperature threshold
    sap_day <- tmax_year > t_upper & tmin_year < t_lower

    # Calculate proportion of sap days
    sap_prop_year <- terra::app(sap_day, mean)

    # Calculate sum of sap days
    sap_sum_year <- terra::app(sap_day, sum)

    # Put rasters in lists
    sap_prop_list[[i]] <- sap_prop_year
    sap_sum_list[[i]] <- sap_sum_year

    # Print year for progress
    message(paste0("Successfully calculated proportion and sum of sap days for
                   year ", unique_years[i]))

    # Free up memory
    gc()
    rm(sap_day)
    rm(sap_prop_year)
    rm(sap_sum_year)
  }

  # Combine rasters into one with layer for each year
  sap_prop <- terra::rast(sap_prop_list)
  sap_sum <- terra::rast(sap_sum_list)

  # Set year names
  names(sap_prop) <- unique_years
  names(sap_sum) <- unique_years

  # Return list of final rasters
  list(proportion = sap_prop, sum = sap_sum)
}
