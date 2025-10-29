#' Get the Sen's slope estimate and p-value given an sf dataframe of
#'   farm locations
#'
#' This function takes an sf dataframe of (assumed) farm coordinates. Then, it
#' extracts the time series of yearly sap day proportions for each farm. Sen's
#' slope is calculated for each time series. The slope estimate and p-value are
#' added as columns to the extracted data frame.
#'
#' @param farms_coords sf dataframe containing farms and their coordinates
#' @param sap_prop terra SpatRaster containing yearly sap day proportion
#'   rasters, probably calculated from [sap_day()]
#' @param elevation one layer terra SpatRaster containing elevation values
#'
#' @return sf dataframe containing the time series of sap day proportions
#'   for each year, the geometry, the Sen's slope estimate, and the Sen's slope
#'   p-value
#'
#' @export
get_sens_farms <- function(farms_coords, sap_prop, elevation = NULL) {
  # Error checking
  sf_helper(farms_coords)
  if (class(sap_prop) != "SpatRaster") {
    stop("sap_prop must have class SpatRaster.")
  }

  # Extract sap day proportions at farm locations
  farms_props <- terra::extract(sap_prop, farms_coords)

  # Add on elevation if provided
  if (!is.null(elevation)) {
    if (class(elevation) != "SpatRaster") {
      stop("elevation must have class SpatRaster.")
    } else if (dim(elevation)[3] != 1) {
      stop("elevation must be a 1 layer raster.")
    }
    elevation_values <- terra::extract(elevation, farms_coords)[, 2]
    farms_props$farm_elevation <- elevation_values
  }

  # Create sf version of farms_props
  farms_sf <- sf::st_sf(farms_props, geometry = farms_coords$geometry) |>
    tidyr::drop_na() |>
    dplyr::select(-ID)

  # Initialize empty vectors
  farms_sf$sens_estimate <- rep(0, nrow(farms_sf))
  farms_sf$sens_p_value <- rep(0, nrow(farms_sf))

  # Conduct Sen's and Mann-Kendall test for every farm
  for (i in seq_len(nrow(farms_sf))) {
    sens_slope <- sens_slope(as.numeric(sf::st_drop_geometry(farms_sf[i, names(sap_prop)])))
    farms_sf$sens_estimate[i] <- sens_slope$estimates
    farms_sf$sens_p_value[i] <- sens_slope$p.value

    mk <- Kendall::MannKendall(as.numeric(sf::st_drop_geometry(farms_sf[i, names(sap_prop)])))
    farms_sf$mk_estimate[i] <- mk$tau
    farms_sf$mk_p_value[i] <- mk$sl
  }

  # Return sf dataframe
  farms_sf
}
