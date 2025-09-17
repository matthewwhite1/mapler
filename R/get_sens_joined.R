#' Get an sf data frame of maple farms' Sen's slope grouped by shapefile variable
#'
#' This function takes an sf dataframe probably outputted by [get_sens_farms()]
#' or [get_sens_significance()].
#' The farms are grouped by variable in the shapefile, and the average of the
#' significance value is taken for each group. A dataframe is returned that
#' contains the significance averages for each group and the corresponding
#' shapefile boundaries - this can be plotted with something like ggplot.
#'
#' @param farms_sf An sf dataframe containing the time series of sap day proportions
#'   for each year, the geometry, the Sen's slope estimate, the Sen's slope
#'   p-value, and the significance problem, probably outputted by [get_sens_significance()].
#' @param shapefile An sf dataframe containing geographical shape boundaries.
#' @param variable A character vector of length 1 containing the name of the
#'   variable within the shapefile dataframe to group by.
#' @param sap_prop A terra SpatRaster containing yearly sap day proportion
#'   rasters.
#'
#' @return An sf dataframe that contains the significance averages for each group
#'   and the corresponding shapefile boundaries - this can be plotted with
#'   something like ggplot.
#'
#' @export
get_sens_joined <- function(farms_sf, shapefile, variable) {
  # Error checking
  sf_helper(farms_sf)
  if (!any(class(shapefile) == "sf") || !any(class(shapefile) == "data.frame")) {
    stop("shapefile must have both class sf and data.frame.")
  } else if (!is.character(variable) || length(variable) != 1) {
    stop("variable must be a character vector of length 1.")
  } else if (!any(names(shapefile) == variable)) {
    stop("variable must be the name of a column in the shapefile.")
  } else if (class(sap_prop) != "SpatRaster") {
    stop("sap_prop must have class SpatRaster.")
  }

  # Join sf objects
  farms_sf <- sf::st_transform(farms_sf, sf::st_crs(shapefile))
  farms_joined <- sf::st_join(farms_sf, shapefile, join = sf::st_within)

  # Calculate mean proportion by region
  farm_sig_mean <- farms_joined |>
    sf::st_drop_geometry() |>
    dplyr::group_by(!!rlang::sym(variable)) |>
    dplyr::summarize(sig_mean = mean(significant))
  names(farm_sig_mean) <- stringr::str_remove_all(names(farm_sig_mean), "\"")

  # Rejoin back into shapefile table
  shapefile_joined <- dplyr::right_join(shapefile, farm_sig_mean, by = variable)

  # Return thing to be plotted
  shapefile_joined
}

# farms_coords <- read_csv("Data_Clean/farms_coords.csv")
# farms_coords <- st_as_sf(farms_coords, coords = c("long", "lat"), crs = 4326)
# shapefile <- read_sf("Data_Clean/NA_Eco_Level3/NA_CEC_Eco_Level3.shp")
# variable <- names(shapefile)[2]
# sap_prop <- terra::rast("../loca_sap_weighted.tif")
# elevation <- terra::rast("Data_Clean/elevation.LOCA_2016-04-02.nc")
#
# farms_coords <- read_csv("Data_Clean/farms_coords.csv")
# farms_coords <- st_as_sf(farms_coords, coords = c("long", "lat"), crs = 4326)
# shapefile <- counties(cb = TRUE, year = 2020)
# variable <- "GEOID"
# sap_prop <- terra::rast("../loca_sap_weighted.tif")
