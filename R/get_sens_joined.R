#' Get an sf data frame of maple farms' Sen's slope grouped by shapefile
#'   variable
#'
#' This function takes an sf dataframe probably outputted by [get_sens_farms()].
#' The farms are grouped by variable in the shapefile, and the average of the
#' significance value is taken for each group. A dataframe is returned that
#' contains the significance averages for each group and the corresponding
#' shapefile boundaries - this can be plotted with something like ggplot.
#'
#' @param farms_sf sf dataframe containing the time series of sap day
#'   proportions for each year, the geometry, the Sen's slope estimate,
#'   the Sen's slope p-value, and the significance value,
#'   probably outputted by [get_sens_farms()]
#' @param shapefile sf dataframe containing geographical shape boundaries
#' @param group_var character vector of length 1 containing the name of the
#'   variable within the shapefile dataframe to group by
#' @param sig_var character vector of length 1 containing the name of the
#'   significance column in farms_sf that will be averaged per shapefile region
#'
#' @return sf dataframe that contains the significance averages for each group
#'   and the corresponding shapefile boundaries - this can be plotted with
#'   something like ggplot
#'
#' @examples
#' \dontrun{
#' # Read in farm coordinates and sap day projection
#' farms_sf <- sf::st_as_sf(farms_coords, coords = c("long", "lat"), crs = 4326)
#' test_loca_file <- system.file("extdata", "test_loca_sap_day.tif",
#'                               package = "mapler")
#' sap_prop <- terra::rast(test_loca_file)
#'
#' # Get Sen's slope for every farm
#' farms_sf <- get_sens_farms(farms_sf, sap_prop)
#'
#' # Read in eco regions shape file
#' shapefile <- sf::read_sf("Data_Clean/NA_Eco_Level3/NA_CEC_Eco_Level3.shp")
#' variable <- names(shapefile)[2]
#'
#' # Get mean significance by eco region
#' get_sens_joined(farms_sf, shapefile, variable)
#' }
#' @export
get_sens_joined <- function(farms_sf, shapefile, group_var, sig_var = "sens_significant") {
  # Error checking
  sf_helper(farms_sf)
  if (!any(class(shapefile) == "sf") ||
      !any(class(shapefile) == "data.frame")) {
    stop("shapefile must have both class sf and data.frame.")
  } else if (!is.character(group_var) || length(group_var) != 1) {
    stop("group_var must be a character vector of length 1.")
  } else if (!any(names(shapefile) == group_var)) {
    stop("group_var must be the name of a column in the shapefile.")
  } else if (!is.character(sig_var) || length(sig_var) != 1) {
    stop("sig_var must be a character vector of length 1.")
  } else if (!any(names(farms_sf) == sig_var)) {
    stop("sig_var must be the name of a column in farms_sf.")
  }

  # Join sf objects
  farms_sf <- sf::st_transform(farms_sf, sf::st_crs(shapefile))
  farms_joined <- sf::st_join(farms_sf, shapefile, join = sf::st_within)

  # Calculate mean proportion by region
  farm_sig_mean <- farms_joined |>
    sf::st_drop_geometry() |>
    dplyr::group_by(.data[[group_var]]) |>
    dplyr::summarize(sig_mean = mean(.data[[sig_var]]))
  names(farm_sig_mean) <- stringr::str_remove_all(names(farm_sig_mean), "\"")

  # Rejoin back into shapefile table
  shapefile_joined <- dplyr::right_join(shapefile, farm_sig_mean, by = group_var)

  # Return thing to be plotted
  shapefile_joined
}
