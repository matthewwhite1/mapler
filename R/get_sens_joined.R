#' Get an sf data frame of maple farms' Sen's slope grouped by shapefile
#'   variable
#'
#' This function takes an sf dataframe probably outputted by [get_sens_farms()]
#' or [get_sens_significance()].
#' The farms are grouped by variable in the shapefile, and the average of the
#' significance value is taken for each group. A dataframe is returned that
#' contains the significance averages for each group and the corresponding
#' shapefile boundaries - this can be plotted with something like ggplot.
#'
#' @param farms_sf sf dataframe containing the time series of sap day
#'   proportions for each year, the geometry, the Sen's slope estimate,
#'   the Sen's slope p-value, and the significance value,
#'   probably outputted by [get_sens_significance()]
#' @param shapefile sf dataframe containing geographical shape boundaries
#' @param variable character vector of length 1 containing the name of the
#'   variable within the shapefile dataframe to group by
#'
#' @return sf dataframe that contains the significance averages for each group
#'   and the corresponding shapefile boundaries - this can be plotted with
#'   something like ggplot
#'
#' @export
get_sens_joined <- function(farms_sf, shapefile, variable) {
  # Error checking
  sf_helper(farms_sf)
  if (!any(class(shapefile) == "sf") ||
      !any(class(shapefile) == "data.frame")) {
    stop("shapefile must have both class sf and data.frame.")
  } else if (!is.character(variable) || length(variable) != 1) {
    stop("variable must be a character vector of length 1.")
  } else if (!any(names(shapefile) == variable)) {
    stop("variable must be the name of a column in the shapefile.")
  }

  # Join sf objects
  farms_sf <- sf::st_transform(farms_sf, sf::st_crs(shapefile))
  farms_joined <- sf::st_join(farms_sf, shapefile, join = sf::st_within)

  # Calculate mean proportion by region
  farm_sig_mean <- farms_joined |>
    sf::st_drop_geometry() |>
    dplyr::group_by({{variable}}) |>
    dplyr::summarize(sig_mean = mean(significant))
  names(farm_sig_mean) <- stringr::str_remove_all(names(farm_sig_mean), "\"")

  # Rejoin back into shapefile table
  shapefile_joined <- dplyr::right_join(shapefile, farm_sig_mean, by = variable)

  # Return thing to be plotted
  shapefile_joined
}
