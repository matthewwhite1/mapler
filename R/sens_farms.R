#' Get an sf dataframe with significance proportions by shapefile
#'
#' This is a wrapper function that combines both [get_sens_farms()] and
#'   [get_sens_joined()]. This function takes an sf dataframe of (assumed) farm
#'   coordinates. Then, it extracts the time series of yearly sap day
#'   proportions for each farm. Sen's slope is calculated for each time series.
#'   The slope estimate and p-value are added as columns to the extracted
#'   dataframe. Then, using the previously calculated
#'   Sen's slope already present in the dataframe, a significance variable is
#'   created that is 1 if the slope is significant-positive, -1 if the slope is
#'   negative-significant, and 0 if the slope is not significant.
#'   The farms are grouped by variable in the shapefile, and the average of the
#'   significance value is taken for each group. A dataframe is returned that
#'   contains the significance averages for each group and the corresponding
#'   shapefile boundaries - this can be plotted with something like ggplot.
#'
#' @param farms_coords sf dataframe containing farms and their coordinates
#' @param sap_prop terra SpatRaster containing yearly sap day proportion
#'   rasters, probably calculated from [sap_day()]
#' @param elevation one layer terra SpatRaster containing elevation values
#' @param shapefile sf dataframe containing geographical shape boundaries
#' @param group_var character vector of length 1 containing the name of the
#'   variable within the shapefile dataframe to group by
#' @param sig_var character vector of length 1 containing the name of the
#'   significance column that will be averaged per shapefile region. This will
#'   either be "sens" or "mk"
#'
#' @return sf dataframe that contains the significance averages for each group
#'   and the corresponding shapefile boundaries - this can be plotted with
#'   something like ggplot
#'
#' @examples
#' \dontrun{
#' # Read in farm coordinates and sap day projection
#' farms_sf <- sf::st_as_sf(farms_coords, coords = c("lon", "lat"), crs = 4326)
#' test_loca_file <- system.file("extdata", "test_loca_sap_day.tif",
#'                               package = "mapler")
#' sap_prop <- terra::rast(test_loca_file)
#'
#' # Read in eco regions shape file
#' shapefile <- eco_regions
#' variable <- names(shapefile)[2]
#'
#' # Get proportion of Sen's significance at each eco region
#' eco_regions_joined <- sens_farms(farms_coords = farms_sf, sap_prop = sap_prop,
#'                                  shapefile = shapefile, group_var = variable)
#' }
#' @export
sens_farms <- function(farms_coords, sap_prop, elevation = NULL, shapefile, group_var, sig_var = "sens") {
  # Error checking
  if (!sig_var %in% c("sens", "mk")) {
    stop("sig_var must either be sens or mk")
  }

  get_sens_farms(farms_coords, sap_prop, elevation) |>
    get_sens_joined(shapefile, group_var, paste0(sig_var, "_significant"))
}
