#' Add a significance column to a [get_sens_farms()] dataframe
#'
#' Add a Sen's slope significance column given an sf dataframe
#'   probably outputted by [get_sens_farms()]. Using the previously calculated
#'   Sen's slope already present in the dataframe, a significance variable is
#'   created that is 1 if the slope is significant-positive, -1 if the slope is
#'   negative-significant, and 0 if the slope is not significant.
#'
#' @param farms_sf sf dataframe containing the time series of sap day
#'   proportions for each year, the geometry, the Sen's slope estimate,
#'   and the Sen's slope p-value, probably outputted by [get_sens_farms()]
#'
#' @return the same sf dataframe passed into the function, but with the added
#'   significance column
#'
#' @export
get_sens_significance <- function(farms_sf) {
  # Error checking
  sf_helper(farms_sf)

  # Find sen's slope significance for each location
  farms_sf$significant <- rep(0, nrow(farms_sf))
  for (i in seq_len(nrow(farms_sf))) {
    if (farms_sf$sens_p_value[i] < 0.05) {
      if (farms_sf$sens_estimate[i] < 0) {
        farms_sf$significant[i] <- -1
      } else {
        farms_sf$significant[i] <- 1
      }
    }
  }

  # Return sf dataframe
  farms_sf
}
