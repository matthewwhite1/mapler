#' Calculate the Sen's slope for every pixel of a raster
#'
#' @param t_rast terra raster stack
#' @param cores integer amount of cores for parallelization
#'
#' @return terra raster stack with six layers: Sen's slope estimate,
#'   z statistic, p-value, sample size, confidence interval lower bound, and
#'   confidence interval upper bound
#'
#' @export
sens_slope_rast <- function(t_rast, cores = 1) {
  t_rast_slope <- terra::app(t_rast, sens_slope_rast_helper, cores = cores)
  t_rast_slope
}

sens_slope_rast_helper <- function(t_pixel) {
  if (all(is.na(t_pixel))) {
    return(rep(NA, 6))
  }
  t_pixel_no_na <- t_pixel[!is.na(t_pixel)]
  t <- table(t_pixel_no_na)
  sens_slope <- sens_slope_rcpp(t_pixel_no_na, t)
  unlist(sens_slope)
}
