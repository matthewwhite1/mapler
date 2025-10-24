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


# t_rast <- terra::rast("../loca_sap_weighted.tif")
# t_rast_small <- terra::aggregate(t_rast, 5)
# time1 <- Sys.time()
# test1 <- sens_slope_rast(t_rast, cores = 4)
# time2 <- Sys.time()
# time2 - time1
