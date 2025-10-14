#' @export
sens_slope_rast <- function(t_rast) {
  t_rast_slope <- terra::app(t_rast, sens_slope_rast_helper, cores = 4)
  t_rast_slope
}

sens_slope_rast_helper <- function(t_pixel) {
  if (all(is.na(t_pixel))) {
    return(c(NA, NA))
  }
  sens_slope <- sens_slope(t_pixel[!is.na(t_pixel)])
  c(sens_slope$estimates[1], sens_slope$p.value[1])
}


# t_rast <- terra::rast("../loca_sap_weighted.tif")
# t_rast_small <- terra::aggregate(t_rast, 5)
# time1 <- Sys.time()
# test1 <- sens_slope_rast(t_rast)
# time2 <- Sys.time()
# time2 - time1
