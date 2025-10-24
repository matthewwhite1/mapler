test_that("trend::sens.slope() matches mapler::sens_slope() for rnorm", {
  set.seed(12345)
  x <- rnorm(1000)
  mapler_sens <- sens_slope(x)
  trend_sens <- trend::sens.slope(x)[names(mapler_sens)]

  for (i in seq_along(mapler_sens)) {
    expect_equal(mapler_sens[[i]], trend_sens[[i]], tolerance = 1E-4)
  }
})

test_that("trend::sens.slope() matches mapler::sens_slope() for raster", {
  test_loca_file <- system.file("extdata", "test_loca_sap_day.tif", package = "mapler")
  test_loca <- terra::rast(test_loca_file)

  mapler_sens <- sens_slope_rast(test_loca, cores = 4)

  trend_sens_helper <- function(t_pixel) {
    if (all(is.na(t_pixel))) {
      return(rep(NA, 6))
    }
    t_pixel_no_na <- t_pixel[!is.na(t_pixel)]
    sens <- trend::sens.slope(t_pixel_no_na)
    c(sens$estimates, sens$statistic, sens$p.value,
      sens$parameter, sens$conf.int[1], sens$conf.int[2])
  }
  trend_sens <- terra::app(test_loca, trend_sens_helper, cores = 4)

  for (i in 1:dim(mapler_sens)[3]) {
    mapler_vals <- terra::values(mapler_sens[[i]])
    trend_vals <- terra::values(trend_sens[[i]])
    expect_equal(mapler_vals, trend_vals, tolerance = 1E-3)
  }
})
