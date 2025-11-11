test_that("trend::sens.slope() matches mapler::sens_slope() for rnorm", {
  skip_on_os("linux")
  set.seed(12345)
  x <- rnorm(1000)
  mapler_sens <- sens_slope(x)
  trend_sens <- trend::sens.slope(x)[names(mapler_sens)]

  for (i in seq_along(mapler_sens)) {
    expect_equal(mapler_sens[[i]], trend_sens[[i]], tolerance = 1E-4)
  }
})

test_that("trend::sens.slope() matches mapler::sens_slope() for raster", {
  skip_on_os("linux")
  test_loca_file <- system.file("extdata", "test_loca_sap_day.tif",
                                package = "mapler")
  test_loca <- terra::rast(test_loca_file)

  mapler_sens <- sens_slope_rast(test_loca, cores = 1)

  trend_sens_helper <- function(t_pixel) {
    if (all(is.na(t_pixel))) {
      return(rep(NA, 6))
    }
    t_pixel_no_na <- t_pixel[!is.na(t_pixel)]
    sens <- trend::sens.slope(t_pixel_no_na)
    c(sens$estimates, sens$statistic, sens$p.value,
      sens$parameter, sens$conf.int[1], sens$conf.int[2])
  }
  trend_sens <- terra::app(test_loca, trend_sens_helper, cores = 1)

  for (i in seq_len(dim(mapler_sens)[3])) {
    mapler_vals <- terra::values(mapler_sens[[i]])
    trend_vals <- terra::values(trend_sens[[i]])
    expect_equal(mapler_vals, trend_vals, tolerance = 1E-3)
  }
})

test_that("sens_slope() is faster than trend::sens.slope()", {
  x <- rnorm(1000)
  my_test <- bench::mark(sens_slope(x), trend::sens.slope(x), check = FALSE)
  expect_true(my_test$median[1] < my_test$median[2])
  expect_true(my_test$mem_alloc[1] < my_test$mem_alloc[2])
})
