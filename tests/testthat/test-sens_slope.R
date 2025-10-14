test_that("trend::sens.slope() matches mapler::sens_slope()", {
  set.seed(12345)
  x <- rnorm(1000)
  mapler_sens <- sens_slope(x)
  trend_sens <- trend::sens.slope(x)[names(mapler_sens)]

  for (i in seq_along(mapler_sens)) {
    expect_equal(mapler_sens[[i]], trend_sens[[i]], tolerance = 1E-4)
  }
})
