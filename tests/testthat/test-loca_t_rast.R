test_that("loca_t_rast() fails gracefully", {
  expect_error(loca_t_rast(1))
  expect_error(loca_t_rast(c("hehe", "haha")))
  expect_error(loca_t_rast("../", 1))
  suppressWarnings(expect_error(loca_t_rast("../")))
})
