test_that("prism_t_rast() fails gracefully", {
  expect_error(prism_t_rast("../"))
  expect_error(prism_t_rast(1))
  expect_error(prism_t_rast("../", 1))
  expect_error(prism_t_rast("../", "hehe"))
  expect_error(prism_t_rast(c("hehe", "haha")))
})
