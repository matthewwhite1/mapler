test_that("sap_day() fails gracefully", {
  test_loca_file <- system.file("extdata", "test_loca_sap_day.tif",
                                package = "mapler")
  test_rast <- terra::rast(test_loca_file)
  terra::time(test_rast) <- as.numeric(names(test_rast))

  expect_error(sap_day(1, test_rast))
  expect_error(sap_day("hehe", test_rast))
  expect_error(sap_day(test_rast, 1))
  expect_error(sap_day(test_rast, "hehe"))
  expect_error(sap_day(1, 1))
  expect_error(sap_day("hehe", "hehe"))
  expect_error(sap_day(test_rast, test_rast, t_upper = "hehe", years = 1950))
  expect_error(sap_day(test_rast, test_rast, t_lower = "hehe", years = 1950))
  expect_error(sap_day(test_rast, test_rast, years = "hehe"))
})
