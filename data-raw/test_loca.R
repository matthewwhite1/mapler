test_loca <- terra::rast("../ACCESS-CM2_run1_ssp585_prop.tif") |>
  terra::shift(dx = -360) |>
  terra::aggregate(10)

terra::writeRaster(test_loca, "inst/extdata/test_loca_sap_day.tif", overwrite = TRUE)
