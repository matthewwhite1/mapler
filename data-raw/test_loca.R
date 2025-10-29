# Load in rasters
loca_rast <- loca_t_rast("D:/Data/LOCA2/ACCESS-CM2/0p0625deg/r1i1p1f1/")

# Calculate yearly sap days
k_upper <- 2.2 + 273.15
k_lower <- -1.1 + 273.15
loca_sap_day <- sap_day(loca_rast$tmax, loca_rast$tmin,
                        t_upper = k_upper, t_lower = k_lower)

# Shift raster to correct longitude and make pixels bigger
test_loca <- loca_sap_day$proportion |>
  terra::shift(dx = -360) |>
  terra::aggregate(20)

# Write raster to package
terra::writeRaster(test_loca, "inst/extdata/test_loca_sap_day.tif",
                   overwrite = TRUE)
