library(Kendall)
library(tidyverse)
library(terra)
library(sf)

farms_coords <- read_csv("Data_Clean/farms_coords.csv")
farms_coords <- st_as_sf(farms_coords, coords = c("long", "lat"), crs = 4326)
sap_prop <- terra::rast("D:/Data/LOCA2/ACCESS-CM2_run1_ssp585_prop.tif") |>
  terra::shift(dx = -360)
farms_sf <- get_sens_farms(farms_coords, sap_prop)

test <- MannKendall(st_drop_geometry(farms_sf[1, 1:151]))
test2 <- SeasonalMannKendall(ts(st_drop_geometry(farms_sf[1, 1:151])))
