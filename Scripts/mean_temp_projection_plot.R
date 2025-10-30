library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(terra)
library(tigris)

# Read in stuff
temp_rast <- terra::rast("../mean_temp_rast_ACCESS-CM2-ssp245.tif") |>
  terra::shift(dx = -360)
farms_coords <- readr::read_csv("Data_Clean/farms_coords.csv")
farms_coords <- sf::st_as_sf(farms_coords, coords = c("long", "lat"), crs = 4326)

# Do sens stuff
farms_sf <- get_sens_farms(farms_coords, temp_rast)
farms_sf <- get_sens_significance(farms_sf)
shapefile <- sf::read_sf("Data_Clean/NA_Eco_Level3/NA_CEC_Eco_Level3.shp")
eco_regions_joined <- get_sens_joined(farms_sf, shapefile, names(shapefile)[2])

# Get North America map
crop_lims <- c(xmin = -99, ymin = 32, xmax = -59, ymax = 53)
world <- ne_countries(scale = "medium", returnclass = "sf")
north_america <- world %>%
  filter(region_un == "Americas", name %in% c("United States of America", "Canada")) |>
  st_crop(crop_lims)
us_states <- ne_states(country = "United States of America", returnclass = "sf") |>
  st_crop(crop_lims)
canada_provinces <- ne_states(country = "Canada", returnclass = "sf") |>
  st_crop(crop_lims)

# Plot
ggplot() +
  geom_sf(data = north_america, fill = "grey95", color = "black", size = 0.2) +
  geom_sf(data = us_states, fill = NA, color = "darkgray", size = 0.3) +
  geom_sf(data = canada_provinces, fill = NA, color = "darkgray", size = 0.3) +
  geom_sf(data = eco_regions_joined, mapping = aes(fill = sig_mean)) +
  geom_sf(data = farms_sf, color = "black", size = 1.5) +
  coord_sf(xlim = c(-99, -59), ylim = c(32, 53), expand = FALSE) +
  scale_fill_viridis_c(name = "Significance Proportion", option = "plasma") +
  theme_minimal() +
  labs(
    # title = "Significance of Sens Slope at Maple Farms",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme(legend.position = "inside",
        legend.position.inside = c(0.9, 0.22))

# Check stuff
temp_vect <- sf::st_drop_geometry(farms_sf)
temp_vect <- as.numeric(temp_vect[100, 1:151])
plot(temp_vect)

# U.S. counties
us_counties <- counties(cb = TRUE, year = 2020)
us_counties_joined <- get_sens_joined(farms_sf, us_counties, "GEOID")
