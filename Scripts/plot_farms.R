library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(terra)
library(ggOceanMaps)
library(tigris)

# Read coordinates
farms_coords_valid <- read.csv("Data_Clean/farms_coords.csv")

# Load in sap day raster
loca_sap_prop <- terra::rast("../loca_sap_weighted.tif")

# Extract sap day proportions at farm locations
farms_locations <- terra::vect(data.frame(lon = farms_coords_valid$long, lat = farms_coords_valid$lat))
farms_props <- terra::extract(loca_sap_prop, farms_locations)
elevation <- terra::rast("Data_Clean/elevation.LOCA_2016-04-02.nc")
elevation_values <- terra::extract(elevation, farms_locations)$Elevation
farms_props$elevation <- elevation_values
names(farms_props)[1] <- "Farm"
farms_props$Farm <- farms_coords_valid$farm
farms_props <- farms_props |>
  dplyr::mutate(lon = farms_coords_valid$long, lat = farms_coords_valid$lat) |>
  dplyr::filter(!is.na(`1950`))

# Calculate sens slope for each location
farms_props$significant <- rep(0, nrow(farms_props))
for (i in 1:nrow(farms_props)) {
  slope <- trend::sens.slope(as.numeric(farms_props[i, 2:152]))
  if (slope$p.value < 0.05) {
    if (slope$estimates < 0) {
      farms_props$significant[i] <- -1
    } else {
      farms_props$significant[i] <- 1
    }
  }
}

# (Optional) read from csv
farms_props <- read_csv("Data_Clean/farms_props_sens_average.csv")

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

# Convert farms to sf, get distance to coast
farms_sf <- st_as_sf(farms_props, coords = c("lon", "lat"), crs = 4326)
coastlines <- st_read("Data_Raw/coastlines-split-4326/lines.shp")
farms_sf$d2c <- ggOceanMaps::dist2land(farms_sf, bind = FALSE, shapefile = coastlines)

### Eco regions plot
# Get eco regions
eco_regions <- read_sf("Data_Clean/NA_Eco_Level3/NA_CEC_Eco_Level3.shp")

# Join sf objects
farms_sf <- st_transform(farms_sf, st_crs(eco_regions))
farms_joined <- st_join(farms_sf, eco_regions, join = st_within)

# Calculate mean proportion by region
farm_sig_mean <- farms_joined |>
  st_drop_geometry() |>
  group_by(NA_L3NAME) |>
  summarize(sig_mean = mean(significant),
            elev_mean = mean(elevation))

# Rejoin back into eco regions table
eco_regions_joined <- right_join(eco_regions, farm_sig_mean, by = "NA_L3NAME")

# Ecoregions plot
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

### Heatmap plot
# Make grid version of NA
north_america_grid <- north_america |>
  st_make_grid(n = c(100, 100))
ggplot() +
  geom_sf(data = north_america) +
  geom_sf(data = north_america_grid, alpha = 0.5)
north_america_grid_crop <- st_intersection(north_america, north_america_grid) |>
  st_make_valid() |>
  mutate(grid_id = row_number())
ggplot() +
  geom_sf(data = north_america_grid_crop)

# Join sf objects
farms_sf <- st_transform(farms_sf, st_crs(north_america_grid_crop))
farms_sf_grid <- st_join(north_america_grid_crop, farms_sf)
farms_sig_mean_grid <- farms_sf_grid |>
  st_drop_geometry() |>
  group_by(grid_id) |>
  summarize(sig_mean = mean(significant),
            elev_mean = mean(elevation))
farms_grid_joined <- right_join(north_america_grid_crop, farms_sig_mean_grid, by = "grid_id")
ggplot() +
  geom_sf(data = farms_grid_joined, mapping = aes(fill = sig_mean)) +
  geom_sf(data = us_states, fill = NA, color = "gray90", size = 0.3) +
  geom_sf(data = canada_provinces, fill = NA, color = "gray90", size = 0.3) +
  scale_fill_viridis_c(name = "Significance Proportion", option = "plasma", na.value = "darkgray") +
  theme_minimal() +
  labs(
    # title = "Significance of Sens Slope at Maple Farms",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme(legend.position = "inside",
        legend.position.inside = c(0.9, 0.22))

### Counties plot
# Get counties
us_counties <- counties(cb = TRUE, year = 2020)

# Join sf objects
farms_sf <- st_transform(farms_sf, st_crs(us_counties))
farms_joined <- st_join(farms_sf, us_counties, join = st_within)

# Calculate mean proportion by region
farm_sig_mean <- farms_joined |>
  st_drop_geometry() |>
  group_by(GEOID) |>
  summarize(sig_mean = mean(significant),
            elev_mean = mean(elevation))

# Rejoin back into counties table
us_counties_joined <- right_join(us_counties, farm_sig_mean, by = "GEOID")

# Plot
ggplot() +
  geom_sf(data = north_america, fill = "grey95", color = "black", size = 0.2) +
  geom_sf(data = us_states, fill = NA, color = "darkgray", size = 0.3) +
  geom_sf(data = canada_provinces, fill = NA, color = "darkgray", size = 0.3) +
  geom_sf(data = us_counties_joined, mapping = aes(fill = sig_mean)) +
  geom_sf(data = farms_sf, color = "black", size = 1) +
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
