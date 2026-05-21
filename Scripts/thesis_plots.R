library(terra)
library(tidyverse)
library(sf)
library(tidyterra)
library(RColorBrewer)
library(ggmap)
library(osmdata)
library(rnaturalearth)
library(rnaturalearthdata)
library(tigris)
library(ggrepel)

### Raster of Cache Valley ###
# Load in PRISM
prism_mean <- terra::rast("D:/PRISM/prism_tmean_us_25m_2020_avg_30y.tif")

# Set extent of Cache Valley and crop raster
cache <- rast()
ext(cache) <- c(-112.026215, -111.733704, 41.582580, 42.345350)
prism_cache <- terra::crop(prism_mean, cache)
names(prism_cache) <- "mean"

# Create discrete breaks
prism_cache <- prism_cache |>
  mutate(cuts = cut(mean, breaks = 5:9))

# Get Cache Valley Google map
cache_map <- ggmap::get_map("Cache Valley, Utah", source = "google")

# Plot
jpeg("figures/cache_prism.jpg", width = 7, height = 5, units = "in", res = 600)
ggmap(cache_map) +
  geom_spatraster(data = prism_cache, aes(fill = cuts), alpha = 0.6) +
  scale_fill_manual(values = brewer.pal(4, "RdPu"), name = "Mean Temperature") +
  scale_x_continuous("Longtitude", limits = c(-112.05, -111.7),
                     breaks = seq(-112.05, -111.75, by = 0.15)) +
  scale_y_continuous("Latitude", limits = c(41.54, 42.063),
                     breaks = seq(41.55, 42.05, by = 0.1)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid = element_blank(),
        text = element_text(size = 16))
dev.off()


### Historical PRISM Sap Day Example ###
# Load in raster stack and calculate mean
sap_day_prism <- terra::rast("F:/Data/PRISM_sugar_prop.tif")
sap_day_prism_avg <- sap_day_prism |>
  terra::app(fun = mean)

# Get North America map
world <- ne_countries(scale = "medium", returnclass = "sf")
us_states <- ne_states(country = "United States of America", returnclass = "sf")
canada_provinces <- ne_states(country = "Canada", returnclass = "sf")

# Create discrete breaks
sap_day_prism_avg <- sap_day_prism_avg |>
  mutate(cuts = cut(mean, breaks = seq(0, 0.7, by = 0.1), include.lowest = TRUE))

# Plot
jpeg("figures/prism_sap_day.jpg", width = 7, height = 5, units = "in", res = 600)
ggplot() +
  geom_spatraster(data = sap_day_prism_avg, aes(fill = cuts)) +
  scale_fill_manual(values = brewer.pal(7, "GnBu"), name = "Sap Day Proportion" , na.translate = FALSE) +
  geom_sf(data = us_states, fill = NA, color = "darkgray", size = 0.3) +
  coord_sf(xlim = c(-125.5, -66.5), ylim = c(23.875, 50.5), expand = FALSE) +
  theme_minimal() +
  xlab("Longitude") +
  ylab("Latitude") +
  theme(text = element_text(size = 16))
dev.off()


### Rapp Site Locations ###
# Get North America map
world <- ne_countries(scale = "medium", returnclass = "sf")
north_america <- world |>
  filter(region_un == "Americas", name %in% c("United States of America", "Canada"))
us_states <- ne_states(country = "United States of America", returnclass = "sf")
canada_provinces <- ne_states(country = "Canada", returnclass = "sf")

# Get Rapp coordinates
rapp_lats <- c(37.011, 38.231, 41.625, 42.532, 43.734, 48.431)
rapp_lons <- c(-82.676, -79.658, -87.081, -72.190, -72.249, -70.688)
rapp_locations <- terra::vect(data.frame(lon = rapp_lons, lat = rapp_lats))
rapp_sf <- st_as_sf(rapp_locations, coords = c("lon", "lat"), crs = 4326)
st_crs(rapp_sf) <- st_crs(north_america)
sites <- c("Divide Ridge", "Southernmost Maple",
           "Indiana Dunes National Lakeshore", "Harvard Forest",
           "Dartmouth Organic Farm", "Quebec - Northern range")
rapp_sf$name <- sites

# Plot
jpeg("figures/rapp_site_locations.jpg", width = 7, height = 5, units = "in", res = 600)
ggplot() +
  geom_sf(data = north_america, fill = "grey95", color = "black", size = 0.2) +
  geom_sf(data = us_states, fill = NA, color = "darkgray", size = 0.3) +
  geom_sf(data = canada_provinces, fill = NA, color = "darkgray", size = 0.3) +
  geom_sf(data = rapp_sf, color = "orange", size = 2) +
  ggrepel::geom_label_repel(data = rapp_sf,
                            aes(label = name, geometry = geometry),
                            stat = "sf_coordinates",
                            size = 2.5) +
  coord_sf(xlim = c(-90, -60), ylim = c(32, 53), expand = FALSE) +
  theme_minimal() +
  xlab("Longitude") +
  ylab("Latitude") +
  theme(text = element_text(size = 16))
dev.off()


### Rapp Site Projections ###
# Get raster stack
sap_day_access <- terra::rast("F:/Data/LOCA2/sugar_sap_days/ACCESS-CM2_run1_ssp585_prop.tif") |>
  terra::shift(dx = -360)

# Get North America map
world <- ne_countries(scale = "medium", returnclass = "sf")
north_america <- world |>
  filter(region_un == "Americas", name %in% c("United States of America", "Canada"))
us_states <- ne_states(country = "United States of America", returnclass = "sf")
canada_provinces <- ne_states(country = "Canada", returnclass = "sf")

# Get Rapp coordinates
rapp_lats <- c(37.011, 38.231, 41.625, 42.532, 43.734, 48.431)
rapp_lons <- c(-82.676, -79.658, -87.081, -72.190, -72.249, -70.688)
rapp_locations <- terra::vect(data.frame(lon = rapp_lons, lat = rapp_lats))
rapp_sf <- st_as_sf(rapp_locations, coords = c("lon", "lat"), crs = 4326)
st_crs(rapp_sf) <- st_crs(north_america)

# Rapp locations proportion
rapp_props <- terra::extract(sap_day_access, rapp_sf)
sites <- c("Divide Ridge", "Southernmost Maple",
           "Indiana Dunes National Lakeshore", "Harvard Forest",
           "Dartmouth Organic Farm", "Quebec - Northern range")
names(rapp_props)[1] <- "Site"
rapp_props$Site <- sites
rapp_props_df <- rapp_props |>
  pivot_longer(-Site, names_to = "Year", values_to = "Mean") |>
  mutate(Year = as.numeric(Year))
rapp_props_df$Period <- rep(c(rep("Hindcast", 65), rep("Future", 86)), 6)

# Plot
jpeg("figures/rapp_site_projections.jpg", width = 7, height = 5, units = "in", res = 600)
ggplot(rapp_props_df, aes(Year, Mean, color = Period)) +
  geom_line() +
  theme_bw() +
  scale_x_continuous(breaks = seq(1950, 2100, by = 50)) +
  scale_y_continuous("Proportion", breaks = seq(0, 0.3, by = 0.05), limits = c(0, 0.3)) +
  scale_color_manual(values = brewer.pal(3, "PiYG")[c(1, 3)], breaks = c("Hindcast", "Future")) +
  facet_wrap(~ Site) +
  # ggtitle("Proportion of Ideal Sap Days at Different Maple Sites") +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 9.5))
dev.off()


### Plot farms_coords ###
# Read in farm coordinates
farms_sf <- sf::st_as_sf(farms_coords, coords = c("lon", "lat"), crs = 4326)

# Get North America map
world <- ne_countries(scale = "medium", returnclass = "sf")
north_america <- world |>
  filter(region_un == "Americas", name %in% c("United States of America", "Canada"))
us_states <- ne_states(country = "United States of America", returnclass = "sf")
canada_provinces <- ne_states(country = "Canada", returnclass = "sf")

# Plot
jpeg("figures/farms_locations.jpg", width = 7, height = 5, units = "in", res = 600)
ggplot() +
  geom_sf(data = north_america, fill = "grey95", color = "black", size = 0.2) +
  geom_sf(data = us_states, fill = NA, color = "darkgray", size = 0.3) +
  geom_sf(data = canada_provinces, fill = NA, color = "darkgray", size = 0.3) +
  geom_sf(data = farms_sf, color = "black", size = 0.5) +
  coord_sf(xlim = c(-96, -60), ylim = c(35, 52)) +
  scale_fill_manual("Significance Proportion", values = my_palette) +
  theme_minimal() +
  labs(
    x = "Longitude",
    y = "Latitude"
  ) +
  theme(text = element_text(size = 16))
dev.off()


### sens_slope_rast() Example ###
# Load in raster stack
sap_day_access <- terra::rast("F:/Data/LOCA2/sugar_sap_days/ACCESS-CM2_run1_ssp585_prop.tif") |>
  terra::shift(dx = -360)

# Calculate Sen's slope for every pixel
access_sens <- sens_slope_rast(sap_day_access)
access_est_p <- c(access_sens$estimates, access_sens$p.value)

# Create helper function
sens_helper <- function(x) {
  if (any(is.na(x))) {
    return(NA)
  } else if (x[1] >= 0 && x[2] <= 0.05) {
    return(1)
  } else if (x[1] >= 0 && x[2] > 0.05) {
    return(2)
  } else if (x[1] < 0 && x[2] > 0.05) {
    return(3)
  } else {
    return(4)
  }
}

# Calculate categories
sens_categories <- terra::app(access_est_p, sens_helper)
names(sens_categories) <- "significance"

# Get North America map
world <- ne_countries(scale = "medium", returnclass = "sf")
us_states <- ne_states(country = "United States of America", returnclass = "sf")
canada_provinces <- ne_states(country = "Canada", returnclass = "sf")

# Create discrete breaks
sens_categories <- sens_categories |>
  mutate(cuts = factor(significance))

# Plot
jpeg("figures/sens_slope_rast.jpg", width = 7, height = 5, units = "in", res = 600)
ggplot() +
  geom_spatraster(data = sens_categories, aes(fill = cuts)) +
  geom_sf(data = us_states, fill = NA, color = "darkgray", size = 0.3) +
  geom_sf(data = canada_provinces, fill = NA, color = "darkgray", size = 0.3) +
  scale_fill_manual(values = brewer.pal(4, "PuOr"),
                    labels = c("Positive significant",
                               "Positive non-significant",
                               "Negative non-significant",
                               "Negative significant"),
                    na.translate = FALSE,
                    name = "Significance") +
  coord_sf(xlim = c(-125.5, -66.5), ylim = c(23.875, 53.5), expand = FALSE) +
  theme_minimal() +
  xlab("Longitude") +
  ylab("Latitude") +
  theme(text = element_text(size = 16),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.position = "bottom")
dev.off()


### Sen's Farms Ecoregions Example ###
# Load in raster stack
sap_day_access <- terra::rast("F:/Data/LOCA2/skinner_sap_days/ACCESS-CM2_run1_ssp585_prop.tif") |>
  terra::shift(dx = -360)

# Read in farm coordinates
farms_sf <- sf::st_as_sf(farms_coords, coords = c("lon", "lat"), crs = 4326)

# Read in eco regions shape file
shapefile <- sf::read_sf("Data_Clean/NA_Eco_Level3/NA_CEC_Eco_Level3.shp")
variable <- names(shapefile)[2]

# Get proportion of Sen's significance at each eco region
eco_regions_joined <- sens_farms(farms_coords = farms_sf, sap_prop = sap_day_access,
                                 shapefile = shapefile, group_var = variable)

# Remove eco regions with less than 5 farms
eco_regions_joined <- eco_regions_joined |>
  dplyr::filter(n_farms >= 5)

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

# Create discrete breaks
eco_regions_joined <- eco_regions_joined |>
  dplyr::mutate(cuts = cut(sig_mean, c(-1, -0.9, -0.8, -0.6, -0.5, -0.1, 0, 0.3, 0.5), include.lowest = TRUE))

# Define color palette
my_palette <- rev(brewer.pal(8, "PuOr"))[c(1, 2, 3, 4, 6, 7)]

# Plot
jpeg("figures/sens_eco_regions.jpg", width = 7, height = 5, units = "in", res = 600)
ggplot() +
  geom_sf(data = north_america, fill = "grey95", color = "black", size = 0.2) +
  geom_sf(data = us_states, fill = NA, color = "darkgray", size = 0.3) +
  geom_sf(data = canada_provinces, fill = NA, color = "darkgray", size = 0.3) +
  geom_sf(data = eco_regions_joined, aes(fill = cuts)) +
  geom_sf(data = farms_sf, color = "black", size = 0.5) +
  coord_sf(xlim = c(-96, -60), ylim = c(33, 52)) +
  scale_fill_manual("Significance Proportion", values = my_palette) +
  theme_minimal() +
  labs(
    x = "Longitude",
    y = "Latitude"
  ) +
  theme(legend.position = "inside",
        legend.position.inside = c(0.85, 0.22),
        text = element_text(size = 16),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10))
dev.off()


### Sen's Farms Counties Example ###
sap_day_access <- terra::rast("F:/Data/LOCA2/skinner_sap_days/ACCESS-CM2_run1_ssp585_prop.tif") |>
  terra::shift(dx = -360)

# Read in farm coordinates
farms_sf <- sf::st_as_sf(farms_coords, coords = c("lon", "lat"), crs = 4326)

# Get US counties
shapefile <- tigris::counties(cb = TRUE, year = 2020)
variable <- "GEOID"

# Get proportion of Sen's significance at each U.S. county
us_counties_joined <- sens_farms(farms_coords = farms_sf, sap_prop = sap_day_access,
                                 shapefile = shapefile, group_var = variable)

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

# Create discrete breaks
us_counties_joined <- us_counties_joined |>
  dplyr::mutate(cuts = cut(sig_mean, c(-1, -0.8, -0.5, -0.3, -0.1, 0.1, 0.3, 0.7, 1), include.lowest = TRUE))

# Define color palette
my_palette <- rev(brewer.pal(7, "PuOr"))

# Plot
jpeg("figures/sens_us_counties.jpg", width = 7, height = 5, units = "in", res = 600)
ggplot() +
  geom_sf(data = north_america, fill = "grey95", color = "black", size = 0.2) +
  geom_sf(data = us_states, fill = NA, color = "darkgray", size = 0.3) +
  geom_sf(data = canada_provinces, fill = NA, color = "darkgray", size = 0.3) +
  geom_sf(data = us_counties_joined, aes(fill = cuts)) +
  geom_sf(data = farms_sf, color = "black", size = 0.3) +
  coord_sf(xlim = c(-95, -59), ylim = c(35, 50), expand = FALSE) +
  scale_fill_manual("Significance Proportion", values = my_palette) +
  theme_minimal() +
  labs(
    x = "Longitude",
    y = "Latitude"
  ) +
  theme(legend.position = "inside",
        legend.position.inside = c(0.85, 0.29),
        text = element_text(size = 16),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10))
dev.off()
