library(tidyverse)

# Get farms coordinates
farms <- read.csv("Data_Clean/farms.csv")
quebec_farms <- read.csv("Data_Clean/quebec_farms.csv")
farms <- rbind(farms, quebec_farms)
farms_coords <- tidygeocoder::geocode(farms, address = address)
farms_coords_valid <- farms_coords |>
  filter(!is.na(lat))
farms_coords_invalid <- farms_coords |>
  filter(is.na(lat))

# Get coordinates of streets for invalid coordinates
farms_coords_invalid_us <- farms_coords_invalid[1:117, ] |>
  mutate(address = str_remove(address, "^\\d+\\s?")) |>
  mutate(address = str_remove(address, ",.+,")) |>
  select(-c(lat, long))
farms_coords_invalid_cn <- farms_coords_invalid[118:134, ] |>
  mutate(address = str_remove(address, "^\\d+,?\\s.+?,")) |>
  select(-c(lat, long))
farms_coords_invalid <- rbind(farms_coords_invalid_us, farms_coords_invalid_cn)
farms_coords_2 <- tidygeocoder::geocode(farms_coords_invalid, address = address)
farms_coords_2 <- farms_coords_2 |>
  filter(!is.na(lat))

# Combine into valid coords
farms_combined <- rbind(farms_coords_valid, farms_coords_2)

# Write coordinates
write.csv(farms_combined, "Data_Clean/farms_coords.csv")
