library(tidyverse)

# Get farms coordinates
farms <- rbind(us_farms, canada_farms, quebec_farms)
farms_coords <- tidygeocoder::geocode(farms, address = address)
farms_coords_valid <- farms_coords |>
  filter(!is.na(lat))
farms_coords_invalid <- farms_coords |>
  filter(is.na(lat))

# Get coordinates of streets for invalid coordinates
farms_coords_invalid_us <- farms_coords_invalid |>
  filter(str_detect(state, "^[A-Z]{2}$")) |>
  mutate(address = str_remove(address, "^\\d+\\s?")) |>
  mutate(address = str_remove(address, ",.+,")) |>
  select(-c(lat, long))
farms_coords_invalid_cn <- farms_coords_invalid |>
  filter(str_detect(state, "^[A-Z]{4}$")) |>
  mutate(address = str_remove(address, "^\\d+,?\\s.+?,")) |>
  select(-c(lat, long))
farms_coords_invalid <- rbind(farms_coords_invalid_us, farms_coords_invalid_cn)
farms_coords_2 <- tidygeocoder::geocode(farms_coords_invalid, address = address)
farms_coords_2 <- farms_coords_2 |>
  filter(!is.na(lat))

# Combine into valid coords
farms_coords <- rbind(farms_coords_valid, farms_coords_2)

usethis::use_data(farms_coords, overwrite = TRUE)
