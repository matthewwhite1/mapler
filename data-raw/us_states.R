us_states <- sf::read_sf("Data_Clean/US_State_Lines/cb_2018_us_state_500k.shp") |>
  sf::st_drop_geometry() |>
  dplyr::filter(!(STUSPS %in% c("PR", "DC", "AS", "VI", "GU", "MP"))) |>
  dplyr::pull(STUSPS)

usethis::use_data(us_states, overwrite = TRUE)
