#' @export
get_us_states <- function() {
  us_states <- sf::read_sf("Data_Clean/US_State_Lines/cb_2018_us_state_500k.shp") |>
    dplyr::filter(!(STUSPS %in% c("PR", "DC", "AS", "VI", "GU", "MP"))) |>
    dplyr::select(STUSPS) |>
    unclass()
  us_states$STUSPS
}
