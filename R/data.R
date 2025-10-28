#' U.S. Maple Farm Addresses
#'
#' Maple syrup farm addresses scraped from
#'   [maplesyrupfarms.org](https://www.maplesyrupfarms.org/). Since scraping
#'   the farm names was complicated, a few farm names are cut off at the first
#'   hyphen.
#'
#' @format ## `us_farms`
#' A dataframe with 374 rows and 4 columns:
#' \describe{
#'   \item{farm}{Farm name}
#'   \item{address}{Farm address}
#'   \item{state}{U.S. state}
#'   \item{region}{U.S. state region}
#' }
"us_farms"


#' Canada Maple Farm Addresses
#'
#' Maple syrup farm addresses scraped from
#'   [maplesyrupfarms.org](https://www.maplesyrupfarms.org/).
#'
#' @format ## `canada_farms`
#' A dataframe with 15 rows and 4 columns:
#' \describe{
#'   \item{farm}{Farm name}
#'   \item{address}{Farm address}
#'   \item{state}{Province}
#'   \item{region}{Province region}
#' }
"canada_farms"


#' Quebec, Canada Maple Farm Addresses
#'
#' Maple syrup farm addresses scraped from
#'   [bonjourquebec.com](https://www.bonjourquebec.com/en-us/to-see-and-do/delicious-discoveries/sugar-shacks).
#'
#' @format ## `quebec_farms`
#' A dataframe with 85 rows and 4 columns:
#' \describe{
#'   \item{farm}{Farm name}
#'   \item{address}{Farm address}
#'   \item{state}{Province}
#'   \item{region}{Province region}
#' }
"quebec_farms"


#' North America Maple Farm Addresses and Coordinates
#'
#' Maple syrup farm addresses and coordinates scraped from
#'   [maplesyrupfarms.org](https://www.maplesyrupfarms.org/) and
#'   [bonjourquebec.com](https://www.bonjourquebec.com/en-us/to-see-and-do/delicious-discoveries/sugar-shacks).
#'   Since scraping the farm names was complicated, a few farm names are cut
#'   off at the first hyphen. Farm addresses were geocoded with
#'   [tidygeocoder::geocode()].
#'
#' @format ## `farms_coords`
#' A dataframe with 444 rows and 6 columns:
#' \describe{
#'   \item{farm}{Farm name}
#'   \item{address}{Farm address}
#'   \item{state}{U.S. state/Canada province}
#'   \item{region}{U.S. state/Canada province region}
#'   \item{lat}{Latitude}
#'   \item{long}{Longitude}
#' }
"farms_coords"
