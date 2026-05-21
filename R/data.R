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
#'   [ggmap::geocode()].
#'
#' @format ## `farms_coords`
#' A dataframe with 444 rows and 6 columns:
#' \describe{
#'   \item{farm}{Farm name}
#'   \item{address}{Farm address}
#'   \item{state}{U.S. state/Canada province}
#'   \item{region}{U.S. state/Canada province region}
#'   \item{lat}{Latitude}
#'   \item{lon}{Longitude}
#' }
"farms_coords"


#' LOCA2 Model Names
#'
#' LOCA2 model names scraped from the
#'   [LOCA2 server](https://cirrus.ucsd.edu/~pierce/LOCA2/NAmer/).
#'
#' @format ## `loca2_model_names`
#' A character vector with 27 elements.
"loca2_model_names"


#' GHCNd Station Data
#'
#' GHCNd station data pulled from
#'   [ghcnd-stations.txt](https://www.ncei.noaa.gov/pub/data/ghcn/daily/ghcnd-stations.txt).
#'
#' @format ## `ghcnd_stations`
#' A dataframe with 129658 rows and 9 columns:
#' \describe{
#'   \item{ID}{Station identification code}
#'   \item{LATITUDE}{Station latitude in degrees}
#'   \item{LONGITUDE}{Station longitude in degrees}
#'   \item{ELEVATION}{Station elevation in meters}
#'   \item{STATE}{U.S. postal code for the state (for U.S. stations only)}
#'   \item{NAME}{Station name}
#'   \item{GSN_FLAG}{Flag that indicates whether the station is part of
#'     the GCOS Surface Network (GSN). Unique values are blank and "GSN"}
#'   \item{HCN_CRN_FLAG}{Flag that indicates whether the station is part of the
#'     U.S. Historical Climatology Network (HCN) or U.S. Climate Reference
#'     Network (CRN). Unique values are blank, "HCN", and "CRN"}
#'   \item{WMO_ID}{Station World Meteorological Organization (WMO) number}
#' }
"ghcnd_stations"


#' U.S. State Abbreviations
#'
#' @format ## `us_states`
#' A character vector with 50 elements.
"us_states"
