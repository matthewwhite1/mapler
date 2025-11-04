#' Download GHCNd files with given ids
#'
#' @param ids character vector of GHCNd station ids
#' @param out_dir name of the directory where the files will be downloaded to
#'
#' @examples
#' \dontrun{
#' # Download files for all U.S. stations
#' my_ids <- get_ghcnd_country_ids("US")
#' download_ghcnd(my_ids)
#' }
#' @export
download_ghcnd <- function(ids, out_dir = paste0(getwd())) {
  # Error checking
  if (!is.character(ids)) {
    stop("ids must be a character vector")
  } else if (!is.character(out_dir) || length(out_dir) != 1) {
    stop("out_dir must be a character vector of length 1")
  } else if (!dir.exists(out_dir)) {
    stop("Out directory does not exist.")
  }

  # Download csvs that correspond to given ids
  url <- "https://www.ncei.noaa.gov/data/global-historical-climatology-network-daily/access/"
  for (id in ids) {
    file_name <- paste0(url, id, ".csv")
    if (!RCurl::url.exists(file_name)) {
      warning(paste0("File for id ", id, " does not exist."))
      next
    }
    utils::download.file(file_name, file.path(out_dir, paste0(id, ".csv")))
  }
}


#' Get all GHCNd station ids for a given country
#'
#' @param country_code character vector of WMO country codes
#'
#' @return character vector of station ids
#'
#' @importFrom dplyr filter select pull
#' @importFrom stringr str_detect
#'
#' @examples
#' # Get all U.S. country ids
#' get_ghcnd_country_ids("US")
#' @export
get_ghcnd_country_ids <- function(country_code) {
  # Error checking
  if (!is.character(country_code)) {
    stop("country_code must be a character vector")
  }

  # Set up empty vector of ids
  ids <- c()

  # For each given country code...
  for (code in country_code) {
    # Subset desired ids
    desired_ids <- mapler::ghcnd_stations |>
      dplyr::filter(stringr::str_detect(.data$ID, paste0("^", code))) |>
      dplyr::select("ID") |>
      dplyr::pull()

    # Throw warning and move on if no ids are found
    if (rlang::is_empty(desired_ids)) {
      warning(paste0("No station IDs found for country code ", code))
      next
    }

    # Append ids to overall ids vector
    ids <- c(ids, desired_ids)
  }

  # Return ids
  ids
}
