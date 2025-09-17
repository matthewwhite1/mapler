#' Download GHCNd files with given ids
#'
#' @param ids Station ids
#' @param out_dir Folder where you want the files to end up
#'
#' @export
download_ghcnd <- function(ids, out_dir = paste0(getwd())) {
  # Download csvs that correspond to given ids
  url <- "https://www.ncei.noaa.gov/data/global-historical-climatology-network-daily/access/"
  for (id in ids) {
    utils::download.file(paste0(url, id, ".csv"), paste0(out_dir, id, ".csv"))
  }
}


#' Get all station ids for a given country
#'
#' @param file_path The file path where your ghcnd-stations.txt is located
#' @param country_code WMO country code
#'
#' @return A character vector of station ids
#'
#' @importFrom dplyr filter select pull
#' @importFrom stringr str_detect
#' @importFrom rlang .data
#'
#' @export
get_country_ids <- function(file_path, country_code) {
  # Read in ids dataframe, assuming it comes from the ghcnd website
  col_widths <- c(11, 74)
  all_ids <- utils::read.fwf(file_path, widths = col_widths)
  colnames(all_ids) <- c("station_id", "rest of stuff")

  # Subset desired ids
  desired_ids <- all_ids |>
    dplyr::filter(stringr::str_detect(.data$station_id, paste0("^", country_code))) |>
    dplyr::select(.data$station_id) |>
    dplyr::pull()

  return(desired_ids)
}
