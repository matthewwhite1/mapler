#' Get all 27 LOCA2 model names
#'
#' @return A character vector of length 27 with all valid model names.
#'
#' @export
get_loca2_model_names <- function() {
  # Define url
  url <- "https://cirrus.ucsd.edu/~pierce/LOCA2/NAmer/"

  # Get model names
  page <- httr::GET(url)
  pagehtml <- XML::htmlParse(page)
  nodes <- XML::getNodeSet(pagehtml, "//table")
  names_table <- XML::readHTMLTable(nodes[[1]])
  names_table[, -1] |>
    dplyr::filter(!is.na(Name) & Name != "Parent Directory") |>
    dplyr::pull(Name) |>
    stringr::str_remove("/$")
}
