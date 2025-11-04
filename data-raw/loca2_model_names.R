# Define url
url <- "https://cirrus.ucsd.edu/~pierce/LOCA2/NAmer/"

# Get model names
page <- httr::GET(url)
pagehtml <- XML::htmlParse(page)
nodes <- XML::getNodeSet(pagehtml, "//table")
names_table <- XML::readHTMLTable(nodes[[1]])
loca2_model_names <- names_table[, -1] |>
  dplyr::filter(!is.na(Name) & Name != "Parent Directory") |>
  dplyr::pull(Name) |>
  stringr::str_remove("/$")

usethis::use_data(loca2_model_names, overwrite = TRUE)
