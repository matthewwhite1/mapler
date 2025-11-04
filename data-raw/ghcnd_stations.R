# Read in ids dataframe
col_widths <- c(11, 9, 10, 7, 3, 31, 4, 4, 6)
url <- "https://www.ncei.noaa.gov/pub/data/ghcn/daily/ghcnd-stations.txt"
col_names <- c("ID", "LATITUDE", "LONGITUDE", "ELEVATION", "STATE",
               "NAME", "GSN_FLAG", "HCN_CRN_FLAG", "WMO_ID")
all_ids <- utils::read.fwf(url, widths = col_widths,
                           col.names = col_names, comment.char = "")
ghcnd_stations <- all_ids |>
  dplyr::mutate(STATE = stringr::str_trim(STATE),
                NAME = stringr::str_trim(NAME),
                GSN_FLAG = stringr::str_trim(GSN_FLAG),
                HCN_CRN_FLAG = stringr::str_trim(HCN_CRN_FLAG))

usethis::use_data(ghcnd_stations, overwrite = TRUE)
