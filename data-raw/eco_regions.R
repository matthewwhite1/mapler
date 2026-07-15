eco_regions <- sf::read_sf("Data_Clean/NA_Eco_Level3/NA_CEC_Eco_Level3.shp")

usethis::use_data(eco_regions, overwrite = TRUE)
