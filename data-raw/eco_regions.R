eco_regions <- sf::read_sf("Data_Clean/NA_Eco_Level3/NA_CEC_Eco_Level3.shp")
eco_regions <- sf::st_simplify(eco_regions, dTolerance = 5000)

usethis::use_data(eco_regions, overwrite = TRUE)
