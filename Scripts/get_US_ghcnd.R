# https://latitudelongitude.org/us/
us_ids <- get_country_ids("Data_Raw/GHCNd_Data/ghcnd-stations.txt", "US")
test_ids <- head(us_ids, 20)
download_ghcnd(test_ids, "Data_Raw/GHCNd_Data/")
