# 1991 test
download_prism(sp_res = "4km", data = c("tmin", "tmax"),
               start_date = as.Date("1991-01-01"),
               end_date = as.Date("1991-04-30"),
               t_res = "daily",
               out_dir = "Data_Raw/PRISM_Sap_Seasons_Data/1991")

# 2020 test
download_prism(sp_res = "4km", data = c("tmin", "tmax"),
               start_date = as.Date("2020-01-01"),
               end_date = as.Date("2020-04-30"),
               t_res = "daily",
               out_dir = "Data_Raw/PRISM_Sap_Seasons_Data/2020")

# Download the rest of the years
for (year in 1992:2019) {
  my_start_date <- as.Date(paste0(year, "-01-01"))
  my_end_date <- as.Date(paste0(year, "-04-30"))
  download_prism(sp_res = "4km", data = c("tmin", "tmax"),
                 start_date = my_start_date,
                 end_date = my_end_date,
                 t_res = "daily",
                 out_dir = paste0("Data_Raw/PRISM_Sap_Seasons_Data/", year))
}

# Download missing 1981 to 1990 data
for (year in 1981:1990) {
  my_start_date <- as.Date(paste0(year, "-01-01"))
  my_end_date <- as.Date(paste0(year, "-04-30"))
  download_prism(sp_res = "4km", data = c("tmin", "tmax"),
                 start_date = my_start_date,
                 end_date = my_end_date,
                 t_res = "daily",
                 out_dir = paste0("Data_Raw/PRISM_Sap_Seasons_Data/", year))
}

# Download the rest of PRISM
for (year in 1981:2020) {
  my_start_date <- as.Date(paste0(year, "-05-01"))
  my_end_date <- as.Date(paste0(year, "-12-31"))
  download_prism(sp_res = "4km", data = c("tmin", "tmax"),
                 start_date = my_start_date,
                 end_date = my_end_date,
                 t_res = "daily",
                 out_dir = paste0("D:/Data/PRISM/", year))
}
