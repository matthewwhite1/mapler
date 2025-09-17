################################################################################
# This script finds the maximum temperature differences across the US
# in 2017 using monthly PRISM data.
################################################################################

library(terra)
library(stringr)

# Define empty list and folder name variable
months <- list.files("Data_Raw/PRISM_Data_2017_Monthly/", full.names = TRUE)
diff_list <- vector(mode = "list", length = length(months))

# For each month folder, find the t_diff and add it to the list
for (i in seq_along(months)) {
  files <- list.files(months[i])
  t_max_true <- str_detect(files, "tmax")
  t_max_file <- paste0(months[i], "/", files[t_max_true])
  t_min_file <- paste0(months[i], "/", files[!t_max_true])
  t_diff <- rast(t_max_file) - rast(t_min_file)
  diff_list[i] <- t_diff
}

# Combine all rasters into one raster with 12 layers
# ChatGPT gave me the idea of doing this
diff_raster <- rast(diff_list)

# Create final image
# I read about the app() function in Chapter 4 of the textbook
max_t_diff <- app(diff_raster, max)

# Plot
plot(max_t_diff)
