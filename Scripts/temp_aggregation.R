library(tidyverse)
library(terra)

# List available folders
folders <- list.dirs("F:/Data/LOCA2/")
folders_nopath <- list.dirs("F:/Data/LOCA2/", full.names = FALSE)
my_models <- folders[which(folders_nopath %in% loca2_model_names)]
my_models_nopath <- folders_nopath[which(folders_nopath %in% loca2_model_names)]

# For each model...
for (i in seq_along(my_models)) {
  # List available runs
  runs <- list.files(paste0(my_models[i], "/0p0625deg/"), full.names = TRUE)
  runs_nopath <- list.files(paste0(my_models[i], "/0p0625deg/"))
  run_nums <- stringr::str_extract(runs_nopath, "^.{2}")

  # For each run...
  for (j in seq_along(runs)) {
    # List available scenarios
    scenarios <- list.files(runs[j], full.names = TRUE)
    scenarios <- scenarios[!stringr::str_detect(scenarios, "historical")]
    scenarios_nopath <- list.files(runs[j])
    scenarios_nopath <- scenarios_nopath[!stringr::str_detect(scenarios_nopath, "historical")]

    # For each scenario...
    for (k in seq_along(scenarios)) {
      # Only load and calculate historical once
      if (k == 1) {
        hist_rast <- loca_t_rast(runs[j], scenario = "historical")
        hist_months <- str_remove(time(hist_rast$tmax), "-\\d{2}$")
        hist_months_unique <- unique(hist_months)
        hist_tmax_month_list <- list()
        hist_tmin_month_list <- list()

        # For each month...
        for (l in seq_along(hist_months_unique)) {
          # tmax
          hist_tmax_month <- hist_rast$tmax[[which(hist_months == hist_months_unique[l])]]
          hist_tmax_month_mean <- app(hist_tmax_month, mean)
          hist_tmax_month_list[l] <- hist_tmax_month_mean

          # tmin
          hist_tmin_month <- hist_rast$tmin[[which(hist_months == hist_months_unique[l])]]
          hist_tmin_month_mean <- app(hist_tmin_month, mean)
          hist_tmin_month_list[l] <- hist_tmin_month_mean

          # Print progress
          print(paste0("Done with month ", hist_months_unique[l]))
        }

        hist_monthly_tmax <- rast(hist_tmax_month_list)
        hist_monthly_tmin <- rast(hist_tmin_month_list)
      }

      # Scenario
      scenario_rast <- loca_t_rast(runs[j], scenario = scenarios_nopath[k])
      scenario_months <- str_remove(time(scenario_rast$tmax), "-\\d{2}$")
      scenario_months_unique <- unique(scenario_months)
      scenario_tmax_month_list <- list()
      scenario_tmin_month_list <- list()

      # For each month...
      for (l in seq_along(scenario_months_unique)) {
        # tmax
        scenario_tmax_month <- scenario_rast$tmax[[which(scenario_months == scenario_months_unique[l])]]
        scenario_tmax_month_mean <- app(scenario_tmax_month, mean)
        scenario_tmax_month_list[l] <- scenario_tmax_month_mean

        # tmin
        scenario_tmin_month <- scenario_rast$tmin[[which(scenario_months == scenario_months_unique[l])]]
        scenario_tmin_month_mean <- app(scenario_tmin_month, mean)
        scenario_tmin_month_list[l] <- scenario_tmin_month_mean

        # Print progress
        print(paste0("Done with month ", scenario_months_unique[l]))
      }

      scenario_monthly_tmax <- rast(scenario_tmax_month_list)
      scenario_monthly_tmin <- rast(scenario_tmin_month_list)

      # Combine historical and scenario tmax
      monthly_tmax <- c(hist_monthly_tmax, scenario_monthly_tmax)
      filename_tmax <- paste0("F:/Data/LOCA2/monthly_tmax/", my_models_nopath[i],
                         "_", run_nums[j], "_", scenarios_nopath[k], "_tmax.tif")
      terra::writeRaster(monthly_tmax, filename_tmax)

      # Combine historical and scenario tmin
      monthly_tmin <- c(hist_monthly_tmin, scenario_monthly_tmin)
      filename_tmin <- paste0("F:/Data/LOCA2/monthly_tmin/", my_models_nopath[i],
                              "_", run_nums[j], "_", scenarios_nopath[k], "_tmin.tif")
      terra::writeRaster(monthly_tmin, filename_tmin)
    }
  }

  # Garbage collection
  terra::tmpFiles(current = TRUE, orphan = TRUE, old = TRUE, remove = TRUE)
  gc()
}



my_rast <- loca_t_rast("F:/Data/LOCA2/ACCESS-CM2/0p0625deg/r1i1p1f1/")
my_months <- time(my_rast$tmax) |>
  str_remove("-\\d{2}$") |>
  unique()
my_files_tmax <- list.files("F:/Data/LOCA2/monthly_tmax/", full.names = TRUE)
my_files_tmax_nopath <- list.files("F:/Data/LOCA2/monthly_tmax/")
my_files_tmin <- list.files("F:/Data/LOCA2/monthly_tmin/", full.names = TRUE)
my_files_tmin_nopath <- list.files("F:/Data/LOCA2/monthly_tmin/")
filepath_tmax <- "F:/Data/LOCA2/tmax_temp/"
filepath_tmin <- "F:/Data/LOCA2/tmin_temp/"
for (i in seq_along(my_files_tmax)) {
  my_rast_tmax <- rast(my_files_tmax[i])
  names(my_rast_tmax) <- my_months
  writeRaster(my_rast_tmax, paste0(filepath_tmax, my_files_tmax_nopath[i]), overwrite = TRUE)

  my_rast_tmin <- rast(my_files_tmin[i])
  names(my_rast_tmin) <- my_months
  writeRaster(my_rast_tmin, paste0(filepath_tmin, my_files_tmin_nopath[i]), overwrite = TRUE)

  print(i)
}
