library(tidyverse)

# Get file names
files <- list.files("F:/Data/LOCA2/mean_temp_projections", full.names = TRUE)

# Define stuff
significants <- c()
estimates <- c()
models <- rep(0, length(files))
runs <- rep(0, length(files))
scenarios <- rep(0, length(files))

# For each file...
for (i in seq_along(files)) {
  # Get model, run, and scenario
  model <- stringr::str_extract(files[i], "rast_[^_]*+") |>
    stringr::str_remove("rast_")
  models[i] <- model
  run <- stringr::str_extract(files[i], "run\\d{1}") |>
    stringr::str_remove("run")
  runs[i] <- as.numeric(run)
  scenario <- stringr::str_extract(files[i], "ssp\\d{3}") |>
    stringr::str_remove("ssp")
  scenarios[i] <- as.numeric(scenario)

  # Prepare things for sens functions
  mean_temp_rast <- terra::rast(files[i]) |>
    terra::shift(dx = -360)
  farms_sf <- sf::st_as_sf(farms_coords, coords = c("long", "lat"), crs = "WGS84")

  # Use sens functions
  farms_sf <- get_sens_farms(farms_sf, mean_temp_rast) |>
    get_sens_significance()

  # Save values to significants vector
  significants <- c(significants, farms_sf$significant)
  estimates <- c(estimates, farms_sf$sens_estimate)

  print(i)
}

# Condense estimates vector
my_len <- dim(farms_sf)[1]
new_estimate <- rep(0, length(files))
for (i in seq_len(length(files))) {
  new_estimate[i] <- mean(estimates[(1 + ((i - 1) * my_len)):(my_len * i)])
}

# Create dataframe
mean_temp_df <- data.frame(model = models, scenario = scenarios,
                           run = runs, estimate_mean = new_estimate)

# Explore dataframe
mean_temp_df |>
  arrange(-estimate_mean)

summary(mean_temp_df$estimate_mean)

# Check if scenario estimates are different from each other
ggplot(mean_temp_df, aes(estimate_mean)) +
  geom_histogram(breaks = seq(0.03, 0.08, by = 0.005),
                 color = "black") +
  facet_wrap(~ scenario) +
  scale_y_continuous("Count", breaks = 1:7) +
  theme_bw()

ggplot(mean_temp_df, aes(estimate_mean, color = factor(scenario))) +
  geom_density(lwd = 1) +
  scale_color_brewer(type = "qual", palette = "Dark2") +
  theme_bw()

scenario_aov <- aov(estimate_mean ~ scenario, data = mean_temp_df)
ggplot(as.data.frame(scenario_aov$residuals), aes(sample = scenario_aov$residuals)) +
  stat_qq() +
  stat_qq_line()
shapiro.test(scenario_aov$residuals) # Residuals are not normal, so can't use ANOVA
kruskal.test(estimate_mean ~ scenario, data = mean_temp_df) # Groups are different

# Check if model estimates are different from each other
ggplot(mean_temp_df, aes(estimate_mean, color = model)) +
  geom_density(lwd = 1) +
  scale_color_brewer(type = "qual", palette = "Dark2") +
  theme_bw()

model_aov <- aov(estimate_mean ~ model, data = mean_temp_df)
ggplot(as.data.frame(model_aov$residuals), aes(sample = model_aov$residuals)) +
  stat_qq() +
  stat_qq_line()
shapiro.test(model_aov$residuals) # Residuals are not normal
kruskal.test(estimate_mean ~ model, data = mean_temp_df) # Groups are not different
