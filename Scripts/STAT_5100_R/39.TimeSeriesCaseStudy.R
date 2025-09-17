library(tidyverse)
library(trend)
library(forecast)

# Read in data
sales <- data.frame(
  week = 1:161,
  sale = c(
    45.9, 45.4, 42.8, 34.4, 31.9, 36.6, 39.2, 41.4, 40.3, 43.1, 43.2,
    41.2, 38.4, 38.3, 41.9, 37.1, 34.5, 31.3, 30.2, 28.3, 25.9, 26.6,
    26.2, 29,   34.8, 36.8, 37.2, 41.7, 41.2, 40.7, 39.5, 40.4, 38,
    35.6, 33.9, 35.2, 41.8, 42.4, 38.9, 42.1, 41.7, 39.2, 38.5, 42.5,
    47.9, 48.6, 52,   53.5, 53.5, 52.9, 53.4, 52.8, 51.4, 52.5, 52.4,
    51.5, 51.7, 53.3, 55.4, 56.9, 60,   60.8, 62.3, 62.6, 63.1, 62.8,
    64.7, 66.3, 63,   65.5, 70.6, 76,   80.1, 78.6, 78.3, 78.1, 73.6,
    68.8, 64.4, 62.4, 61.1, 63.1, 65.3, 68.3, 72.5, 73.2, 72.9, 70.5,
    69.4, 68.2, 69.3, 72.3, 73.5, 70.3, 68.3, 64.1, 62.5, 62.6, 60.4,
    61.1, 64.7, 65.1, 61.5, 64.2, 67.8, 66.8, 64.1, 66.4, 68,   71,
    76.9, 84.1, 85.9, 85.2, 86.2, 85.7, 81.3, 75.9, 75,   72.5, 69.6,
    67.3, 69.8, 72.2, 75.2, 77.2, 76.8, 72.4, 69.4, 68.7, 65.1, 64.4,
    64.2, 63.2, 62.1, 65.8, 73.7, 77.1, 76,   74.6, 70.6, 67.5, 67.9,
    68.9, 67.8, 65.1, 65,   67.6, 67.9, 66.5, 68.2, 71.7, 71.3, 68.9,
    70,   73.1, 69.1, 67.3, 72.9, 78.6, 82.3
  )
)

# Plot
ggplot(sales, aes(week, sale)) +
  geom_line() +
  theme_bw() +
  xlab("Week") +
  scale_y_continuous("Sales", breaks = seq(20, 90, by = 10), limits = c(20, 90))

# sens slope
sens.slope(sales$sale)

# Make lm
sales_lm <- lm(sale ~ week, data = sales)
summary(sales_lm)
checkresiduals(sales_lm)

# Make the data stationary
sales <- sales |>
  mutate(log_sale = log(sale))
sales_log_lm <- lm(log_sale ~ week, data = sales)
sales_log_lm_df <- data.frame(week = sales$week, residuals = sales_log_lm$residuals)

# Plot stationary data
ggplot(sales_log_lm_df, aes(week, residuals)) +
  geom_line() +
  theme_bw()

# Looks like log transformation wasn't helpful
sales_ts <- ts(sales$sale)
sales_arima <- auto.arima(sales_ts, max.order = 10)
sales_arima # ARIMA(0, 1, 1)
checkresiduals(sales_arima)
