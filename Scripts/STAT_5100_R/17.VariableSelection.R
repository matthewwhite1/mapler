library(tidyverse)
library(forecast)
library(olsrr)

# Read in data
surgical <- read_csv("Data_Clean/surgical.csv")

# Randomly select training and test sets
set.seed(1234)
surgical <- surgical |>
  mutate(U = runif(n())) |>
  arrange(U)
train <- surgical[1:72, ] |>
  select(-U)
test <- surgical[73:108, ] |>
  select(-U)

# Check initial residual assumptions
surgical_lm <- lm(Time ~ bloodclot + prognostic + enzyme + liver, data = train)
summary(surgical_lm)
checkresiduals(surgical_lm)

# Make transformation
train <- train |>
  mutate(logTime = log(Time))

# Compare different models with different combinations of predictors
models <- ols_step_all_possible(surgical_lm)
models_results <- as.data.frame(models$result)

# Same thing for log transformation
surgical_log_lm <- lm(logTime ~ bloodclot + prognostic + enzyme + liver, data = train)
log_models <- ols_step_all_possible(surgical_log_lm)
log_models_results <- as.data.frame(log_models$result) |>
  arrange(-rsquare)

# Best model has all variables. Apply to test set
test <- test |>
  mutate(logTime = log(Time),
         logTimehat = 4.161124 + 0.050300 * bloodclot + 0.013688 * prognostic +
           0.011588 * enzyme + 0.081940 * liver) |>
  mutate(error = (logTime - logTimehat)^2)
mean(test$error)

# MSE ended up being lower than SAS' MSE, just because the splitting
# of the train/test data in SAS had a different random number stream
