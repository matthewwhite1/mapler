library(tidyverse)
library(forecast)

# Create the data frame
toluca <- data.frame(
  lotsize = c(80, 30, 50, 90, 70, 60, 120, 80, 100, 50, 40, 70, 90, 20, 110, 100, 30, 50, 90, 110, 30, 90, 40, 80, 70),
  workhours = c(399, 121, 221, 376, 361, 224, 546, 352, 353, 157, 160, 252, 389, 113, 435, 420, 212, 268, 377, 421, 273, 468, 244, 342, 323)
)

# Create model and parameter confidence intervals
toluca_lm <- lm(workhours ~ lotsize, data = toluca)
toluca_confint <- confint(toluca_lm, level = 0.975)

# Get predictions and standard errors for three lotsizes
dummy <- data.frame(lotsize = c(30, 65, 100))
predictions <- predict.lm(toluca_lm, newdata = dummy, se.fit = TRUE)

# Create 90% simultaneous intervals
alpha = 0.10 # Confidence level
p = 2 # Number of betas
n = 25 # Number of observations
g = 3 # Number of intervals
W = sqrt(p * qf(1 - alpha, p, n - p)) # WH critical value
t = qt(1 - alpha / (2 * g), n - p) # Bonf. critical value
simul_intervals_90 <- data.frame(lotsize = c(30, 65, 100),
                              Yhat = predictions$fit,
                              seYhat = predictions$se.fit)
simul_intervals_90 <- simul_intervals_90 |>
  mutate(WH_lower = Yhat - W * seYhat,
         WH_upper = Yhat + W * seYhat,
         B_lower = Yhat - t * seYhat,
         B_upper = Yhat + t * seYhat)
simul_intervals_90

# Get predictions and standard errors for two more lotsizes
dummy_2 <- data.frame(lotsize = c(80, 100))
predictions_2 <- predict.lm(toluca_lm, newdata = dummy_2, se.fit = TRUE)
sigma_hat <- summary(toluca_lm)$sigma
se_pred <- sqrt(predictions_2$se.fit^2 + sigma_hat^2)

# Create 95% simultaneous intervals
alpha = 0.05
p = 2
n = 25
g = 2
S = sqrt(g * qf(1 - alpha, g, n - p)) # Scheffe critical value
t = qt(1 - alpha / (2 * g), n - p) # Bonf. critical value
simul_intervals_95 <- data.frame(lotsize = c(80, 100),
                                 Yhat = predictions_2$fit,
                                 seYhatnew = se_pred)
simul_intervals_95 <- simul_intervals_95 |>
  mutate(S_lower = Yhat - S * seYhatnew,
         S_upper = Yhat + S * seYhatnew,
         B_lower = Yhat - t * seYhatnew,
         B_upper = Yhat + t * seYhatnew)
simul_intervals_95
