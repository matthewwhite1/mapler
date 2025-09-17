library(tidyverse)
library(forecast)

# Create the data frame
plasma <- data.frame(
  age = c(
    0, 0, 0, 0, 0,
    1.0, 1.0, 1.0, 1.0, 1.0,
    2.0, 2.0, 2.0, 2.0, 2.0,
    3.0, 3.0, 3.0, 3.0, 3.0,
    4.0, 4.0, 4.0, 4.0, 4.0
  ),
  level = c(
    13.44, 12.84, 11.91, 20.09, 15.60,
    10.11, 11.38, 10.28, 8.96, 8.59,
    9.83, 9.00, 8.65, 7.85, 8.88,
    7.94, 6.01, 5.14, 6.90, 6.77,
    4.86, 5.10, 5.67, 5.75, 6.23
  )
)

# Simple linear model
plasma_lm <- lm(level ~ age, data = plasma)
summary(plasma_lm)
checkresiduals(plasma_lm)

# Scatterplot
ggplot(plasma, aes(age, level)) +
  geom_point(size = 2.5) +
  geom_smooth(method = "lm") +
  theme_bw() +
  xlab("Age") +
  ylab("Level") +
  ggtitle("Plasma Data") +
  theme(plot.title = element_text(hjust = 0.5))

# Consider transformations
lambda <- BoxCox.lambda(plasma$level, lower = -1, upper = 1)
plasma <- plasma |>
  mutate(log_level = log(level),
         invsqrt_level = -1 / sqrt(level))

# Inverse square root
plasma_invsqrt_lm <- lm(invsqrt_level ~ age, data = plasma)
summary(plasma_invsqrt_lm)
checkresiduals(plasma_invsqrt_lm)

# Log
plasma_log_lm <- lm(log_level ~ age, data = plasma)
summary(plasma_log_lm)
checkresiduals(plasma_log_lm)

# Choose inverse square root, as R-squared is higher and residuals look slightly
# more normal
ggplot(plasma, aes(age, invsqrt_level)) +
  geom_point(size = 2.5) +
  geom_smooth(method = "lm") +
  theme_bw() +
  xlab("Age") +
  scale_y_continuous("Inverse Square Root Level", breaks = seq(-0.45, -0.20, by = 0.05), limits = c(-0.46, -0.20)) +
  ggtitle("Plasma Data") +
  theme(plot.title = element_text(hjust = 0.5))
