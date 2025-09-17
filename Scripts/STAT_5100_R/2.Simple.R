library(tidyverse)
library(forecast)

# Create the data frame
toluca <- data.frame(
  lotsize = c(80, 30, 50, 90, 70, 60, 120, 80, 100, 50, 40, 70, 90, 20, 110, 100, 30, 50, 90, 110, 30, 90, 40, 80, 70),
  workhours = c(399, 121, 221, 376, 361, 224, 546, 352, 353, 157, 160, 252, 389, 113, 435, 420, 212, 268, 377, 421, 273, 468, 244, 342, 323)
)

# Scatterplot
ggplot(toluca, aes(lotsize, workhours)) +
  geom_point()

# Make a nice scatterplot
ggplot(toluca, aes(lotsize, workhours)) +
  geom_point(size = 3) +
  theme_bw() +
  scale_x_continuous("Lot Size", breaks = seq(10, 130, by = 20), limits = c(10, 130)) +
  scale_y_continuous("Work Hours", breaks = seq(50, 650, by = 100), limits = c(50, 650)) +
  ggtitle("Toluca Company Data") +
  theme(plot.title = element_text(hjust = 0.5))

# Correlation
cor(toluca$lotsize, toluca$workhours)

# Simple linear model
toluca_lm <- lm(workhours ~ lotsize, data = toluca)
summary(toluca_lm)
checkresiduals(toluca_lm)

# Scatterplot with regression line
# Make a nice scatterplot
ggplot(toluca, aes(lotsize, workhours)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm") +
  theme_bw() +
  scale_x_continuous("Lot Size", breaks = seq(10, 130, by = 20), limits = c(10, 130)) +
  scale_y_continuous("Work Hours", breaks = seq(50, 650, by = 100), limits = c(50, 650)) +
  ggtitle("Toluca Company Data") +
  theme(plot.title = element_text(hjust = 0.5))

# Observe predictions
toluca_lm$fitted.values

# Check for lack of fit
toluca_lm_2 <- lm(workhours ~ poly(lotsize, 2), data = toluca)
anova(toluca_lm_2, toluca_lm)
