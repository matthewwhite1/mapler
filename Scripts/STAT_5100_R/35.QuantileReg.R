library(tidyverse)
library(quantreg)
library(RColorBrewer)
library(hrqglas)

# Read in data
engel <- read_csv("Data_Clean/engel.csv")
taus <- c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95)
engel_quantile <- rq(income ~ foodexp,
                     tau = taus,
                     data = engel)
summary(engel_quantile)
plot(engel_quantile)

# Plot all fits
fit_df <- data.frame(
  intercept = engel_quantile$coefficients[1, ],
  slope = engel_quantile$coefficients[2, ],
  tau = taus
)
ggplot(engel, aes(foodexp, income)) +
  geom_point() +
  theme_bw() +
  geom_abline(data = fit_df,
              mapping = aes(slope = slope, intercept = intercept, color = factor(tau))) +
  scale_x_continuous("Food Expenditure", breaks = seq(250, 2000, by = 250), limits = c(225, 2059)) +
  scale_y_continuous("Income", breaks = seq(500, 5000, by = 500), limits = c(350, 5000)) +
  scale_color_brewer(palette = "YlOrRd", name = "Quantile") +
  ggtitle("Engel Data Quantile Fits") +
  theme(plot.title = element_text(hjust = 0.5))

################################################################################

# Read in data
baseball <- read_csv("../baseball.csv")
baseball <- baseball[complete.cases(baseball), ]

# Define X matrix
X <- baseball |>
  select(nAtBat, nHits, nHome, nRuns, nRBI, nBB, YrMajor, CrAtBat, CrHits, CrHome,
         CrRuns, CrRbi, CrBB, nOuts, nAssts, nError) |>
  as.matrix()

# Define Y vector
y <- baseball$Salary

# Quantile regression variable selection
groups <- rep(1:4, each = 4)
taus <- c(0.1, 0.5, 0.9)
baseball_quantile <- cv.hrq_glasso(X, y, group.index = groups, tau = 0.1)
plot(baseball_quantile)

