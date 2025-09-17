library(tidyverse)
library(car)
library(glmnet)
library(GGally)

# Read in data
bodyfat <- data.frame(
  triceps = c(
    19.5, 24.7, 30.7, 29.8, 19.1, 25.6, 31.4, 27.9, 22.1, 25.5,
    31.1, 30.4, 18.7, 19.7, 14.6, 29.5, 27.7, 30.2, 22.7, 25.2
  ),
  thigh = c(
    43.1, 49.8, 51.9, 54.3, 42.2, 53.9, 58.5, 52.1, 49.9, 53.5,
    56.6, 56.7, 46.5, 44.2, 42.7, 54.4, 55.3, 58.6, 48.2, 51.0
  ),
  midarm = c(
    29.1, 28.2, 37.0, 31.1, 30.9, 23.7, 27.6, 30.6, 23.2, 24.8,
    30.0, 28.3, 23.0, 28.6, 21.3, 30.1, 25.7, 24.6, 27.1, 27.5
  ),
  body = c(
    11.9, 22.8, 18.7, 20.1, 12.9, 21.7, 27.1, 25.4, 21.3, 19.3,
    25.4, 27.2, 11.7, 17.8, 12.8, 23.9, 22.6, 25.4, 14.8, 21.1
  )
)

# Look at original fit
bodyfat_lm <- lm(body ~ triceps + thigh + midarm, data = bodyfat)
summary(bodyfat_lm)
vif(bodyfat_lm)

# https://www.statology.org/ridge-regression-in-r/
# Try ridge regression as remedial measure for high VIF values
bodyfat_ridge <- glmnet(as.matrix(bodyfat[, 1:3]), bodyfat[, 4], alpha = 0) # alpha = 0 makes it ridge

# Choose an optimal value for lambda using cross validation
bodyfat_ridge_cv <- cv.glmnet(as.matrix(bodyfat[, 1:3]), bodyfat[, 4], alpha = 0, nfolds = 5)
best_lambda <- bodyfat_ridge_cv$lambda.min
plot(bodyfat_ridge_cv)

# Analyze final model
bodyfat_best_ridge <- glmnet(as.matrix(bodyfat[, 1:3]), bodyfat[, 4], alpha = 0, lambda = best_lambda)
coef(bodyfat_best_ridge)
plot(bodyfat_ridge, xvar = "lambda")

################################################################################

# Read in data (not in this repository since it is only accessible by those with a SAS account)
baseball <- read_csv("../baseball.csv")
baseball <- baseball[complete.cases(baseball), ]

# https://stats.stackexchange.com/questions/58531/using-lasso-from-lars-or-glmnet-package-in-r-for-variable-selection
# LASSO variable selection
baseball_lasso <- cv.glmnet(as.matrix(baseball[, 3:23]), baseball$logSalary, alpha = 1) # alpha = 1 makes it LASSO
baseball_lasso
plot(baseball_lasso)
coefs <- coef(baseball_lasso, s = "lambda.min")
inds <- which(coefs != 0)
variables <- row.names(coefs)[inds]
variables <- variables[-1]
variables
baseball_lasso$glmnet.fit$dev.ratio[baseball_lasso$glmnet.fit$lambda == baseball_lasso$lambda.min] # R-squared
baseball_lasso_pred <- predict(baseball_lasso, as.matrix(baseball[, 3:23]), s = "lambda.min")

# Elastic net variable selection
baseball_elastic <- cv.glmnet(as.matrix(baseball[, 3:23]), baseball$logSalary, alpha = 0.5) # alpha = 0.5 makes it elastic
baseball_elastic
plot(baseball_elastic)
coefs_elastic <- coef(baseball_elastic, s = "lambda.min")
inds_elastic <- which(coefs_elastic != 0)
variables_elastic <- row.names(coefs_elastic)[inds]
variables_elastic <- variables_elastic[-1]
variables_elastic # Same variables chosen as with LASSO!
baseball_elastic$glmnet.fit$dev.ratio[baseball_elastic$glmnet.fit$lambda == baseball_elastic$lambda.min] # R-squared
baseball_elastic_pred <- predict(baseball_elastic, as.matrix(baseball[, 3:23]), s = "lambda.min")

# Scatterplot matrix
baseball_df <- data.frame(logSalary = baseball$logSalary,
                          predlasso = as.vector(baseball_lasso_pred),
                          predelastic = as.vector(baseball_elastic_pred))
ggpairs(baseball_df)
