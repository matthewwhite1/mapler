library(tidyverse)
library(rpart)
library(rpart.plot)

# Read in data (not in this repository since it is only accessible by those with a SAS account)
baseball <- read_csv("../baseball.csv")
baseball <- baseball[complete.cases(baseball), ]

# loess
baseball_loess <- loess(logSalary ~ CrAtBat + nBB, data = baseball)
summary(baseball_loess)

# Scatter of actual vs predicted
loess_df <- data.frame(logSalary = baseball$logSalary, predloess = baseball_loess$fitted)
ggplot(loess_df, aes(logSalary, predloess)) +
  geom_point() +
  theme_bw()

# Regression tree
tree_formula <- paste("logSalary ~", paste(colnames(baseball)[3:23], collapse = " + "))
baseball_tree <- rpart(tree_formula, data = baseball)
rpart.plot(baseball_tree)

# Scatter of actual vs predicted
tree_pred <- predict(baseball_tree, baseball[, 3:23])
tree_df <- data.frame(logSalary = baseball$logSalary, predtree = tree_pred)
ggplot(tree_df, aes(logSalary, predtree)) +
  geom_point() +
  theme_bw()
