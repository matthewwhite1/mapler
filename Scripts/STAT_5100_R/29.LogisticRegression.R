library(tidyverse)
library(pROC)
library(car)
library(MASS)

# Read in data
outbreak <- read_csv("Data_Clean/outbreak.csv")

# Run logistic regression
outbreak_logistic <- glm(Disease ~ Age + SES_mid + SES_low + Sector,
                         data = outbreak,
                         family = "binomial")
summary(outbreak_logistic)
roc(outbreak_logistic)

# Create ROC curve
predicted <- predict(outbreak_logistic, outbreak, type = "response")
outbreak_roc <- roc(outbreak$Disease, predicted)
plot(outbreak_roc)

# Make conditional effect plot by comparing predicted disease probabilities
# for sector 1 vs sector 2 at low socioeconomic status as a function of age
c <- coef(outbreak_logistic)
outbreak <- outbreak |>
  mutate(p1 = 1 / (1 + exp(-(c[1] + c[2] * Age + c[4]))),
         p2 = 1 / (1 + exp(-(c[1] + c[2] * Age + c[4] + c[5]))))
ggplot(outbreak) +
  geom_line(aes(Age, p1, color = "Sector 1")) +
  geom_line(aes(Age, p2, color = "Sector 2"), lty = 2) +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 80, by = 20), limits = c(0, 80)) +
  scale_y_continuous("Predicted Probability", breaks = seq(0, 0.8, by = 0.2),
                     limits = c(0, 0.8)) +
  scale_color_manual(values = c("black", "black"), name = "") +
  ggtitle("Conditional Effect Plot at Low SES") +
  theme(plot.title = element_text(hjust = 0.5))

# Check for multicollinearity
outbreak_regular <- lm(Disease ~ Age + SES_mid + SES_low + Sector,
                       data = outbreak)
vif(outbreak_regular)

# Variable selection
outbreak_step <- stepAIC(outbreak_logistic, direction = "backward")
