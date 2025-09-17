library(tidyverse)
library(GGally)
library(car)

# Create data frame
powercells <- read.table(text = "
cycles charge_rate temperature
150  0.6  10
86   1.0  10
49   1.4  10
288  0.6  20
157  1.0  20
131  1.0  20
184  1.0  20
109  1.4  20
279  0.6  30
235  1.0  30
224  1.4  30
", header = TRUE)

# Scatterplot matrix
ggpairs(powercells)

# Define higher-order predictors
powercells <- powercells |>
  mutate(cr_temp = charge_rate * temperature,
         cr2 = charge_rate^2,
         temp2 = temperature^2)

# Check for interaction
powercells_lm <- lm(cycles ~ charge_rate + temperature + cr_temp, data = powercells)
summary(powercells_lm)
vif(powercells_lm)
linearHypothesis(powercells_lm, "cr_temp = 0")

# Check for higher-order predictors
powercells_lm_2 <- lm(cycles ~ charge_rate + temperature + cr_temp + cr2 + temp2, data = powercells)
summary(powercells_lm_2)
vif(powercells_lm_2)
linearHypothesis(powercells_lm_2, c("cr_temp = 0", "cr2 = 0", "temp2 = 0"))

# Basic model
powercells_lm_3 <- lm(cycles ~ charge_rate + temperature, data = powercells)
summary(powercells_lm_3)

# Look at higher-order variables with standardized data
std_powercells <- powercells |>
  mutate(across(everything(), ~ (. - mean(.)) / sd(.) * (1 / sqrt(10))))

std_powercells <- std_powercells |>
  mutate(cr_temp = charge_rate * temperature,
         cr2 = charge_rate^2,
         temp2 = temperature^2)

std_powercells_lm <- lm(cycles ~ charge_rate + temperature + cr_temp, data = std_powercells)
summary(std_powercells_lm)
vif(std_powercells_lm)
linearHypothesis(std_powercells_lm, "cr_temp = 0")

std_powercells_lm_2 <- lm(cycles ~ charge_rate + temperature + cr_temp + cr2 + temp2, data = std_powercells)
summary(std_powercells_lm_2)
vif(std_powercells_lm_2)
linearHypothesis(std_powercells_lm_2, c("cr_temp = 0", "cr2 = 0", "temp2 = 0"))

################################################################################

# Create data frame
insurance <- data.frame(
  months = c(
    17, 26, 21, 30, 22, 0, 12, 19, 4, 16,
    28, 15, 11, 38, 31, 21, 20, 13, 30, 14
  ),
  size = c(
    151, 92, 175, 31, 104, 277, 210, 120, 290, 238,
    164, 272, 295, 68, 85, 224, 166, 305, 124, 246
  ),
  type = c(
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1
  )
)
insurance$type <- as.factor(insurance$type)

# Model with only quantitative predictor
insurance_lm_size <- lm(months ~ size, data = insurance)
summary(insurance_lm_size)
ggplot(insurance, aes(x = size, y = months)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  scale_x_continuous("Size of the Insurance Firm in Millions of Dollars",
                     breaks = seq(25, 300, by = 25), limits = c(25, 305)) +
  ylab("Insurance Innovation Adoption Speed in Months") +
  ggtitle("Insurance Data") +
  theme(plot.title = element_text(hjust = 0.5))

# Model with only qualitative predictor
insurance_lm_type <- lm(months ~ type, data = insurance)
summary(insurance_lm_type)
ggplot(insurance_lm_type, aes(type, months)) +
  geom_boxplot() +
  theme_bw() +
  xlab("Type of Firm: Mutual (0) or Stock(1)") +
  scale_y_continuous("Insurance Innovation Adoption Speed in Months",
                     breaks = seq(0, 40, by = 10), limits = c(0, 40)) +
  ggtitle("Insurance Data") +
  theme(plot.title = element_text(hjust = 0.5))

# Additive model
insurance_lm_full <- lm(months ~ size + type, data = insurance)
summary(insurance_lm_full)

# Define predicted values for each type level by hand and look at fitted lines
insurance <- insurance |>
  mutate(pred0 = 33.87407 - 0.10174 * size,
         pred1 = 33.87407 - 0.10174 * size + 8.05547)
ggplot(insurance, aes(size, months, color = type)) +
  geom_point() +
  geom_line(aes(size, pred0), color = "orange") +
  geom_line(aes(size, pred1), color = "black") +
  theme_bw() +
  scale_x_continuous("Size of the Insurance Firm in Millions of Dollars",
                     breaks = seq(25, 300, by = 25), limits = c(25, 305)) +
  ylab("Insurance Innovation Adoption Speed in Months") +
  scale_color_manual(values = c("orange", "black")) +
  ggtitle("Insurance Data") +
  theme(plot.title = element_text(hjust = 0.5))

# Interaction model
insurance_lm_interaction <- lm(months ~ size * type, data = insurance)
summary(insurance_lm_interaction)

insurance <- insurance |>
  mutate(pred0 = 33.83837 - 0.10153 * size,
         pred1 = 33.83837 - 0.10153 * size + 8.13125 - 0.0041714 * size)
ggplot(insurance, aes(size, months, color = type)) +
  geom_point() +
  geom_line(aes(size, pred0), color = "orange") +
  geom_line(aes(size, pred1), color = "black") +
  theme_bw() +
  scale_x_continuous("Size of the Insurance Firm in Millions of Dollars",
                     breaks = seq(25, 300, by = 25), limits = c(25, 305)) +
  ylab("Insurance Innovation Adoption Speed in Months") +
  scale_color_manual(values = c("orange", "black")) +
  ggtitle("Insurance Data") +
  theme(plot.title = element_text(hjust = 0.5))
