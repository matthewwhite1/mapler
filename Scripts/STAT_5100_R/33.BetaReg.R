library(tidyverse)
library(betareg)

# Read in data
reading <- read_csv("Data_Clean/ReadingSkills.csv")

# Look at distribution of response variable
ggplot(reading, aes(accuracy)) +
  geom_histogram(breaks = seq(0.45, 1, by = 0.05), color = "black") +
  theme_bw() +
  scale_x_continuous("Accuracy", breaks = seq(0.45, 1, by = 0.05)) +
  ylab("Count") +
  ggtitle("Accuracy Distribution") +
  theme(plot.title = element_text(hjust = 0.5))

# Beta regression
reading_beta <- betareg(accuracy ~ iq + dyslexia, data = reading)
summary(reading_beta)
