library(tidyverse)
library(AER)

# Read in data
geriatric <- read_csv("Data_Clean/Geriatric.csv")

# Poisson regression
geriatric_poisson <- glm(Falls ~ intervention + gender + balance + strength,
                         data = geriatric,
                         family = poisson)
summary(geriatric_poisson)

################################################################################

# Read in data
train <- data.frame(
  Year = c(2003, 2002, 2001, 2000, 1999, 1998, 1997, 1996, 1995, 1994,
           1993, 1992, 1991, 1990, 1989, 1988, 1987, 1986, 1985, 1984,
           1983, 1982, 1981, 1980, 1979, 1978, 1977, 1976, 1975),
  Train_km = c(518, 516, 508, 503, 505, 487, 463, 437, 423, 415,
               424, 430, 439, 431, 436, 443, 397, 414, 418, 389,
               401, 372, 417, 430, 426, 430, 425, 426, 436),
  collisions = c(3, 3, 4, 3, 2, 4, 1, 2, 2, 4,
                 4, 4, 6, 2, 4, 4, 6, 13, 5, 3,
                 7, 3, 2, 2, 3, 4, 8, 12, 2)
)

# Create offset variable
train <- train |>
  mutate(X = Year - 1975,
         logt = log(Train_km))

# Poisson regression with offset
train_poisson <- glm(collisions ~ X, data = train, family = poisson, offset = logt)
summary(train_poisson)

# Check for dispersion
dispersiontest(train_poisson)
