library(tidyverse)

set.seed(12345)
x <- runif(20)
intercept <- 0
slope <- 1
noise <- rnorm(20, mean = 0, sd = 0.3) # Normally distributed error terms
y <- intercept + slope * x + noise
xy_df <- data.frame(x = x, y = y)
jpeg("figures/sens_example_1.jpg", width = 7, height = 5, units = "in", res = 600)
ggplot(xy_df) +
  geom_point(aes(x, y)) +
  theme_bw() +
  theme(text = element_text(size = 16))
dev.off()

x_long <- c()
y_long <- c()
xend_long <- c()
yend_long <- c()
slopes <- c()
intercepts <- c()
count <- 1
for (i in 1:20) {
  xnot <- x[-i]
  ynot <- y[-i]
  for (j in 1:19) {
    x_long[count] <- x[i]
    y_long[count] <- y[i]
    xend_long[count] <- xnot[j]
    yend_long[count] <- ynot[j]
    slopes[count] <- (ynot[j] - y[i]) / (xnot[j] - x[i])
    intercepts[count] <- y[i] - (slopes[count] * x[i])
    count <- count + 1
  }
}
xy_df_long <- data.frame(x = x_long, y = y_long,
                         xend = xend_long, yend = yend_long,
                         slope = slopes, intercept = intercepts)
xy_df_long_sorted <- xy_df_long |>
  arrange(x, xend)

jpeg("figures/sens_example_2.jpg", width = 7, height = 5, units = "in", res = 600)
ggplot() +
  geom_point(data = xy_df, aes(x, y)) +
  theme_bw() +
  geom_segment(data = xy_df_long_sorted[1, ],
               aes(x = x, y = y,
                   xend = xend, yend = yend)) +
  theme(text = element_text(size = 16))
dev.off()

jpeg("figures/sens_example_3.jpg", width = 7, height = 5, units = "in", res = 600)
ggplot() +
  geom_point(data = xy_df, aes(x, y)) +
  theme_bw() +
  geom_segment(data = xy_df_long_sorted[1:19, ],
               aes(x = x, y = y,
                   xend = xend, yend = yend)) +
  theme(text = element_text(size = 16))
dev.off()

jpeg("figures/sens_example_4.jpg", width = 7, height = 5, units = "in", res = 600)
ggplot(xy_df) +
  geom_point(aes(x, y)) +
  theme_bw() +
  geom_segment(data = xy_df_long, aes(x = x, y = y, xend = xend, yend = yend)) +
  theme(text = element_text(size = 16))
dev.off()

jpeg("figures/sens_example_5.jpg", width = 7, height = 5, units = "in", res = 600)
ggplot(xy_df) +
  geom_point(aes(x, y)) +
  theme_bw() +
  geom_segment(data = xy_df_long, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_abline(slope = median(xy_df_long$slope),
              intercept = median(xy_df_long$intercept),
              lwd = 2, color = "red") +
  theme(text = element_text(size = 16))
dev.off()
