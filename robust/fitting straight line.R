library(tidyverse)
library(jsonlite)

data <- tribble(
  ~x, ~y,
  0, 56.751,
  0.2987, 57.037,
  0.4648, 56.979,
  0.5762, 57.074,
  0.8386, 57.422
)

### 1. calculating X distances from the mean of X

mean_added <- data %>%
  mutate(distance = x - mean(x))

### 2. dividing each value of distance by the squared sum of errors produces W (weights)

W_estimated <- mean_added %>%
  mutate(W = distance / sum(distance^2))

### 3. calculating weighted y
W_final <- W_estimated %>%
  mutate(line = y * W)

slope <- W_final %>%
  summarise(slope = sum(line))

W_final %>%
  ggplot() +
  geom_point(aes(x, y)) +
  geom_abline(slope = slope$slope, intercept = 56.751)
