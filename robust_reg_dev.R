slope_est <- function(x1, x2, y1, y2) {
  slope <- (y2 - y1) / (x2 - x1)
  slope
}

x <- c(34, 49, 49, 44, 66, 48, 49, 39, 54, 57, 39, 65, 43, 43, 44, 42, 71, 40, 41, 38, 42, 77, 40, 38, 43, 42, 36, 55, 57, 57, 41, 66, 69, 38, 49, 51, 45, 141, 133, 76, 44, 40, 56, 50, 75, 44, 181, 45, 61, 15, 23, 42, 61, 146, 144, 89, 71, 83, 49, 43, 68, 57, 60, 56, 63, 136, 49, 57, 64, 43, 71, 38, 74, 84, 75, 64, 48)
y <- c(129, 107, 91, 110, 104, 101, 105, 125, 82, 92, 104, 134, 105, 95, 101, 104, 105, 122, 98, 104, 95, 93, 105, 132, 98, 112, 95, 102, 72, 103, 102, 102, 80, 125, 93, 105, 79, 125, 102, 91, 58, 104, 58, 129, 58, 90, 108, 95, 85, 84, 77, 85, 82, 82, 111, 58, 99, 77, 102, 82, 95, 95, 82, 72, 93, 114, 108, 95, 72, 95, 68, 119, 84, 75, 75, 122, 127)

TS_est <- function(x, y, verbose = FALSE, detailed = FALSE) {
  slopes <- c()
  pairs <- data.frame(matrix(ncol = 6, nrow = length(x) * (length(y) - 1)))
  colnames(pairs) <- c("x1", "x2", "y1", "y2", "route", "inverse_route")
  i <- seq(1, length(x))
  index <- 0
  for (i_chosen in i) {
    for (i_chosen2 in i[-i_chosen]) {
      index <- index + 1
      x1 <- x[i_chosen]
      x2 <- x[i_chosen2]
      y1 <- y[i_chosen]
      y2 <- y[i_chosen2]
      pair <- c(x1, x2, y1, y2, paste0(x1, ";", y1, "->", x2, ";", y2), paste0(x2, ";", y2, "->", x1, ";", y1))
      pairs[index, ] <- pair
    }
  }
  message("Routes mapped")
  pairs <- pairs %>% mutate(index = row_number())
  selected_pairs <- data.frame(matrix(ncol = 6, nrow = (length(x) * (length(y) - 1)) / 2))
  colnames(selected_pairs) <- c("x1", "x2", "y1", "y2", "route", "inverse_route")
  for (i in pairs$index) {
    selected_pair <- pairs[pairs$index == i, ][, -7]
    if (!selected_pair$route %in% selected_pairs$inverse_route) {
      selected_pairs[i, ] <- selected_pair
    }
    selected_pairs <- selected_pairs[!is.na(selected_pairs$route), ]
  }
  message("Pairs estimated")
  selected_pairs <- selected_pairs %>%
    mutate(index = row_number())
  message("Estimating slopes")
  for (i in selected_pairs$index) {
    x1 <- as.numeric(selected_pairs[selected_pairs$index == i, ]$x1)
    x2 <- as.numeric(selected_pairs[selected_pairs$index == i, ]$x2)
    y1 <- as.numeric(selected_pairs[selected_pairs$index == i, ]$y1)
    y2 <- as.numeric(selected_pairs[selected_pairs$index == i, ]$y2)
    slope <- slope_est(x1, x2, y1, y2)
    slopes <- c(slopes, slope)
  }
  M_slope <- median(slopes, na.rm = TRUE)
  M_x <- median(x, na.rm = TRUE)
  M_y <- median(y, na.rm = TRUE)
  intercept <- M_y - M_x * M_slope
  results <- list(
    paste0("Theil-Sen Regression Estimator", "\n"),
    paste0("Intercept ", intercept, "\n"),
    paste0("Slope ", M_slope, "\n"),
    if (detailed) {
      sort(slopes)
    }
  )
  if (verbose) {
    message(results)
  }
  c(intercept, M_slope)
}

TS_est(x, y, verbose = TRUE)
lm(y ~ x)
library(ggplot2)
ggplot(data) +
  geom_point(aes(x, y)) +
  geom_abline(intercept = TS_est(data$x, data$y)[1], slope = TS_est(data$x, data$y)[2])

library(tidyverse)
data <- tribble(
  ~x, ~y,
  0, 56751,
  0.2987, 57037,
  0.4648, 56979,
  0.5762, 57074,
  0.8386, 57422
)
TS_est(data$x, data$y, verbose = TRUE, detailed = TRUE)
