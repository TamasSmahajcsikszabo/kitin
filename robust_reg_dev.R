slope_est <- function(x1, x2, y1, y2) {
  slope <- (y2 - y1) / (x2 - x1)
  slope
}

x <- c(34, 49, 49, 44, 66, 48, 49, 39, 54, 57, 39, 65, 43, 43, 44, 42, 71, 40, 41, 38, 42, 77, 40, 38, 43, 42, 36, 55, 57, 57, 41, 66, 69, 38, 49, 51, 45, 141, 133, 76, 44, 40, 56, 50, 75, 44, 181, 45, 61, 15, 23, 42, 61, 146, 144, 89, 71, 83, 49, 43, 68, 57, 60, 56, 63, 136, 49, 57, 64, 43, 71, 38, 74, 84, 75, 64, 48)
y <- c(129, 107, 91, 110, 104, 101, 105, 125, 82, 92, 104, 134, 105, 95, 101, 104, 105, 122, 98, 104, 95, 93, 105, 132, 98, 112, 95, 102, 72, 103, 102, 102, 80, 125, 93, 105, 79, 125, 102, 91, 58, 104, 58, 129, 58, 90, 108, 95, 85, 84, 77, 85, 82, 82, 111, 58, 99, 77, 102, 82, 95, 95, 82, 72, 93, 114, 108, 95, 72, 95, 68, 119, 84, 75, 75, 122, 127)

TS_est <- function(x, y, verbose = FALSE, detailed = FALSE, confidence = FALSE, B = 599) {
  # Theil-Sen regression estimator
  if (!confidence) {
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

    selected_pairs <- selected_pairs[, c(1:4)]
    X1 <- as.numeric(selected_pairs$x1)
    X2 <- as.numeric(selected_pairs$x2)
    Y1 <- as.numeric(selected_pairs$y1)
    Y2 <- as.numeric(selected_pairs$y2)

    for (i in seq(1, nrow(selected_pairs))) {
      x1 <- X1[i]
      x2 <- X2[i]
      y1 <- Y1[i]
      y2 <- Y2[i]
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
      } else {"Results:"}
    )
    if (verbose) {
      message(results)
    }
    c(intercept, M_slope)
  } else {
    # the bootstrap branch
    bootstrap_results <- matrix(ncol = 2, nrow = B)
    colnames(bootstrap_results) <- c("intercepts", "slopes")
    message("Bootstrapping started")
    message <- '.'
    for (b in seq(1, B)) {
      slopes <- c()
      pairs <- matrix(ncol = 6, nrow = length(x) * (length(y) - 1))
      colnames(pairs) <- c("x1", "x2", "y1", "y2", "route", "inverse_route")
      i <- seq(1, length(x))
      index <- 0
      i_boot <- sample(seq(1, length(x)), replace = TRUE)
      x_boot <- x[i_boot]
      y_boot <- y[i_boot]
      for (i_chosen in i) {
        for (i_chosen2 in i[-i_chosen]) {
          index <- index + 1
          x1 <- x_boot[i_chosen]
          x2 <- x_boot[i_chosen2]
          y1 <- y_boot[i_chosen]
          y2 <- y_boot[i_chosen2]
          pair <- c(x1, x2, y1, y2, paste0(x1, ";", y1, "->", x2, ";", y2), paste0(x2, ";", y2, "->", x1, ";", y1))
          pairs[index, ] <- pair
        }
      }
      selected_pairs <- W(ncol = 6)
      colnames(selected_pairs) <- c("x1", "x2", "y1", "y2", "route", "inverse_route")
      for (i in seq(1, nrow(pairs))) {
        selected_pair <- pairs[i, ]
        if (!selected_pair[5] %in% selected_pairs[6]) {
          selected_pairs <- rbind(selected_pairs, selected_pair)
        }
        selected_pairs <- selected_pairs[!is.na(selected_pairs[5]), ]
      }
      
      selected_pairs <- selected_pairs[!is.na(selected_pairs[,1]), c(1:4)]
      X1 <- as.numeric(selected_pairs[,1])
      X2 <- as.numeric(selected_pairs[,2])
      Y1 <- as.numeric(selected_pairs[,3])
      Y2 <- as.numeric(selected_pairs[,4])

      for (i in seq(1, nrow(selected_pairs))) {
        x1 <- X1[i]
        x2 <- X2[i]
        y1 <- Y1[i]
        y2 <- Y2[i]
        slope <- slope_est(x1, x2, y1, y2)
        slopes <- c(slopes, slope)
      }
      M_slope_boot <- median(slopes, na.rm = TRUE)
      M_x <- median(x_boot, na.rm = TRUE)
      M_y <- median(y_boot, na.rm = TRUE)
      intercept_boot <- M_y - M_x * M_slope_boot
      bootstrap_results[b, 1] <- intercept_boot
      bootstrap_results[b, 2] <- M_slope_boot
      if(length(message) %% 10 == 0) {message <- c(message, b)} else {message <- c(message, '.')}
      cat("\f")
      message(message)
    }
    sd_intercept <- sqrt(sd(bootstrap_results[,1], na.rm = TRUE))
    mean_intercept <- mean(bootstrap_results[,1], na.rm = TRUE)
    intercept_upper <- mean_intercept + 1.96 * sd_intercept
    intercept_lower <- mean_intercept - 1.96 * sd_intercept

    sd_slope <- sqrt(sd(bootstrap_results[,2], na.rm = TRUE))
    mean_slope <- mean(bootstrap_results[,2], na.rm = TRUE)
    slope_upper <- mean_slope + 1.96 * sd_slope
    slope_lower <- mean_slope - 1.96 * sd_slope
    
    results <- list(
      `intercept upper bound` = intercept_upper,
      `intercept lower bound` = intercept_lower,
      `slope upper bound` = slope_upper,
      `slope lower bound` = slope_lower
    )
    message(paste0("95% confidence interval estimates with ", b, " times Bootstrap", "\n"))
    results
  } 
}

TS_est(x, y, verbose = TRUE, confidence = TRUE, B = 11)
lm(y ~ x)
library(ggplot2)
ggplot() +
  geom_point(aes(x, y)) +
  geom_abline(intercept = TS_est(x, y)[1], slope = TS_est(x, y)[2])

library(tidyverse)
data <- tribble(
  ~x, ~y,
  0, 56751,
  0.2987, 57037,
  0.4648, 56979,
  0.5762, 57074,
  0.8386, 57422
)
TS_est(data$x, data$y, verbose = TRUE, detailed = TRUE, confidence = TRUE)
x <- data$x
y <- data$y
