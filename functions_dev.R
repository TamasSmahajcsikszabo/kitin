
winsorize <- function(x, tr = 0.2) {
  if (!is.vector(x)) {
    x <- as_vector(x)
  } else {
    x
  }
  threshold_index <- floor(length(x) * tr)
  x <- sort(x)
  winsorized_value_lower <- x[threshold_index + 1]
  x <- sort(x, decreasing = TRUE)
  winsorized_value_upper <- sort(x, decreasing = TRUE)[threshold_index + 1]
  x_wins <- sort(x)
  for (i in seq(1, threshold_index)) {
    if (x_wins[i] < winsorized_value_lower) {x_wins[i] <- winsorized_value_lower}
  }
  x_wins <- sort(x_wins, decreasing = TRUE)
  for (i in seq(1, threshold_index)) {
    if (x_wins[i] > winsorized_value_upper) {x_wins[i] <- winsorized_value_upper}
  }
  x_wins <- sort(x_wins)
  x_wins
}
trimmed_mean <- function(x, tr = 0.2) {
  tr_index <- seq(1, floor(tr * length(x)))
  x_sorted <- sort(x)
  trimmed <- x_sorted[-tr_index]
  trimmed <- sort(trimmed, decreasing = TRUE)
  trimmed <- trimmed[-tr_index]
  trimmed_mean <- mean(trimmed, na.rm = TRUE)
  trimmed_mean
}

bootstrap_t_trim <- function(x, B = 599) {
  trimmed_mean_total <- trimmed_mean(x)
  bootstrap_t <- numeric()
  for (i in seq(1, B)) {
    bootstrap_sample <- sample(x, replace = TRUE)
    winsorized_SD <- sd(winsorize(bootstrap_sample), na.rm = TRUE)
    trimmed_mean <- trimmed_mean(bootstrap_sample, tr = 0.2)
    t_b <- (trimmed_mean - trimmed_mean_total) / (winsorized_SD / (0.6 * sqrt(length(bootstrap_sample))))
    bootstrap_t[i] <- t_b
  }
  bootstrap_t
}

trim <- function(x, tr = 0.2) {
  tr_index <- seq(1, floor(tr * length(x)))
  x_sorted <- sort(x)
  trimmed <- x_sorted[-tr_index]
  trimmed <- sort(trimmed, decreasing = TRUE)
  trimmed <- trimmed[-tr_index]
  trimmed
}

d_estimate <- function(x, tr = 0.2) {
  n <- length(x)
  h <- length(trim(x))
  var_win <- var(winsorize(x, tr), na.rm = TRUE)
  d <- ((n - 1) * var_win) / (h * (h - 1))
  d
}

W_bootstrap <- function(x1, x2, B = 599, tr = 0.2, seed = TRUE) {
  trim_1 <- trimmed_mean(x1, tr)
  trim_2 <- trimmed_mean(x2, tr)
  if (seed) {
    set.seed(1745)
  }

  W <- numeric()
  for (i in seq(1, B)) {
    bootstrap_sample1 <- sample(x1, replace = TRUE)
    bootstrap_sample2 <- sample(x2, replace = TRUE)
    boot_trim1 <- trimmed_mean(bootstrap_sample1, tr)
    boot_trim2 <- trimmed_mean(bootstrap_sample2, tr)
    d1 <- d_estimate(bootstrap_sample1, tr)
    d2 <- d_estimate(bootstrap_sample2, tr)
    W_est <- ((boot_trim1 - boot_trim2) - (trim_1 - trim_2)) / (sqrt(d1 + d2))
    W[i] <- W_est
  }
  W
}

W_CI_95 <- function(x, g1, g2, tr = 0.2) {
  selected <- c(15, 584)
  bound <- sort(x[selected])
  Xt1 <- trimmed_mean(g1, tr)
  Xt2 <- trimmed_mean(g2, tr)
  d1 <- d_estimate(g1, tr)
  d2 <- d_estimate(g2, tr)
  upper <- (Xt1 - Xt2) - bound[2] * sqrt(d1 + d2)
  lower <- (Xt1 - Xt2) - bound[1] * sqrt(d1 + d2)
  results <- c(lower, upper)
  results
}
onestepm <- function(x) {
  if (is.vector(x)) {
    x <- x
  } else {
    x <- as.vector(x[, 1])
  }
  MADN <- mad(x, na.rm = TRUE) / 0.6745
  M <- median(x, na.rm = TRUE)
  outliers <- c()
  cleaned_x <- c()
  for (i in seq_along(x)) {
    ins <- x[i]
    if ((abs(ins - M) / MADN) > 1.28) {
      outliers[i] <- ins
    } else {
      cleaned_x[i] <- ins
    }
  }
  L <- sum(outliers < M, na.rm = TRUE)
  U <- sum(outliers > M, na.rm = TRUE)
  n <- length(x)
  B <- sum(cleaned_x, na.rm = TRUE)
  OSMest <- (1.28 * MADN * (U - L) + B) / (n - L - U)
  OSMest
}

bootstrap <- function(x, fun, B = 599, trim = TRUE, tr = 0.2, seed = TRUE, percentile = TRUE) {
  catcher <- numeric()
  if (seed) {
    set.seed(1745)
  }
  for (i in seq(1, B)) {
    if (trim) {
      boot_sample <- sample(x, replace = TRUE)
      boot_sample <- trim(boot_sample, tr)
      est <- do.call(fun, list(boot_sample))
      catcher[i] <- est
      catcher
    } else {
      boot_sample <- sample(x, replace = TRUE)
      est <- do.call(fun, list(boot_sample))
      catcher[i] <- est
      catcher
    }
  }
  if (percentile) {
    mean_bootstrap <- mean(catcher)
    sd_boostrap_196 <- sd(catcher) * 1.96
    upper <- mean_bootstrap + sd_boostrap_196
    lower <- mean_bootstrap - sd_boostrap_196
    results <- list("lower CI bound" = lower, "upper CI bound" = upper)
  }

  results <- list("Bootstrap sample" = catcher, "Additional results" = results)
  message("Bootstrap estimate ready")
  print(results)
}

bootstrap2 <- function(x, fun, mode = "estimate", B = 599, trimming = TRUE, tr = 0.2, seed = TRUE, percentile = TRUE) {
  if (seed) {
    set.seed(1745)
  }
  if (mode == "estimate") {
    if (is.list(x)) {
      catcher <- matrix(ncol = length(x), nrow = B)
      for (v in seq(1, length(x))) {
        x_selected <- x[[v]]
        place_holder <- c()
        for (i in seq(1, B)) {
          if (trimming) {
            boot_sample <- sample(x_selected, replace = TRUE)
            boot_sample <- trim(boot_sample, tr)
            est <- do.call(fun, list(boot_sample))
            place_holder[i] <- est
          } else {
            boot_sample <- sample(x_selected, replace = TRUE)
            est <- do.call(fun, list(boot_sample))
            place_holder[i] <- est
          }
        }
        catcher[, v] <- place_holder
      }
      catcher
    } else {
      catcher <- numeric()
      for (i in seq(1, B)) {
        if (trimming) {
          boot_sample <- sample(x, replace = TRUE)
          boot_sample <- trim(boot_sample, tr)
          est <- do.call(fun, list(boot_sample))
          catcher[i] <- est
        } else {
          boot_sample <- sample(x, replace = TRUE)
          est <- do.call(fun, list(boot_sample))
          catcher[i] <- est
        }
      }
      catcher
    }
  } else if (mode == "sample") {
    x_selected <- x[[v]]
    place_holder <- c()
    for (v in seq(1, length(x))) {}
  } else {}
}

wincor <- function(x, y = NULL, tr = .2, t_estimate = TRUE, bootstrap = FALSE, B = 599, seed = FALSE) {
  if (seed) {
    set.seed(1745)
  }
  if (is.list(x)) {
    input <- x
    x <- input[[1]]
    y <- input[[2]]
  }
  n <- length(x)
  h <- n - 2 * g
  indices <- seq(1, n)
  if (bootstrap) {
    T_bootstrap <- c()
    for (index in seq(1, B)) {
      sampled_index <- sample(indices, n, replace = TRUE)
      x_boot <- x[sampled_index]
      y_boot <- y[sampled_index]
      T_boot <- wincor(list(x_boot, y_boot))[[3]]
      T_bootstrap <- c(T_bootstrap, T_boot)
    }
    mean <- mean(T_bootstrap, na.rm = TRUE)
    sd <- sqrt(var(T_bootstrap, na.rm = TRUE))
    upper <- mean + 1.96 * sd
    lower <- mean - 1.96 * sd
    result <- list(lower, upper)
    names(result) <- c("95% Lower bound", "95% Upper bound")
    result
  } else {
    g <- floor(length(x) * tr)
    x_win <- c()
    y_win <- c()
    x_h <- c()
    y_h <- c()
    x_g_L <- sort(x)[g + 1]
    x_g_U <- sort(x, decreasing = TRUE)[g + 1]
    for (v in x) {
      if (v > x_g_U) {
        v <- x_g_U
      } else if (v < x_g_L) {
        v <- x_g_L
      } else {
        v <- v
      }
      x_win <- c(x_win, v)
    }
    y_g_L <- sort(y)[g + 1]
    y_g_U <- sort(y, decreasing = TRUE)[g + 1]
    for (v in y) {
      if (v > y_g_U) {
        v <- y_g_U
      } else if (v < y_g_L) {
        v <- y_g_L
      } else {
        v <- v
      }
      y_win <- c(y_win, v)
    }
    w_cor <- cor(x_win, y_win)
    if (t_estimate) {
      n <- length(x)
      T_w <- w_cor * sqrt((n - 2) / (1 - w_cor^2))
      results <- list(w_cor, h, T_w, h - 2)
      names(results) <- c("Winsorized correlation coefficient", "h estimate", "T estimate", "degrees of freedom")
      results
    } else {
      results <- list(w_cor, h)
      names(results) <- c("Winsorized correlation coefficient", "h estimate")
      results
    }
  }
}

tau_estimate <- function(x, y = NULL) {
  if (is.list(x)) {
    input <- x
    x <- input[[1]]
    y <- input[[2]]
  }

  concordants <- c()
  discordants <- c()
  total <- c(0)

  for (index in seq(1, length(x))) {
    for (pair_index in seq(1, length(x))[!seq(1, length(x)) == index]) {
      first <- list(x[index], y[index])
      second <- list(x[pair_index], y[pair_index])
      if (first[[1]] > second[[1]] & first[[2]] > second[[2]] |
        first[[1]] < second[[1]] & first[[2]] < second[[2]]) {
        concordants <- c(concordants, 1)
      } else {
        discordants <- c(discordants, -1)
      }
      total <- total + 1
    }
  }
  tau <- (sum(concordants) + sum(discordants)) / total
  tau
}

gen_var <- function(x, y) {
  var_x <- var(x, na.rm = TRUE)
  var_y <- var(y, na.rm = TRUE)
  covariance <- sqrt(var_x) * sqrt(var_y) * cor(x, y)
  gen_var <- var_x * var_y - covariance^2
  sqrt(gen_var)
}

smoother <- function(x, y, span = 0.1, fun = "mean") {
  smooth <- c()
  for (i in seq(1, length(x))) {
    selected_x <- x[i]
    len <- floor((length(y) * span))
    lower <- seq(i - len, i)
    upper <- seq(i, i + len)
    selection_range <- seq(min(lower[lower > 0]), max(upper[upper <= length(x)]))
    selected_y <- y[selection_range]
    MOL <- do.call(fun, list(selected_y))
    smooth[i] <- MOL
  }
  smooth
}

explanatory_power <- function(y_hat, y, fun = "var") {
  EP <- do.call(fun, list(y_hat)) / do.call(fun, list(y))
  EP
}

slope_est <- function(x1, x2, y1, y2) {
  slope <- (y2 - y1) / (x2 - x1)
  slope
}
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
      selected_pairs <- matrix(ncol = 6)
      colnames(selected_pairs) <- c("x1", "x2", "y1", "y2", "route", "inverse_route")
      for (i in seq(1, nrow(pairs))) {
        selected_pair <- pairs[i, ]
        if (!selected_pair[5] %in% selected_pairs[6]) {
          selected_pairs <- rbind(selected_pairs, selected_pair)
        }
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

