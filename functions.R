
winsorize <- function(x, tr=0.2) {
  if (!is.vector(x)) {x <- as_vector(x)} else {x}
  threshold_index <- floor(length(x) * tr)
  x <- sort(x)
  winsorized_value_lower <- x[threshold_index + 1]
  x <- sort(x, decreasing = TRUE)
  winsorized_value_upper <- sort(x, decreasing =  TRUE)[threshold_index + 1]
  x_wins <- sort(x)
  for (i in seq(1, threshold_index)) {
    x_wins[i] <- winsorized_value_lower
  }
  for (i in seq(1, threshold_index)) {
    x_wins <-  sort(x_wins, decreasing = TRUE)
    x_wins[i] <- winsorized_value_upper
  }
  x_wins <- sort(x_wins)
  x_wins
}
trimmed_mean <- function(x, tr = 0.2){
  tr_index <- seq(1,floor(tr * length(x)))
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
  for (i in seq(1,B)) {
    bootstrap_sample <- sample(x, replace = TRUE)
    winsorized_SD <- sd(winsorize(bootstrap_sample), na.rm = TRUE)
    trimmed_mean <- trimmed_mean(bootstrap_sample, tr = 0.2)
    t_b <- (trimmed_mean - trimmed_mean_total) / (winsorized_SD/(0.6 * sqrt(length(bootstrap_sample))))
    bootstrap_t[i] <- t_b
  }
  bootstrap_t
}

trim <- function(x, tr = 0.2) {
  tr_index <- seq(1,floor(tr * length(x)))
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
  d <- ((n-1)*var_win) / (h*(h-1))
  d
}

W_bootstrap <- function(x1, x2, B = 599, tr = 0.2, seed=TRUE) {
  trim_1 <-  trimmed_mean(x1, tr)
  trim_2 <- trimmed_mean(x2, tr)
  if (seed) {set.seed(1745)}
  
  W <- numeric()
  for (i in seq(1,B)) {
    bootstrap_sample1 <- sample(x1, replace = TRUE)
    bootstrap_sample2 <- sample(x2, replace = TRUE)
    boot_trim1 <- trimmed_mean(bootstrap_sample1, tr)
    boot_trim2 <- trimmed_mean(bootstrap_sample2,tr)
    d1 <- d_estimate(bootstrap_sample1,tr)
    d2 <- d_estimate(bootstrap_sample2,tr)
    W_est <- ((boot_trim1 - boot_trim2) - (trim_1 - trim_2)) / (sqrt(d1 + d2))
    W[i] <- W_est
  }
  W
}

W_CI_95 <- function(x, g1, g2, tr = 0.2) {
  selected <- c(15, 584)
  bound <- sort(x[selected])
  Xt1 <- trimmed_mean(g1,tr)
  Xt2 <- trimmed_mean(g2,tr)
  d1 <- d_estimate(g1, tr)
  d2 <- d_estimate(g2, tr)
  upper <- (Xt1 - Xt2) - bound[2] * sqrt(d1 + d2)
  lower <- (Xt1 - Xt2) - bound[1] * sqrt(d1 + d2)
  results <- c(lower, upper)
  results
}
onestepm <- function(x){
  if (is.vector(x)) {x = x} else {x = as.vector(x[,1])}
  MADN <- mad(x, na.rm = TRUE)/0.6745
  M <- median(x, na.rm = TRUE)
  outliers <- c()
  cleaned_x <- c()
  for (i in seq_along(x)) {
    ins <- x[i]
    if ((abs(ins - M) / MADN) > 1.28) {
      outliers[i] <- ins
    } else {cleaned_x[i] <- ins}
  }
  L <- sum(outliers < M, na.rm = TRUE)
  U <- sum(outliers > M, na.rm = TRUE)
  n <- length(x)
  B <- sum(cleaned_x, na.rm = TRUE)
  OSMest <- (1.28 * MADN * (U-L) + B)/(n - L - U)
  OSMest
}

bootstrap <- function(x, fun, B = 599, trim = TRUE, tr = 0.2, seed = TRUE, percentile = TRUE){
  catcher <- numeric()
  if(seed){set.seed(1745)}
  for (i in seq(1,B)){
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
    mean_bootstrap = mean(catcher)
    sd_boostrap_196= sd(catcher) * 1.96
    upper <-  mean_bootstrap + sd_boostrap_196
    lower <-  mean_bootstrap - sd_boostrap_196
    results <- list("lower CI bound" = lower, "upper CI bound" = upper)
  }
  
  results <- list("Bootstrap sample" = catcher, "Additional results" = results)
  message("Bootstrap estimate ready")
  print(results)
}

bootstrap2 <- function(x, fun, B = 599, trimming = TRUE, tr = 0.2, seed = TRUE, percentile = TRUE) {
  if(seed) {set.seed(1745)}
  if (is.list(x)){
    catcher <- matrix(ncol = length(x), nrow = B)
    for (v in seq(1, length(x))) {
    x_selected <- x[[v]]
    place_holder <- c()
      for (i in seq(1,B)){
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
      } catcher[,v] <- place_holder
    } catcher
  } else {
  catcher <- numeric()
  for (i in seq(1,B)) {
    if (trimming) {
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
  }
}


wincor <- function(x, y, tr = .2) {
  g <- floor(length(x) * tr)
  x_win <- c()
  y_win <- c()
  x_g_L <- sort(x)[g + 1]
  x_g_U <- sort(x, decreasing = TRUE)[g + 1]
  for (v in x) {
    if (v > x_g_U) {v = x_g_U
    } else if (v < x_g_L) {v = x_g_L
    } else {v = v} 
    x_win <- c(x_win, v)} 
  y_g_L <- sort(y)[g + 1]
  y_g_U <- sort(y, decreasing = TRUE)[g + 1]
  for (v in y) {
    if (v > y_g_U) {v = y_g_U
    } else if (v < y_g_L) {v = y_g_L
    } else {v = v} 
    y_win <- c(y_win, v)} 
  w_cor <- cor(x_win, y_win)
  w_cor
}

