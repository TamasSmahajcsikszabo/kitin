n_times <- 1000
result <- c()

for (i in seq(1, n_times)) {
  flip <- sample(c("h", "t"), 1)
  result <- c(result, flip)
}
