library(ggplot2)
# lognormal distribution sampling
data <- rlnorm(20, 1.65, 2.16)

res <- data.frame(matrix(ncol = 2))
names(res) <- c('means', 'variances')
for (i in seq(1, 500)) {
    sampled <- rlnorm(20, 1.65, 2.16)
    m  <- mean(sampled)
    s <- sqrt(var(sampled))
    res[i, 1] <- m
    res[i, 2] <- s
}

dep_plot <- ggplot(res, aes(means, variances)) +
    geom_point(alpha = 1/5, size = 3) +
    theme_light() +
    labs(
         title = "Dependence of mean and standard deviation on a lognormal curve",
         subtitle = "Indepence would be a random cloud scatter of points"
    )

ggsave("dependency_plot.png", dep_plot, device = "png") 

estimate_T  <- function(m, mu, n, s) {
    T = (m - mu) / (s/sqrt(n))
}
d <- c(1,1,2,6,5,9,1,2,5,2,7,6,3,5,8,9)
v  <- 1
v <- v + 1
estimate_density  <- function(d) {
    total_sample <- length(d)
    unique_values <- unique(d)
    probabilities <- data.frame(matrix(ncol = 2, nrow = unique_values))
    names(probabilities) <- c("v","p")
    i = 0
    for (v in unique_values){
        i = i + 1
        p  <- sum(d==v)/total_sample
        probabilities[i, 1] <- v
        probabilities[i, 2] <- p
    }
    probabilities
}
den <- estimate_density(rnorm(100))
plot(density(d))
den_plot <- ggplot(den, aes(v, p)) +
    geom_line()

ggsave("density_plot.png", den_plot, device = "png") 


d <- rlnorm(10000, 1.65, 2.16)
t_s  <- c()
for (i in seq(1, 1000)){
    d_i  <- sample(d, 20)
    t_i  <- estimate_T(
                       m = mean(d_i),
                       mu = 0,
                       n = length(d_i),
                       s - sqrt(var(d_i))
    )
    t_s  <- c(t_s, t_i)
}

curve_data <- data.frame(normal = rnorm(1000),
                         t_s  = t_s)
curve_plot <- ggplot(curve_data) + 
    geom_density(aes(normal)) +
    geom_density(aes(t_s)) +
    xlim(-50, 50)

ggsave("curve_plot.png", curve_plot, device = "png") 
