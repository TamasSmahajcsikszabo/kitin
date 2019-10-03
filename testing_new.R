test <- bootstrap_t_trim(c(2,3,4,5,6))
hist(test)

d_estimate(c(2,3,4,5,5,6,7,8))


x1 <- rnorm(1000, mean = 2)
x2 <- rnorm(1000, mean = 0)
W_bootstrap(x1, x2)


onestepm(x1)
onestepm(x2)

wincor(x1, x2)

tau_estimate(x1, x2)
x <- x1[1:300]
y <- x2[1:300]
tau_estimate(x,y)
xf <- 1
xf2 <- 2
tau_estimate(xf,xf2)
winsorize(x)
