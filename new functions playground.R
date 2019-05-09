a <- c(17,15,12,6,2,5,4,9,8,7,5,6,7)
b <- round(a+ rnorm(length(a))*4,0)
c <- a

result4 <- bootstrap2(list(a,b,c), "trimmed_mean")
assign("a",c())
