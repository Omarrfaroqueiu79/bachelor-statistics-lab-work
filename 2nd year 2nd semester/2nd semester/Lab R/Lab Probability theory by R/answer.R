# Chi-square distribution
set.seed(151)
x <- rchisq(500, 5)
#hist(x, prob=TRUE,xlim = c(0,40))
plot( density(x), col='orange',xlim = c(0,35))
curve( dchisq(x, df=10), col='green', add=TRUE)
curve( dchisq(x, df=15), col='red', add=TRUE )

# t distribution
set.seed(151)
x <- rt(500, 15)
#hist(x, prob=TRUE,ylim = c(0,0.45))
plot( density(x), col='orange',ylim = c(0,0.4))
curve(dt(x, 20), col = "green", add=TRUE)
curve(dt(x, 35), col = "red", add = TRUE)

# F distribution
set.seed(80)
x <- rf(500, 5, 7)
#hist(x, prob=TRUE)
plot( density(x), col='orange',xlim = c(0,8) ,ylim = c(0,0.75))
curve(df(x, 5, 17), col = "green", add=TRUE)
curve(df(x, 25, 7), col = "red", add = TRUE)

# Normal and Sampling distribution of mean
set.seed(151)
x <- rnorm(1000, 10, 5)
plot(density(x),col='orange',ylim = c(0,0.8))
curve(dnorm(x, 10, 5/sqrt(100)), col = "green", add=TRUE)

