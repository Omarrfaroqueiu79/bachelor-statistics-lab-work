# Normal and Sampling distribution of mean
set.seed(1822021)
x <- rnorm(1000, 10, 5)
plot(density(x),col="Blue",ylim = c(0,0.8))
curve(dnorm(x, 10, 5/sqrt(100)), col = "Red", add=TRUE)