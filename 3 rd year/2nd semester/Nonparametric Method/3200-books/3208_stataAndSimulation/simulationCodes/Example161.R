# Example: 16.1 Let g(x)=4/(1+x^2). Employ Monte Carlo integration to compute the integral of int_0^1 g(x)dx


g <- function(x){
  4/(1+x^2)
}
m <- 100000
x <- runif(m)
mean(g(x))