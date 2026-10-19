seed=123456890
nxt=seed
find=as.character(nxt)
find=as.numeric(unlist(strsplit(find,"")))
max=length(find)
middle=ceiling(max/2)
partstart=middle-2
partend=middle+3
part=(partstart:partend)
part=as.numeric(paste(part,collapse=""))
gens=c(part)

for(i in 1:10){
  nxt=part^2
  find=as.character(nxt)
  find=as.numeric(unlist(strsplit(find,"")))
  max=length(find)
  middle=ceiling(max/2)
  partstart=middle-2
  partend=middle+3
  part=(partstart:partend)
  part=as.numeric(paste(part,collapse=""))
  gens=c(gens,part)
}
cat(gens)





###################### Generate n random numbers between the default values
n=10
runif(n)
runif(n)
## Random seed is used to fix random number uniformly
set.seed(2021)
runif(10)
# Test as fix RN
set.seed(2021)
runif(10)
# generate n random numbers between 0 and 25 (with replacement)
sample(0:10, n)
# generate n random numbers between 0 and 25 (without replacement)
sample(0:10, n, replace = FALSE)
# For example, to generate 25 random numbers between the values 0 and 10:
runif(25, min = 0, max = 10)
hist(runif(25, min = 0, max = 10))
plot(runif(1000, min = 0, max = 10))
hist(runif(100, min = 0, max = 10))
hist(runif(1000, min = 0, max = 10))
hist(runif(100000, min = 0, max = 10))
plot(runif(10000, min = 0, max = 10))



##### Random number from Normal distribution
# generates 1 random number
rnorm(1)   
# Genrate 3 random number from normal distritbution with specified mean annd variance
rnorm(3, mean=10, sd=2)    
norm(10, mean = 1, sd = 2)
#> [1] 0.1760327
dnorm(1, mean = -1, sd = 0.5)
#> [1] 0.0002676605
dnorm(0) #standard normal
#> [1] 0.3989423

## For example, you can use the following code to compute the three pdf values in the previous code block.

dnorm(c(0,1,0), mean = c(1, -1, 0), sd= c(2, 0.5, 1))
#> [1] 0.1760326634 0.0002676605 0.3989422804

## for same location
dnorm(0, mean = c(1, -1, 0), sd= c(2, 0.5, 1))
#> [1] 0.1760327 0.1079819 0.3989423 

## for 3 three different location of a distribution
dnorm(c(-3, 2, 5), mean = 3, sd = 2)
#> [1] 0.002215924 0.176032663 0.120985362

### Normal distribution for a sample
library(ggplot2)
x <- seq(from = -5, to = 5, by = 0.05)
norm_dat <- data.frame(x = x, pdf = dnorm(x))
ggplot(norm_dat) + geom_line(aes(x = x, y = pdf))

#################### normal distribution for three sample
x <- seq(from = -5, to = 5, by = 0.05)
norm_dat_1 <- data.frame(dist = "N(0,1)", x = x, pdf = dnorm(x))
norm_dat_2 <- data.frame(dist = "N(1,4)", x = x, pdf = dnorm(x, mean = 1, sd = 2))
norm_dat_3 <- data.frame(dist = "N(-1, 0.25)", x = x, pdf = dnorm(x, mean = -1, sd = 0.5))
norm_dat <- rbind(norm_dat_1, norm_dat_2, norm_dat_3)
ggplot(norm_dat) + geom_line(aes(x = x, y = pdf, color = dist))

#################Cumulative Distribution Function (cdf)
pnorm(0, mean = 1, sd = 2)
#> [1] 0.3085375
pnorm(0) # cdf at 0 of standard normal
#> [1] 0.5

### You can also use pnorm() to visualize the cdf of the standard normal distribution.
q <- seq(from = -5, to = 5, by = 0.1)
norm_dat <- data.frame(q = q, cdf = pnorm(q))
ggplot(norm_dat) + geom_line(aes(x = q, y = cdf))





##### Using function draw graph of sample from uniform distribution
plotunif <- function(x, min = 0, max = 1, lwd = 1, col = 1, ...) {
  
  # Grid of X-axis values
  if (missing(x)) {
    x <- seq(min - 0.5, max + 0.5, 0.01)
  }
  
  if(max < min) {
    stop("'min' must be lower than 'max'")
  }
  
  plot(x, dunif(x, min = min, max = max),
       xlim = c(min - 0.25, max + 0.25), type = "l",
       lty = 0, ylab = "f(x)", ...) 
  segments(min, 1/(max - min), max, 1/(max - min), col = col, lwd = lwd)
  segments(min - 2, 0, min, 0, lwd = lwd, col = col)
  segments(max, 0, max + 2, 0, lwd = lwd, col = col)
  points(min, 1/(max - min), pch = 19, col = col)
  points(max, 1/(max - min), pch = 19, col = col)
  segments(min, 0, min, 1/(max - min), lty = 2, col = col, lwd = lwd)
  segments(max, 0, max, 1/(max - min), lty = 2, col = col, lwd = lwd)
  points(0, min, pch = 21, col = col, bg = "white")
  points(max, min, pch = 21, col = col, bg = "white")
}


plotunif(min = 0, max = 1, lwd = 2, col = 4, main = "Uniform PDF")

library(ggplot2)
x <- seq(from = -5, to = 5, by = 0.05)
norm_dat <- data.frame(x = x, pdf = dnorm(x))
ggplot(norm_dat) + geom_line(aes(x = x, y = pdf))
################ Random Number Generation  Different Method
set.seed(2021)
runif(10)
runif(10)

### Random number generation by LCG
set.seed(100)
lcm <- function(N, x0, a, c, m){
  x <- rep(0,N)
  x[1] <- x0
  for (i in 2:N) x[i] <- (a*x[i-1]+c)%%m
  u <- x/m
  return(u)
}

lcm(N = 8, x0 = 27, a = 17, c = 43, m = 100)
26/64


## Linear congruential generator for different starting values (X0)
set.seed(100)
lcm <- function(N, x0, a, c, m){
  x <- rep(0,N)
  x[1] <- x0
  for (i in 2:N) x[i] <- (a*x[i-1]+c)%%m
  return(x)
}

lcm(N = 100, x0 = 4, a = 13, c = 0, m = 64)

# Uniform random number by LCG
set.seed(100)
lcm <- function(N, x0, a, c, m){
  x <- rep(0,N)
  x[1] <- x0
  for (i in 2:N) x[i] <- (a*x[i-1]+c)%%m
  u=x/m	
  return(u)
  
  lcm(N = 100, x0 = 4, a = 13, c = 0, m = 64)
  4/64
  
  ##### Testing uniformity of RN
  set.seed(2021)
  u <- runif(5000)
  hist(u)
  ks.test(u,"punif")
  
  data=c(.4,.4,.4,.4,.4,.4,.4,.4)
  ks.test(data,"punif")
  
  #### Random number generation by inverse transformation method
  # inverse transform sampling
  num_samples <-  1000
  U           <-  runif(num_samples)
  X           <- -log(1-U)/2
  # plot
  hist(X, freq=F, xlab='X', main='Exponential Random Variable')
  curve(dexp(x, rate=2) , 0, 3, lwd=2, xlab = "", ylab = "", add = T)
  
  
  # set parameters
  a <- 1/2
  b <- 2
  
  # generate 10,000 random uniform variables
  set.seed(1)
  u <- runif(10000)
  
  generated_vals <- (1 - (1 - u)^(1/b) ) ^(1/a)
  hist(generated_vals )
  
  
  #################### inverse transform sampling
  num.samples <-  1000
  U <-  runif(num.samples)
  X <- -log(1-U)/2
  
  # plot
  hist(X, freq=F, xlab='X', main='Generating Exponential R.V.')
  curve(dexp(x, rate=2) , 0, 3, lwd=2, xlab = "", ylab = "", add = T)
  
  
  
  generate_exp <- function(n, lambda) {
    u <- runif(n)
    x <- -1/lambda * log(1 - u)
    return(x)
  }
  
  n_samp <- 1000
  my_samp <- generate_exp(n = n_samp, lambda = 10)
  hist(my_samp, probability = TRUE, n = 25)
  curve(dexp(x, rate = 10), from = 0, to = max(my_samp), col = "red", lty = 2, add = TRUE)
  legend("topright", c("Inverse transform", "R dexp()"), lty = c(1, 2), col = c("black",
                                                                                "red"))
  
  
  
  ################ Acceptance/rejection sampling
  set.seed(2019)                       # for reproducibility
  m = 10^5                             # nr of candidates
  x = runif(m, -1,1)
  y = runif(m, 0, 2/pi)
  s = x[y < (2/pi)*sqrt(1-x^2)]        # accepted candidates
  length(s)
  # nr. accepted
  hist(s, prob=T, col="skyblue2")
  curve((2/pi)*sqrt(1-x^2), -1,1, add=T, col="red")
  
  
  plot(x,y, pch=".", col="maroon")
  points(s,y[y< (2/pi)*sqrt(1-x^2)], pch=".", col="green3")
  
  
  
  rejection <- function(f, M, g, rg,n) {
    naccepts <- 0
    result.sample <- rep(NA, n)
    
    while (naccepts < n) {
      y <- rg(1)
      u <- runif(1)
      
      if ( u <= f(y) / (M*g(y)) ) {
        naccepts <- naccepts + 1
        result.sample[naccepts] = y
      }
    }
    
    result.sample
  }
  
  f <- function(x) 100*(x^3)*(1-x)^2
  g <- function(x) 1
  rg <- runif
  M <- 3.5 #upper bound on f (via basic calculus)
  
  result <- rejection(f, M, g,rg, 10000)
  
  print(length(result[result <= 0.8])/10000)
  
  hist(result,freq = FALSE)
  points(seq(0,1,0.01),dbeta(seq(0,1,0.01),4,3),type = "l")