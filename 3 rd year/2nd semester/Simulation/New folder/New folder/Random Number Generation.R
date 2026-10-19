################ Random Number Generation bf Different Method

set.seed(2021)
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
}
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


###################### Acceptance-Rejection method
#The Problem
#Use the Rejection Method to generate n=10000
#values for a random variable X
#that has the pdf of f(x)=x^2/9, 0<x<3

# Create a density histogram for your generated values
# Superimpose the probability density function of the target distribution to the histogram
# Use the generated values to estimate the mean, standard deviation, and the probability that X<2
# Compare these estimated values with the theoretic ones
# Report the rejection rate

''' The rejection method is a sampling method to deal with hard to sample functions.To deal with the hard function,
we will generate a random numbers from a uniform distribution and accept those that fall within the function and reject those that do not.
Since we are looking at the function from x=0 to x=3
,we must find what the min and max values are for the function in that range.
The function we are looking at is an exponential function, and in this case,
 we will not need to worry about maximums or minimums that are not obvious.'''

f=function(x) (x^2) / 9
x.lower = 0
x.upper = 3
f.min = f(x.lower)
f.max = f(x.upper)
## Here, we find that our minimum f(0)=0 and our maximum f(3)=1

## Since the maximum is 1, that is what we will set our c value to. Our aand b values will match our minimum and maximum x values, respectively.

c = f.max
a = x.lower
b = x.upper
# We will calculate the expected mean, standard deviation, and probability of X<2 for comparison later.

# Find the expected mean
f.mu = integrate(function (x) {x * f(x)},
                 lower = x.lower,
                 upper = x.upper)$value
# Find the expected standard deviation
f.sd = (integrate(function (x) {x^2 * f(x)},
                  lower = x.lower,
                  upper = x.upper)$value - f.mu^2)^(1/2)
# Find the probability X<2
f.p2 = integrate(f,
                 lower = x.lower,
                 upper = 2)$value
###The Work With our parameters set, we can setup the simulation and start to determine our accept and reject numbers.

# Number of simulations
n = 10000
accept = NULL
accept.count = 0
i = 0

for(i in i:n){
  X = runif(1,
            min=a,
            max=b)
  Y = runif(1,
            min=0,
            max=c)
  if (Y <= f(X)){
    accept.count = accept.count+1
    accept[accept.count] = X
  } 
}

### Presenting the Data
hist(accept,
     probability =  TRUE)
curve(f(x),
      add = TRUE)

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




