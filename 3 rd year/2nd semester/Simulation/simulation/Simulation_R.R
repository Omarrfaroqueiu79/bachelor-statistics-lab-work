##### Link for Monte-Carlo integration

# generate n random numbers between the default values of 0 and 1
n=25
runif(n)
# generate n random numbers between 0 and 25
runif(n, min = 0, max = 25)
# generate n random numbers between 0 and 25 (with replacement)
sample(0:25, n, replace = TRUE)
# generate n random numbers between 0 and 25 (without replacement)
sample(0:25, n, replace = FALSE)

# generate n random numbers between 0 and 25 (without replacement)
sample(0:25, , replace = FALSE)

# For example, to generate 25 random numbers between the values 0 and 10:
runif(25, min = 0, max = 10)
hist(runif(25, min = 0, max = 10))
plot(runif(100, min = 0, max = 10))
hist(runif(100, min = 0, max = 10))
hist(runif(1000, min = 0, max = 10))
hist(runif(100000, min = 0, max = 10))


##### Random number from Normal distribution
norm(0, mean = 1, sd = 2)
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



### https://rpubs.com/Koba/Monte-Carlo-Basic-Example
#### Simple example of Monte Carlo Simulation in R
'''# We have a stock with a Gaussian (normal) rate of return. The mean rate of return is 9% and standard deviation is 12%.
 This basically means that our stock will have a normal distribution of rate of return centered at 9. Function rnorm will
 generate values around 9 with standard deviation of 12 that are spread out like a normal distribution.'''

set.seed(1729)
# Generate 100 values
returns = rnorm(1:100, mean = 9, sd = 12)
# plot a histogram
hist(returns)
sample(returns,size = 4)

"""You can see that the returns are different this time. So which scenario should we anticipate. Why dont we consider a large number of scenarios.
 We can estimate the rate of return by sampling from the returns vector a large number of times say 10000 and then taking the mean for each year.
 This is a Monte Carlo simulation. Monte Carlo Simulation is repeated random sampling to obtain numerical results (from wikipeadia)."""

X = matrix(ncol = 4,nrow = 10000)
for(year in 1:4) {
# for each year sample the return 10000 times
 for(i in 1:10000){
  X[i,year] = sample(returns,1)
 }
}

# show first 6 rows
head(X)

# Mean expected rate of return
apply(X,2,mean)












######## Normal Distribution Numbers
# The normal (or Gaussian) distribution is the most common and well know distribution.
# Within R, the normal distribution functions are written as ????.
# generate n random numbers from a normal distribution with given mean & st. dev.
rnorm(n, mean = 0, sd = 1)

# generate CDF probabilities for value(s) in vector q
pnorm(q, mean = 0, sd = 1)
# generate quantile for probabilities in vector p
qnorm(.99, mean = 0, sd = 1)
# generate density function probabilites for value(s) in vector x
dnorm(x, mean = 0, sd = 1)
# For example, to generate 25 random numbers from a normal distribution with mean =
# 100 and standard deviation = 15 :
x <- rnorm(25, mean = 100, sd = 15,col=3)

### You can also pass a vector of values. For instance, say you want to know the CDF
# probabilities for each value in the vector x created above:
pnorm(x, mean = 100, sd = 15,col=3)

####### Approach 1: Make a data set with duplicates of random integers.
install.packages("random")  
library(random)            
random<- randomNumbers(n = 300,min = 9,max = 2000,col = 3)
head(random) 
hist(random)

### Approach 2: Create a data set with a random sequence and no duplicates.
random<- randomSequence(min = 9,max = 2000,col = 3)
head(random)





data=data("iris")
test <- chisq.test(table(dat$Species, dat$size))
test


library("MASS")
print(str(Cars93))


 Load the library.
library("MASS")

# Create a data frame from the main data set.
car.data <- data.frame(Cars93$AirBags, Cars93$Type)

# Create a table with the needed variables.
car.data = table(Cars93$AirBags, Cars93$Type) 
print(car.data)

# Perform the Chi-Square test.
print(chisq.test(car.data))


n = 5000
x1 = runif(n, min =0 , max =1 )
y1 = runif(n, min =0 , max =1 )
plot(x1,y1,col='blue',pch=20)
f <- function(x) ((sin(10*x^2))^2*sin(x))*x+0.1
curve(f,0,1,n=100,col='red',lwd=5,add=TRUE)


n = 1000
x1 = runif(n, min =0 , max =1 )
y1 = runif(n, min =0 , max =1 )
f1 <- ((sin(10*x1^2))^2*sin(x1))*x1+0.1
plot(x1[(y1 > f1)],y1[(y1 > f1)],col='yellow',pch=20)
points(x1[(y1 <= f1)],y1[(y1 <= f1)],col='blue',pch=20)

f <- function(x) ((sin(10*x^2))^2*sin(x))*x+0.1
curve(f,0,1,n=100,col='red',lwd=5,add=TRUE)


# count dots above
area_above = length(y1[(y1>f1)])


# count dots below
area_below = length(y1[(y1<=f1)])

# For nomalized square (1x1), the integration from 0 to 1 

cat("Integrate function = ((sin(10*x^2))^2*sin(x))*x+0.1, from 0 to 1")

cat("Area under curve f (integrated from 0 to 1) =", area_below/n)

n = 10000
x1 = runif(n, min =0 , max =1 )
y1 = runif(n, min =0 , max =1 )
f1 <- ((sin(10*x1^2))^2*sin(x1))*x1+0.1
plot(x1[(y1 > f1)],y1[(y1 > f1)],col='yellow',pch=20)
points(x1[(y1 <= f1)],y1[(y1 <= f1)],col='blue',pch=20)

f <- function(x) ((sin(10*x^2))^2*sin(x))*x+0.1
curve(f,0,1,n=100,col='red',lwd=5,add=TRUE)



# count dots below
area_below = length(y1[(y1<=f1)])

# For nomalized square (1x1), the integration from 0 to 1 

cat("Integrate function = ((sin(10*x^2))^2*sin(x))*x+0.1, from 0 to 1")









n = 15000
x1 = runif(n, min =0 , max =1 )
y1 = runif(n, min =0 , max =1 )
f1 <- ((sin(10*x1^2))^2*sin(x1))*x1+0.1
plot(x1[(y1 > f1)],y1[(y1 > f1)],col='yellow',pch=20)
points(x1[(y1 <= f1)],y1[(y1 <= f1)],col='blue',pch=20)

f <- function(x) ((sin(10*x^2))^2*sin(x))*x+0.1
curve(f,0,1,n=100,col='red',lwd=5,add=TRUE)




n <- 1000

x <- runif(n, -1, 1)
y <- runif(n, -1, 1)

inside <- (x^2+y^2 <= 1)
m <- sum(inside)

plot(x, y,
     pch = ifelse(x^2+y^2 <= 1, 20, 1),
     col = ifelse(x^2+y^2 <= 1, "green", "red"),
     cex = 0.5,
     asp = 1, xlim = c(-1, 1), ylim = c(-1, 1))
xx <- seq(-1, 1, len = 200)
lines(xx, sqrt(1-xx^2))
lines(xx, -sqrt(1-xx^2))

cat("Estimate of pi:", 4*m/n)







