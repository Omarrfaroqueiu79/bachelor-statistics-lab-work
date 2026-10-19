################# Integration in R code
help("integrate")
# Problem 1
# Create function
g<-function(x){
 x^3 + 2*x
  }
# Integrate from 0 to 1
integrate(g, 0, 1)


#Set the number of values
n <- 100000
#Generate n random numbers on the interval [0,1]
x  <- runif(n,min=0,max=1)
#Evaluate the function at the random values of s


fx <-  x^3 + 2*x
#Estimate the integral
integral <- mean(fx)*(1-0) # Multiply by the base length 1-0
integral


############## Ex-2: find integral of x^2
g=function(x){   # Create own function
	y = x^2
}
integrate(g,0,2)


#Set the number of values
n <- 100000
#Generate n random numbers on the interval [0,1]
x  <- runif(n,min=0,max=2)
#Evaluate the function at the random values of s
fx <- x^2
#Estimate the integral
integral <- mean(fx)*(2-0) # Multiply by the base length 1-0
integral



############## Problem 3 : find integral of x^2
g=function(x){   # Create own function
	y = 4*x^2
}
integrate(g,0,6)


set.seed(123)

#Set the number of values
n <- 10000
#Generate n random numbers on the interval [0,6]
x  <- runif(n,min=0,max=6)
#Evaluate the function at the random values
fx <- 4*x^2
#Estimate the integral
integral <- mean(fx)*(6-0) # Multiply by the base length 6-0
integral

############## Problem 4 : find integral of 
g=function(x){   # Create own function
	y = 4*x^2
}
integrate(g,0,6)


set.seed(123)

#Set the number of values
n <- 10000
#Generate n random numbers on the interval 
t  <- runif(n,min=0,max=pi/3)
#Evaluate the function at the random values
ft <- sin(t)
#Estimate the integral
integral <- mean(ft)*(pi/3-0) # Multiply by the base length Pi-0
integral



############## Problem 4 : find integral of 
g=function(x){   # Create own function
	y = 4*x^2
}
integrate(g,2,6)


set.seed(123)

#Set the number of values
n <- 100000
#Generate n random numbers on the interval [2,6]
x  <- runif(n,min=2,max=6)
#Evaluate the function at the random values
fx <- 4*x^2
#Estimate the integral
integral <- mean(fx)*(6-2) # Multiply by the base length 6-0
integral



######## Use simulation to approximate the integral function exp{e^x} in [0,1]
g=function(x){
	y=exp(exp(x))
}
integrate(g,0,1)


set.seed(123)

#Set the number of values
n <- 100000
#Generate n random numbers on the interval [1,0]
x  <- runif(n,min=0,max=1)
#Evaluate the function at the random values
fx <- exp(exp(x))
#Estimate the integral
integral <- mean(fx)*(1-0) # Multiply by the base length 6-0
integral









######## Use simulation to approximate the integral function exp{e^x} in [0,1]
g=function(x){
	y=exp(exp(x))^log(x)
}
integrate(g,0,1)


set.seed(123)
#Set the number of values
n <- 100000
#Generate n random numbers on the interval [1,0]
x  <- runif(n,min=0,max=1)
#Evaluate the function at the random values
fx <- exp(exp(x))^log(x)
#Estimate the integral
integral <- mean(fx)*(1-0) # Multiply by the base length 6-0
integral


