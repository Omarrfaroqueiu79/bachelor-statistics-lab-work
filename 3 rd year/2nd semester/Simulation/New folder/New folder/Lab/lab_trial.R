################# Integration in R code
help("integrate")
#Example 1:
  
  # Create function
  g<-function(x){
    x^3 + 2*x
  }
# Integrate from 0 to 1
integrate(g, 0, 1)

### approximate by MC integral
n.sam <- 10000
# step 1: generate n i.i.d samples from f (in this case uniform(0,1))
x.sam <- runif(n.sam)

# compute the MC approximation
theta.mc <- sum(sapply(x.sam, g))/n.sam

print(theta.mc)

############## Ex-2: find integral of x^2
g=function(x){   # Create own function
  y = x^2
}
integrate(g,0,1)
#
## calculate integration by MC
n.sam <- 100000
# step 1: generate n i.i.d samples from f (in this case uniform(0,1))
x.sam <- runif(n.sam,0,1)
x.sam
#   0.3333333
# compute the MC approximation
theta.mc <- sum(sapply(x.sam, g))/n.sam

print(theta.mc)
#   0.3330347

####### Integrate y= 2 * x + x^2 + 5 * x^3 by exact and MC method in [0,1]
g <- function(x) {    # Create own function
  y <- 2 * x + x^2 + 5 * x^3
}
integrate(g,0,1)

## calculate intergeation by MC
n.sam <- 10000000
# step 1: generate n i.i.d samples from f (in this case uniform(0,1))
x.sam <- runif(n.sam,0,1)
#   0.3333333
# compute the MC approximation
theta.mc <- sum(sapply(x.sam, g))/n.sam
print(theta.mc)


######## Use simulation to approximate the integral function exp{e^x} in [0,1]
g=function(x){
  y=exp(exp(x))
}
integrate(g,0,1)

## calculate intergeation by MC
n.sam <- 100000
# step 1: generate n i.i.d samples from f (in this case uniform(0,1))
x.sam <- runif(n.sam,0,1)
#   0.3333333
# compute the MC approximation
theta.mc <- sum(sapply(x.sam, g))/n.sam
print(theta.mc)

############# Use simulation to approximate the integral(1 − x^2)^3/2 in [0,1]
# analytical solution
g=function(x){
  y=(1-x^2)^(3/2)
}
integrate(g,0,1)

##### MC integral
## calculate intergeation by MC
n.sam <- 1000000
# step 1: generate n i.i.d samples from f (in this case uniform(0,1))
x.sam <- runif(n.sam,0,1)
x.sam
#   0.3333333
# compute the MC approximation
theta.mc <- sum(sapply(x.sam, g))/n.sam
print(theta.mc)


######## integrate exp(x+x^2) in [-2,2] by analytical and MC
g=function(x){
  y=exp(x+x^2)
}
integrate(g,-2,2)

#### 

##### MC integral
## calculate intergeation by MC
n.sam <- 100000
# step 1: generate n i.i.d samples from f (in this case uniform(0,1))
x.sam <- runif(n.sam,-2,2)
x.sam
# compute the MC approximation
theta.mc <- 4*(sum(sapply(x.sam, g))/n.sam)
print(theta.mc)



##### Compute pi
x.sam <- runif(100000, min = -1, max = 1)
y.sam <- runif(100000, min = -1, max = 1)
joint.sam <- cbind(x.sam, y.sam)
joint.sam
g.indA <- function(point){
  if ((point[1]^2 + point[2]^2) <= 1) 
    return (1.0)
  else return(0)
}

theta.mc <- sum(apply(joint.sam, 1, g.indA))/nrow(joint.sam)
4*theta.mc


########### Using MC integration solve the integral in[0,1]
g=function(x){
  y=4/(1+x^2)
}
integrate(g,0,1)

##### MC integral
## calculate intergeation by MC
n.sam <- 100000
# step 1: generate n i.i.d samples from f (in this case uniform(0,1))
x.sam <- runif(n.sam,0,1)
x.sam
# compute the MC approximation
theta.mc <- (sum(sapply(x.sam, g))/n.sam)
print(theta.mc)



g=function(x){
  y=0.125*exp(-0.125*x)
}
integrate(g,0,1)
##### MC integral
## calculate intergeation by MC
n.sam <- s
# step 1: generate n i.i.d samples from f (in this case uniform(0,1))
x.sam <- runif(n.sam,0,1)
x.sam
# compute the MC approximation
theta.mc <- (sum(sapply(x.sam, g))/n.sam)
print(theta.mc)


############# Integrate e^-x in [2,4] using  Monte Carlo integration 
g=function(x){
  y=exp(-x)
}
integrate(g,2,4)

m <- 10000
x <- runif(m, min = 2, max = 4)
theta.hat <- mean(exp(-x)) * 2
print(theta.hat)













