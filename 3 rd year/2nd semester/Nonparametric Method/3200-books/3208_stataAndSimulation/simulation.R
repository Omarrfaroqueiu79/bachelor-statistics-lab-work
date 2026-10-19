# problem 1
set.seed(1000)
g <- function(x){
  4*(1/(1+x^2))
}
m <- 100000
x <- runif(m)
mean(g(x))

# Example 16.2
set.seed(125)
pimc <- function(a,b,g,n){
  x <- runif(n,a,b)
  k <- g(x)
  c <- max(k)
  y <- runif(n,0,c)
  vec <- rep(0,n)
  for(i in 1:n){
    if(y[i]>k[i]){
      vec[i]<-0
    }else{
      vec[i]<-1
    }
  }
  nH <- sum(vec)
  pimc<- c*(b-a)*nH/n
  pimc
}
g <- function(x){(sin(pi*x))^2}
pimc(0,1,g,1000)

# problem 16.3 
set.seed(111)
s <- runif(100000)
mean(exp(x)*(x-.5))
