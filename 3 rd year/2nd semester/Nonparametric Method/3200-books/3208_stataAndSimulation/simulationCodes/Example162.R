# Example 16.2
g <- function(x){
  sin(pi*x)^2
}

pimc<- function(a,b,g,n){
  x <- runif(n,a,b)
  k <- g(x)
  c <- max(k)
  y <- runif(n,0,c)
  vec <- rep(0,n)
  for(i in 1:n){
    if(y[i]>k[i]){
      vec[i]<-0
    }else{
      vec[i]<- 1
    }
  }
  nH <- sum(vec)
  pimc <- c*(b-a)*nH/n
  pimc
}
pimc(0,1,g,10000)