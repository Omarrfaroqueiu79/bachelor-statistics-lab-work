
#data generating.....
 set.seed(123)
poisson <- rpois(100, lambda=30)
poisson

#normalization.....
set.seed(123)
mim <- data.frame(rpois(100,30))
mim <- as.data.frame(sapply(mim, function(x) (x-min(x))/(max(x)-min(x))))
mim

#standardization....
set.seed(123)
mim <- data.frame(rpois(100,30))
mim <- as.data.frame(sapply(mim, function(x) (x-mean(x))/sd(x)))
mim

#centering....
set.seed(123)
mim <- data.frame(rpois(100,30))
mim <- as.data.frame(sapply(mim, function(x) (x-mean(x))))
mim

#auto scalling....
set.seed(123)
mim <- data.frame(rpois(100,30))
mim <- as.data.frame(sapply(mim, function(x) (x-mean(x))/sd(x)))
mim

#range scalling....
set.seed(123)
mim <- data.frame(rpois(100,30))
mim <- as.data.frame(sapply(mim, function(x) (x-mean(x))/(max(x)-min(x))))
mim

#pareto scalling....
set.seed(123)
mim <- data.frame(rpois(100,30))
mim <- as.data.frame(sapply(mim, function(x) (x-mean(x))/sqrt(sd(x))))
mim

#vast scalling....
set.seed(123)
mim <- data.frame(rpois(100,30))
mim <- as.data.frame(sapply(mim, function(x) ((x-mean(x))/sd(x))*(mean(x)/sd(x))))
mim

#level scalling....set.seed(123)
mim <- data.frame(rpois(100,30))
mim <- as.data.frame(sapply(mim, function(x) (x-mean(x))/mean(x)))
mim

set.seed(123)
x<-rnorm(100,2,5)
boxplot(x)

set.seed(123)
x<-rnorm(100,3,7)
boxplot(x)10


set.seed(123)
x<-rnorm(100,2,5)
x
#L1 calculation....
absMean<-function(x){
  average<-sum(abs(x))/length(x)
  return(average)
}
absMean
