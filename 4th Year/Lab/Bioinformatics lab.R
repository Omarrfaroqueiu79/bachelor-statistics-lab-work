
#data generating.....
 set.seed(123)
poisson <- rpois(100, lambda=30)
poisson

#normalization.....
set.seed(123)
jibon <- data.frame(rpois(100,30))
jibon <- as.data.frame(sapply(jibon, function(x) (x-min(x))/(max(x)-min(x))))
jibon

#standardization....
set.seed(123)
jibon <- data.frame(rpois(100,30))
jibon <- as.data.frame(sapply(jibon, function(x) (x-mean(x))/sd(x)))
jibon

#centering....
set.seed(123)
jibon <- data.frame(rpois(100,30))
jibon <- as.data.frame(sapply(jibon, function(x) (x-mean(x))))
jibon

#auto scalling....
set.seed(123)
jibon <- data.frame(rpois(100,30))
jibon <- as.data.frame(sapply(jibon, function(x) (x-mean(x))/sd(x)))
jibon

#range scalling....
set.seed(123)
jibon <- data.frame(rpois(100,30))
jibon <- as.data.frame(sapply(jibon, function(x) (x-mean(x))/(max(x)-min(x))))
jibon

#pareto scalling....
set.seed(123)
jibon <- data.frame(rpois(100,30))
jibon <- as.data.frame(sapply(jibon, function(x) (x-mean(x))/sqrt(sd(x))))
jibon

#vast scalling....
set.seed(123)
jibon <- data.frame(rpois(100,30))
jibon <- as.data.frame(sapply(jibon, function(x) ((x-mean(x))/sd(x))*(mean(x)/sd(x))))
jibon

#level scalling....set.seed(123)
jibon <- data.frame(rpois(100,30))
jibon <- as.data.frame(sapply(jibon, function(x) (x-mean(x))/mean(x)))
jibon

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
