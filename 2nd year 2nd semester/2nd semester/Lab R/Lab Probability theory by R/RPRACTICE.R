#sampling distribution of mean when sample is
#drawing from exponential distribution 
par(mfrow=c(1,3))
set.seed(21)
sample_means=rep(NA, 1000)
for (i in 1:1000) {
sample_means[i]= mean(rpois(10, lambda=4))  
}
r_poi<-rpois(10, lambda=4)
hist(r_poi, main = "", xlab = "r-pois", prob=T)
lines(density(r_poi), col="blue", lwd=2)

hist(sample_means, main = "", xlab = "sample mean", prob=T)
lines(density(sample_means), col="blue", lwd=2)

#sampling distribution of varience when sample is
#drawing from exponential distribution 

sample_vars=rep(NA, 1000)
for (i in 1:1000) {
  sample_vars[i]=var(rpois(10, lambda=10))
}
hist(sample_vars, main = "", xlab = "sample variance", prob=T)
lines(density(sample_vars), col="blue", lwd=2)

