#sampling distribution of mean when sample is
#drawing from exponential distribution
par(mfrow=c(1,3))
set.seed(21)
sample_means=rep(NA, 1000)
for (i in 1:1000) {
  sample_means[i]=mean(rbinom(10,5,0.3))
}
r_binom<-rbinom(10,5,0.3)
hist(r_binom, main = "", xlab = "r_binom", prob=T)
lines(density(r_binom), col="blue", lwd=2)

hist(sample_means, main = "", xlab = "sample mean", prob=T)
lines(density(sample_means), col="green", lwd=2)

#sampling distribution of variance when sample is
#drawing from exponential distribution
sample_vars=rep(NA, 1000)
for (i in 1:1000) {
  sample_vars[i]=var(rbinom(10,5,0.3))
}
hist(sample_vars,main = "", xlab = "sample variance", probability = T)
lines(density(sample_vars), col="yellow", lwd=2)

