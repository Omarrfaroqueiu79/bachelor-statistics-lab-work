# Sampling distribution of mean when sample is 
# drawing from exponential distribution 
par(mfrow=c(1,3))
set.seed(1822021)
sample_means = rep(NA, 1000)
for(i in 1:1000){
  sample_means[i] = mean(rpois(10, lambda = 4))
}

r_poi <- rpois(10, lambda = 4)
hist(r_poi, main = "", xlab = "r_poi", prob = T)
points(r_poi, dpois(r_poi, 4), col="Red", lty=2)

hist(sample_means, main = "", xlab = "Sample Means", prob = T, ylim = c(0,0.8))
lines(density(sample_means), col = "Yellow", lwd = 2)


# Sampling distribution of variance when sample is 
# drawing from exponential distribution
sample_vars = rep(NA, 1000)
for(i in 1:1000){
  sample_vars[i] = var(rpois(10, lambda = 10))
}

hist(sample_vars, main = "", xlab = "Sample Variances", prob = T,ylim=c(0,0.1))
lines(density(sample_vars), col = "Green", lwd = 2)

