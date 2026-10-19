# Sampling distribution of mean when sample is 
# drawing from exponential distribution 
par(mfrow=c(1,3))
set.seed(1822021)
sample_means = rep(NA, 1000)
for(i in 1:1000){
  sample_means[i] = mean(rnorm(20, mean=5.3, sd=9))
}

r_norm <- rnorm(20, mean=5.3, sd=9)
hist(r_norm, main = "", xlab = "r_norm", prob = T)
lines(density(r_norm), col = "Blue", lwd = 2)

hist(sample_means, main = "", xlab = "Sample Means", prob = T)
lines(density(sample_means), col = "Purple", lwd = 2)


# Sampling distribution of variance when sample is 
# drawing from exponential distribution
sample_vars = rep(NA, 1000)
for(i in 1:1000){
  sample_vars[i] = var(rnorm(20, mean=5.3, sd=9))
}

hist(sample_vars, main = "", xlab = "Sample Variance", prob = T)
lines(density(sample_vars), col = "Pink",ylim=c(0,0.015), lwd = 2)
