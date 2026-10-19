# Sampling distribution of mean when sample is 
# drawing from exponential distribution 
par(mfrow=c(1,3))
set.seed(1822021)
sample_means = rep(NA, 1000)
for(i in 1:1000){
  sample_means[i] = mean(rexp(40,0.1))
}

r_exp <- rexp(40,0.1)
hist(r_exp, main = "", xlab = "r_exp", prob = T)
lines(density(r_exp), col = "Red",lwd = 1.8)

hist(sample_means, main = "", xlab = "Sample Means", prob = T)
lines(density(sample_means), col = "Black", lwd = 1.8)


# Sampling distribution of variance when sample is 
# drawing from exponential distribution 
set.seed(00)
sample_vars = rep(NA, 1000)
for(i in 1:1000){
  sample_vars[i] = var(rexp(40,0.1))
}

hist(sample_vars, main = "", xlab = "Sample Variances", prob = T)
lines(density(sample_vars), col = "Yellow",ylim=c(0,0.010), lwd = 1.8)