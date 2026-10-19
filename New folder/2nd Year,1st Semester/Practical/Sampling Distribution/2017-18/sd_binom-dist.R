# Sampling distribution of mean when sample is 
# drawing from exponential distribution 
par(mfrow=c(1,3))
#set.seed(11)
sample_means = rep(NA, 1000)
for(i in 1:1000){
  sample_means[i] = mean(rbinom(10, 5, 0.3))
}

r_binom <- rbinom(10, 5, 0.3)
hist(r_binom, main = "", xlab = "r_binom", prob = T)
points(r_binom, dbinom(r_binom, 5, 0.3), col="darkblue")
#lines(density(r_poi), col = "darkblue", lwd = 1)

hist(sample_means, main = "", xlab = "Sample Means", prob = T)
lines(density(sample_means), col = "darkblue", lwd = 2)


# Sampling distribution of variance when sample is 
# drawing from exponential distribution
sample_vars = rep(NA, 1000)
for(i in 1:1000){
  sample_vars[i] = var(rbinom(10, 5, 0.3))
}

hist(sample_vars, main = "", xlab = "Sample Variances", prob = T)
lines(density(sample_vars), col = "darkblue", lwd = 2)

