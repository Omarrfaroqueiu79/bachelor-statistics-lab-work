# Given data
data <- c(108, 112, 117, 130, 111, 131, 113, 113, 105, 128)

# Assumed parameters for the normal distribution
assumed_mean <- 120
assumed_sd <- 10

# Perform one-sample t-test
t_test_result <- t.test(data, mu = assumed_mean, alternative = "two.sided", conf.level = 0.95)

# Print the results
cat("One-sample t-test:\n")
cat("Test Statistic:", t_test_result$statistic, "\n")
cat("Degrees of Freedom:", t_test_result$parameter, "\n")
cat("P-value:", t_test_result$p.value, "\n")

# Interpretation
if (t_test_result$p.value < 0.05) {
  cat("Result: Reject the null hypothesis. The data may not come from a normal distribution with the assumed parameters.\n")
} else {
  cat("Result: Fail to reject the null hypothesis. The data may come from a normal distribution with the assumed parameters.\n")
}



#------------------------------------------------------------------------------------------

#Manually:
# Given data
data <- c(108, 112, 117, 130, 111, 131, 113, 113, 105, 128)

# Assumed parameters for the normal distribution
assumed_mean <- 120
assumed_sd <- 10

# Number of observations
n <- length(data)

# Calculate the sample mean and standard deviation
sample_mean <- mean(data)
sample_sd <- sd(data)

# Calculate the standard error of the mean
se <- sample_sd / sqrt(n)

# Calculate the t-statistic
t_stat <- (sample_mean - assumed_mean) / se

# Degrees of freedom
df <- n - 1

# Calculate the p-value
p_value <- 2 * pt(-abs(t_stat), df)

# Print the results
cat("One-sample t-test:\n")
cat("Test Statistic:", t_stat, "\n")
cat("Degrees of Freedom:", df, "\n")
cat("P-value:", p_value, "\n")

# Interpretation
if (p_value < 0.05) {
  cat("Result: Reject the null hypothesis. The data may not come from a normal distribution with the assumed parameters.\n")
} else {
  cat("Result: Fail to reject the null hypothesis. The data may come from a normal distribution with the assumed parameters.\n")
}

