# Given data
fish_lengths <- c(5.0, 3.9, 5.2, 5.5, 2.8, 6.1, 6.4, 2.6, 1.7, 4.3)

# Assumed median for comparison
assumed_median <- 3.7

# Perform one-sample Wilcoxon signed-rank test
wilcoxon_test_result <- wilcox.test(fish_lengths, mu = assumed_median, alternative = "two.sided", conf.int = TRUE)

# Print the results
cat("One-sample Wilcoxon signed-rank test:\n")
cat("Test Statistic:", wilcoxon_test_result$statistic, "\n")
cat("P-value:", wilcoxon_test_result$p.value, "\n")
cat("95% Confidence Interval for the Median:", wilcoxon_test_result$conf.int, "\n")

# Interpretation
if (wilcoxon_test_result$p.value < 0.05) {
  cat("Result: Reject the null hypothesis. The median length of pygmy sun-fish differs significantly from 3.7 centimeters.\n")
} else {
  cat("Result: Fail to reject the null hypothesis. There is not enough evidence to conclude that the median length differs significantly from 3.7 centimeters.\n")
}


#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Manually:
# Given data
fish_lengths <- c(5.0, 3.9, 5.2, 5.5, 2.8, 6.1, 6.4, 2.6, 1.7, 4.3)

# Assumed median for comparison
assumed_median <- 3.7

# Number of observations
n <- length(fish_lengths)

# Calculate the differences between observed values and assumed median
differences <- fish_lengths - assumed_median

# Rank the absolute differences
ranked_diff <- rank(abs(differences))

# Calculate the test statistic
w_stat <- sum((differences > 0) * ranked_diff - (differences < 0) * ranked_diff)

# Calculate the expected value of the test statistic under the null hypothesis
w_expected <- n * (n + 1) / 4

# Calculate the standard deviation of the test statistic
w_sd <- sqrt(n * (n + 1) * (2 * n + 1) / 24)

# Calculate the z-score
z <- (w_stat - w_expected) / w_sd
z
# Calculate the p-value
p_value <- 2 * pnorm(-abs(z))

# Print the results
cat("One-sample Wilcoxon signed-rank test:\n")
cat("Test Statistic:", w_stat, "\n")
cat("P-value:", p_value, "\n")

# Interpretation
if (p_value < 0.05) {
  cat("Result: Reject the null hypothesis. The median length of pygmy sun-fish differs significantly from 3.7 centimeters.\n")
} else {
  cat("Result: Fail to reject the null hypothesis. There is not enough evidence to conclude that the median length differs significantly from 3.7 centimeters.\n")
}

