# Given data
observed_frequencies <- c(224, 119, 130, 48, 59)
expected_probabilities <- c(0.4, 0.2, 0.2, 0.1, 0.1)
total_observation <- sum(observed_frequencies)

# Calculate expected frequencies
expected_frequencies <- total_observation * expected_probabilities

# Perform chi-square test
chi_square_test_result <- chisq.test(observed_frequencies, p = expected_probabilities)

# Print the results
cat("Chi-square test of independence:\n")
cat("Test Statistic:", chi_square_test_result$statistic, "\n")
cat("Degrees of Freedom:", chi_square_test_result$parameter, "\n")
cat("P-value:", chi_square_test_result$p.value, "\n")

# Interpretation
if (chi_square_test_result$p.value < 0.05) {
  cat("Result: Reject the null hypothesis. There is evidence that the color distribution is different from the specified probabilities.\n")
} else {
  cat("Result: Fail to reject the null hypothesis. There is not enough evidence to conclude that the color distribution is different from the specified probabilities.\n")
}



#--------------------------------------------------------------------------------
#Manually:
# Given data
observed_frequencies <- c(224, 119, 130, 48, 59)
expected_probabilities <- c(0.4, 0.2, 0.2, 0.1, 0.1)
total_observation <- sum(observed_frequencies)

# Calculate expected frequencies
expected_frequencies <- total_observation * expected_probabilities

# Calculate the chi-square statistic
chi_square_statistic <- sum((observed_frequencies - expected_frequencies)^2 / expected_frequencies)

# Degrees of freedom
df <- length(observed_frequencies) - 1

# Calculate the p-value
p_value <- 1 - pchisq(chi_square_statistic, df)

# Print the results
cat("Chi-square test of independence:\n")
cat("Test Statistic:", chi_square_statistic, "\n")
cat("Degrees of Freedom:", df, "\n")
cat("P-value:", p_value, "\n")

# Interpretation
if (p_value < 0.05) {
  cat("Result: Reject the null hypothesis. There is evidence that the color distribution is different from the specified probabilities.\n")
} else {
  cat("Result: Fail to reject the null hypothesis. There is not enough evidence to conclude that the color distribution is different from the specified probabilities.\n")
}
