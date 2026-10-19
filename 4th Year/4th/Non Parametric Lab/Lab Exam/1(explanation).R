# Problem: A supervisor records the number of employees absent over a 30-day period. Test for randomness.

# Given data representing the number of absent employees over 30 days
absentees <- c(27, 6, 19, 24, 18, 12, 15, 17, 18, 20, 0, 9, 4, 12, 3, 2, 7, 7, 0, 5, 32, 16, 38, 31, 27, 15, 5, 9, 4, 10)

# Function to perform runs test to check for randomness
runs_test <- function(data) {
  n <- length(data)
  
  # Count the runs (consecutive increasing values)
  runs <- 1
  for (i in 2:n) {
    # Check if the current value is greater than the previous one
    if (data[i] > data[i - 1]) {
      runs <- runs + 1
    }
  }
  
  # Calculate the expected number of runs and standard deviation
  runs_expected <- (2 * n - 1) / 3
  sd_runs <- sqrt((2 * n * (2 * n - 1)) / 27)
  
  # Calculate the test statistic (Z-score)
  z <- (runs - runs_expected) / sd_runs
  
  # Calculate the p-value using the normal distribution
  p_value <- 2 * pnorm(-abs(z))
  
  # Print results
  cat("Runs Test:\n")
  cat("Test Statistic (Z):", z, "\n")
  cat("P-value:", p_value, "\n")
  
  # Interpretation based on the p-value
  if (p_value < 0.05) {
    cat("Result: Reject the null hypothesis. The sequence is not random.\n")
  } else {
    cat("Result: Fail to reject the null hypothesis. The sequence appears to be random.\n")
  }
}

# Perform runs test on the given data
runs_test(absentees)
