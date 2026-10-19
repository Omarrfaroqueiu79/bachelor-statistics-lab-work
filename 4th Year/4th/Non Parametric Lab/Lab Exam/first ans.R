# Sample data
absentees <- c(27, 6, 19, 24, 18, 12, 15, 17, 18, 20, 0, 9, 4, 12, 3, 2, 7, 7, 0, 5, 32, 16, 38, 31, 27, 15, 5, 9, 4, 10)

# Function to perform runs test
runs_test <- function(data) {
  n <- length(data)
  runs <- rle(sign(diff(data)) >= 0)$lengths
  r <- sum(runs^2) - n
  
  # Calculate test statistic and p-value
  runs_expected <- (2 * n - 1) / 3
  s <- (runs_expected * (2 * runs_expected - 1)) / (n - 1)
  z <- (r - runs_expected) / sqrt(s)
  p_value <- 2 * pnorm(-abs(z))
  
  # Print results
  cat("Runs Test:\n")
  cat("Test Statistic:", z, "\n")
  cat("P-value:", p_value, "\n")
  
  # Interpretation
  if (p_value < 0.05) {
    cat("Result: Reject the null hypothesis. The sequence is not random.\n")
  } else {
    cat("Result: Fail to reject the null hypothesis. The sequence is random.\n")
  }
}

# Perform runs test
runs_test(absentees)


#------------------------------------------------------------------------------------
#Manually:
# Sample data
absentees <- c(27, 6, 19, 24, 18, 12, 15, 17, 18, 20, 0, 9, 4, 12, 3, 2, 7, 7, 0, 5, 32, 16, 38, 31, 27, 15, 5, 9, 4, 10)

# Function to perform runs test
runs_test <- function(data) {
  n <- length(data)
  
  # Count the runs
  runs <- 1
  for (i in 2:n) {
    if (data[i] > data[i - 1]) {
      runs <- runs + 1
    }
  }
  
  # Calculate the expected number of runs and standard deviation
  runs_expected <- (2 * n - 1) / 3
  sd_runs <- sqrt((2 * n * (2 * n - 1)) / 27)
  
  # Calculate the test statistic
  z <- (runs - runs_expected) / sd_runs
  
  # Calculate the p-value
  p_value <- 2 * pnorm(-abs(z))
  
  # Print results
  cat("Runs Test:\n")
  cat("Test Statistic:", z, "\n")
  cat("P-value:", p_value, "\n")
  
  # Interpretation
  if (p_value < 0.05) {
    cat("Result: Reject the null hypothesis. The sequence is not random.\n")
  } else {
    cat("Result: Fail to reject the null hypothesis. The sequence is random.\n")
  }
}

# Perform runs test
runs_test(absentees)
