# LAB-02(SPII)

# Given parameters
mean_lifetime <- 10
time_limit <- 50
lambda <- 1 / mean_lifetime

# (a)
# Expected number of replacements
expected_replacements <- time_limit / mean_lifetime
expected_replacements

# Print the expected number of replacements (Additional Part)
cat("Expected number of replacements:", expected_replacements, "\n")

# (b)
# Simulating the process
set.seed(42)  # For reproducibility

# Generate a large number of exponential lifetimes
lifetimes <- rexp(1000, rate=lambda)

# Compute cumulative sum
cumulative_times <- cumsum(lifetimes)

# Count how many light bulbs were replaced before reaching 50 hours
count <- sum(cumulative_times < time_limit)

# Output result
cat("Simulated number of replacements in 50 hours:", count, "\n")
