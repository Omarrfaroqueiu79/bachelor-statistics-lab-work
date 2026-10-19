# LAB-03(SPII)

# Given parameters
lambda <- 5  # Arrival rate (customers per hour)
mu <- 8      # Service rate (customers per hour)

# Utilization factor (ρ)
rho <- lambda / mu

# a)
# Probability that the server is idle
P0 <- 1 - rho

# b)
# Expected number of customers in the system
L <- rho / (1 - rho)

# c)
# Average time a customer spends in the system
W <- 1 / (mu - lambda)

# Print results
cat(sprintf("Probability that the server is idle: %.2f%%\n", P0*100))
cat(sprintf("Expected number of customers in the system: %.4f\n", L))
cat(sprintf("Average time a customer spends in the system (W): %.4f hours (%.2f minutes)\n", W, W*60))





