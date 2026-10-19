# LAB-01 (SPII)

# Define transition matrix P with row and column names
states <- c("Sunny", "Cloudy", "Rainy")
states

P <- matrix(c(0.7, 0.2, 0.1,
              0.3, 0.4, 0.3,
              0.2, 0.5, 0.3), 
            nrow = 3, byrow = TRUE, 
            dimnames = list(states, states))
P

# Initial state (today is Sunny)
initial_state <- c(1, 0, 0)  # [Sunny, Cloudy, Rainy]

# (a) Probability of being Rainy in 2 days
P_2 <- P %*% P
prob_rainy_2_days <- sum(initial_state %*% P_2[,3])  # Extract Rainy probability
prob_rainy_2_days

# Interpret
cat(sprintf("If today is Sunny, then after 2 days, the probability that it will be Rainy is %.4f.\n", prob_rainy_2_days))
cat(sprintf("This means there is a %.2f%% chance of experiencing Rainy weather after 2 days.\n", prob_rainy_2_days * 100))

# (b) Compute steady-state probabilities
A <- rbind(t(P) - diag(3), c(1, 1, 1))  # Augmented system
b <- c(0, 0, 0, 1)  # Constraint sum(π) = 1
steady_state <- solve(t(A) %*% A, t(A) %*% b)

# Convert steady-state results to a named vector
steady_state_df <- data.frame("Steady-State Probability" = steady_state, row.names=states)
steady_state_df

# Interpret
cat("\n Long-term (Steady-State) Probabilities:\n")
print(steady_state_df)
cat("\n This means that in the long run, the city will have:\n")
cat(sprintf(" %.2f%% Sunny days,\n", steady_state[1] * 100))
cat(sprintf(" %.2f%% Cloudy days, and\n", steady_state[2] * 100))
cat(sprintf(" %.2f%% Rainy days.\n", steady_state[3] * 100))
cat("These probabilities remain stable over time.\n")

