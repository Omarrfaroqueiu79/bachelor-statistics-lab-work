# LAB-05(SPII)

# Set the number of steps
n_steps <- 50

# Initialize the starting position
position <- numeric(n_steps + 1)

# Simulate the random walk
set.seed(42)  # For reproducibility

for (i in 2:(n_steps + 1)) {
  # Move left or right with probability 1/2
  move <- sample(c(-1, 1), 1, replace = TRUE)
  position[i] <- position[i - 1] + move
}
position

# Plot the position of the particle at each step
plot(position, type = "o", col = "blue", xlab = "Step", ylab = "Position",
     main = "Random Walk - 50 Steps", pch = 16)


# Part (b) - Final position after 50 steps
final_position <- position[n_steps + 1]
print(paste("Final position after 50 steps:", final_position))











