# LAB-06(SPII)

# Load dataset
data(mtcars)
head(mtcars)

# a) Create observational vector
mtcars$state <- ifelse(mtcars$mpg >= 19, 1, 0)
mtcars$state

# b) Compute transition counts
transitions <- table(head(mtcars$state, -1), tail(mtcars$state, -1))
transitions

# Compute transition probability matrix
transition_matrix <- prop.table(transitions, margin = 1)

# Display the estimated matrix
print(transition_matrix)

# c) Compute expected transition counts based on given matrix
row_totals <- rowSums(transitions)
row_totals

# Given Expected Transition Matrix
expected_matrix <- matrix(c(0.2, 0.8, 0.5, 0.5), nrow = 2, byrow = TRUE)
expected_matrix

# Compute chi-square statistic
chi_square <- sum(row_totals * (transition_matrix - expected_matrix)^2 / expected_matrix)
chi_square

# Critical value for chi-square distribution with df=1 at alpha=0.05
critical_value <- qchisq(0.95, df=1)
critical_value

# Degrees of freedom (df)
df <- (nrow(transition_matrix) - 1) * (ncol(transition_matrix) - 1)
df

# Calculate the p-value
p_value <- 1 - pchisq(chi_square, df)
p_value

# Significance level (alpha)
alpha <- 0.05

# Decision
if (critical_value < chi_square) {
  cat("Conclusion: Reject the null hypothesis. The observed process is NOT a realization of the given Markov chain.\n")
} else {
  cat("Conclusion: Accept the null hypothesis. The observed process is a realization of the given Markov chain.\n")
}

# Interpretation of p-value
if (p_value <= alpha) {
  print("Reject the null hypothesis: The observed transition matrix is not consistent with the given transition matrix.")
} else {
  print("Fail to reject the null hypothesis: The observed transition matrix is consistent with the given transition matrix.")
}
