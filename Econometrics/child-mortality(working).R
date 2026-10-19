# Load dataset
data <- read.csv("D:/STAT-4th Year/STAT-4th Year 1st Semester/LAB (STAT)/LAB (ENM)/Table 6.4.csv")
data

# Extract variables
y  <- data$CM
x2 <- data$PGNP
x3 <- data$FLFP
n  <- length(y)

# Deviations from means
y_dev  <- y  - mean(y)
x2_dev <- x2 - mean(x2)
x3_dev <- x3 - mean(x3)

# Compute sums
y_x2  <- sum(y_dev * x2_dev)
y_x3  <- sum(y_dev * x3_dev)
x2_sq <- sum(x2_dev^2)
x3_sq <- sum(x3_dev^2)
x2_x3 <- sum(x2_dev * x3_dev)

# Denominator (common)
denom <- (x2_sq * x3_sq) - (x2_x3^2)

# Beta2 and Beta3 (equations 7.4.7, 7.4.8)
beta2 <- ( (y_x2 * x3_sq) - (y_x3 * x2_x3) ) / denom
beta3 <- ( (y_x3 * x2_sq) - (y_x2 * x2_x3) ) / denom

# Beta1 (equation 7.4.6)
beta1 <- mean(y) - beta2 * mean(x2) - beta3 * mean(x3)

# Predictions and R^2
y_hat <- beta1 + beta2 * x2 + beta3 * x3
ss_res <- sum((y - y_hat)^2)
ss_tot <- sum((y - mean(y))^2)
r2 <- 1 - ss_res/ss_tot

cat("Beta1 (Intercept):", beta1, "\n")
cat("Beta2 (PGNP):", beta2, "\n")
cat("Beta3 (FLFP):", beta3, "\n")
cat("R^2:", r2, "\n")

