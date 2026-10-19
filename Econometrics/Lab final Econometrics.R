setwd("C:\\Users\\M R Computer\\OneDrive\\Desktop\\Econometrics_Lab")
#________________________________(1)__________________________________________________
# Read the data
data <- read.csv("cobb doglaus.csv")

# Display the data
cat("Cobb-Douglas Production Data:\n")
print(data)

# Extract variables
Y <- data$Real.gross.product..millions.of.NT..  # Output
L <- data$Labor.days..millions.of.days.         # Labor input
K <- data$Real.capital.input..millions.of.NT..  # Capital input

# Take natural logarithms for linearization
lnY <- log(Y)
lnL <- log(L)
lnK <- log(K)

# Create design matrix X with intercept and predictors
X <- cbind(1, lnL, lnK)  # Adding intercept column

# Calculate regression coefficients using matrix algebra: β = (X'X)^(-1)X'y
beta_hat <- solve(t(X) %*% X) %*% t(X) %*% lnY

# Extract coefficients
lnA <- beta_hat[1]  # Intercept (ln(A))
beta <- beta_hat[2] # Labor coefficient
alpha <- beta_hat[3] # Capital coefficient

# Calculate A (technology parameter)
A <- exp(lnA)

# Calculate predicted values
lnY_hat <- X %*% beta_hat
Y_hat <- exp(lnY_hat)

# Calculate residuals
residuals <- lnY - lnY_hat

# Calculate residual sum of squares (RSS)
RSS <- sum(residuals^2)

# Calculate total sum of squares (TSS)
TSS <- sum((lnY - mean(lnY))^2)

# Calculate explained sum of squares (ESS)
ESS <- TSS - RSS

# Calculate R-squared
R_squared <- ESS / TSS

# Calculate adjusted R-squared
n <- length(Y)
p <- ncol(X) - 1  # number of predictors
adj_R_squared <- 1 - (RSS/(n-p-1)) / (TSS/(n-1))

# Calculate standard error of regression
sigma_hat <- sqrt(RSS / (n - p - 1))

# Calculate variance-covariance matrix of coefficients
var_cov_beta <- sigma_hat^2 * solve(t(X) %*% X)

# Calculate standard errors of coefficients
se_beta <- sqrt(diag(var_cov_beta))

# Calculate t-statistics
t_stats <- beta_hat / se_beta

# Calculate p-values
p_values <- 2 * pt(abs(t_stats), df = n - p - 1, lower.tail = FALSE)

# Create results table
results <- data.frame(
  Coefficient = beta_hat,
  Std_Error = se_beta,
  t_Statistic = t_stats,
  p_Value = p_values
)

rownames(results) <- c("ln(A)", "β (Labor)", "α (Capital)")

# Print results
cat("\nCobb-Douglas Production Function Estimation:\n")
cat("============================================\n\n")

cat("Linearized Model: ln(Y) = ln(A) + β*ln(L) + α*ln(K)\n\n")

cat("Estimated Parameters:\n")
cat("A (Technology parameter):", round(A, 4), "\n")
cat("β (Labor elasticity):", round(beta, 4), "\n")
cat("α (Capital elasticity):", round(alpha, 4), "\n")
cat("Returns to scale (β + α):", round(beta + alpha, 4), "\n\n")

cat("Regression Equation:\n")
cat("ln(Y) =", round(lnA, 4), "+", round(beta, 4), "* ln(L) +", round(alpha, 4), "* ln(K)\n")
cat("Y =", round(A, 4), "* L^", round(beta, 4), "* K^", round(alpha, 4), "\n\n")

cat("Coefficient Estimates:\n")
print(results)

cat("\nModel Fit Statistics:\n")
cat("R-squared:", round(R_squared, 4), "\n")
cat("Adjusted R-squared:", round(adj_R_squared, 4), "\n")
cat("Residual Standard Error:", round(sigma_hat, 4), "\n")
cat("Number of observations:", n, "\n\n")

# Test for constant returns to scale (β + α = 1)
cat("Test for Constant Returns to Scale (β + α = 1):\n")
cat("Sum of elasticities:", round(beta + alpha, 4), "\n")

if (abs(beta + alpha - 1) < 0.1) {
  cat("Conclusion: Approximately constant returns to scale\n")
} else if (beta + alpha > 1) {
  cat("Conclusion: Increasing returns to scale\n")
} else {
  cat("Conclusion: Decreasing returns to scale\n")
}

# ---------------------------- DIAGNOSTIC PLOTS ----------------------------

# Set up plotting area
par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))

# 1. Actual vs Predicted values
plot(lnY, lnY_hat, main = "Actual vs Predicted (log scale)", 
     xlab = "Actual ln(Y)", ylab = "Predicted ln(Y)", pch = 16, col = "blue")
abline(0, 1, col = "red", lwd = 2)
legend("topleft", legend = "Perfect fit line", col = "red", lwd = 2)

# 2. Residuals vs Fitted values
plot(lnY_hat, residuals, main = "Residuals vs Fitted Values", 
     xlab = "Fitted ln(Y)", ylab = "Residuals", pch = 16, col = "darkgreen")
abline(h = 0, col = "red", lwd = 2)
legend("topright", legend = "Zero residual line", col = "red", lwd = 2)

# 3. Normal Q-Q plot of residuals
sorted_residuals <- sort(residuals)
theoretical_quantiles <- qnorm(ppoints(length(residuals)))
plot(theoretical_quantiles, sorted_residuals, main = "Normal Q-Q Plot of Residuals", 
     xlab = "Theoretical Quantiles", ylab = "Sample Quantiles", pch = 16, col = "purple")
abline(0, sd(residuals), col = "red", lwd = 2)

# 4. Time series of actual vs predicted
plot(1:n, lnY, type = "l", col = "blue", lwd = 2, main = "Time Series: Actual vs Predicted", 
     xlab = "Time (Years)", ylab = "ln(Y)", ylim = range(c(lnY, lnY_hat)))
lines(1:n, lnY_hat, col = "red", lwd = 2, lty = 2)
legend("topleft", legend = c("Actual", "Predicted"), col = c("blue", "red"), 
       lwd = 2, lty = c(1, 2))

# Reset plotting parameters
par(mfrow = c(1, 1))

# ---------------------------- ADDITIONAL ANALYSIS ----------------------------

# Calculate marginal products
cat("\nMarginal Products Analysis:\n")
cat("============================\n")

# For the last observation (most recent year)
last_Y <- Y[n]
last_L <- L[n]
last_K <- K[n]

MPL <- beta * (last_Y / last_L)  # Marginal product of labor
MPK <- alpha * (last_Y / last_K) # Marginal product of capital

cat("Marginal Product of Labor (MPL) for year", data$Year[n], ":", round(MPL, 2), "\n")
cat("Marginal Product of Capital (MPK) for year", data$Year[n], ":", round(MPK, 2), "\n")

# Calculate output elasticities
cat("\nOutput Elasticities:\n")
cat("Labor elasticity (β):", round(beta, 4), "\n")
cat("Capital elasticity (α):", round(alpha, 4), "\n")

# Forecast for next period (assuming 5% growth in inputs)
cat("\nForecast for next period (5% input growth):\n")
L_next <- last_L * 1.05
K_next <- last_K * 1.05
Y_forecast <- A * (L_next^beta) * (K_next^alpha)
growth_rate <- (Y_forecast / last_Y - 1) * 100

cat("Forecasted output:", round(Y_forecast, 2), "million NT$\n")
cat("Expected growth rate:", round(growth_rate, 2), "%\n")

#__________________________________________(2)_____________________________________________

# Read the data
data <- read.csv("data set 6.4.csv")

# Display the first few rows to understand the data
head(data)

# Extract variables
CM <- data$CM
FLFP <- data$FLFP
PGNP <- data$PGNP
TFR <- data$TFR

# Create design matrix X with intercept and predictors
X <- cbind(1, FLFP, PGNP, TFR)  # Adding intercept column

# Calculate regression coefficients using matrix algebra: β = (X'X)^(-1)X'y
beta_hat <- solve(t(X) %*% X) %*% t(X) %*% CM

# Calculate predicted values
y_hat <- X %*% beta_hat

# Calculate residuals
residuals <- CM - y_hat

# Calculate residual sum of squares (RSS)
RSS <- sum(residuals^2)

# Calculate total sum of squares (TSS)
TSS <- sum((CM - mean(CM))^2)

# Calculate explained sum of squares (ESS)
ESS <- TSS - RSS

# Calculate R-squared
R_squared <- ESS / TSS

# Calculate adjusted R-squared
n <- length(CM)
p <- ncol(X) - 1  # number of predictors
adj_R_squared <- 1 - (RSS/(n-p-1)) / (TSS/(n-1))

# Calculate standard error of regression
sigma_hat <- sqrt(RSS / (n - p - 1))

# Calculate variance-covariance matrix of coefficients
var_cov_beta <- sigma_hat^2 * solve(t(X) %*% X)

# Calculate standard errors of coefficients
se_beta <- sqrt(diag(var_cov_beta))

# Calculate t-statistics
t_stats <- beta_hat / se_beta

# Calculate p-values
p_values <- 2 * pt(abs(t_stats), df = n - p - 1, lower.tail = FALSE)

# Create results table
results <- data.frame(
  Coefficient = beta_hat,
  Std_Error = se_beta,
  t_Statistic = t_stats,
  p_Value = p_values
)

rownames(results) <- c("Intercept", "FLFP", "PGNP", "TFR")

# Print results

cat("Regression Equation:\n")
cat("CM =", round(beta_hat[1], 4), "+", round(beta_hat[2], 4), "* FLFP +", 
    round(beta_hat[3], 4), "* PGNP +", round(beta_hat[4], 4), "* TFR\n\n")

cat("Coefficient Estimates:\n")
print(results)

cat("\nModel Fit Statistics:\n")
cat("R-squared:", round(R_squared, 4), "\n")
cat("Adjusted R-squared:", round(adj_R_squared, 4), "\n")
cat("Residual Standard Error:", round(sigma_hat, 4), "\n")
cat("Residual Sum of Squares:", round(RSS, 4), "\n")
cat("Total Sum of Squares:", round(TSS, 4), "\n")

# Diagnostic plots (optional visualization)
par(mfrow = c(2, 2))
plot(y_hat, residuals, main = "Residuals vs Fitted", xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red")
qqnorm(residuals, main = "Normal Q-Q Plot")
qqline(residuals, col = "red")
plot(y_hat, sqrt(abs(residuals)), main = "Scale-Location Plot", xlab = "Fitted Values", ylab = "√|Residuals|")
plot(1:n, residuals, main = "Residuals vs Order", xlab = "Observation Order", ylab = "Residuals")
abline(h = 0, col = "red")



#___________________________________________(3)_____________________________________________


# Read the data
data <- read.csv("durbin.csv")

# Display the data
cat("Durbin-Watson Test Data:\n")
print(head(data))

# Extract variables
Y <- data$Y
X <- data$X
n <- length(Y)

# ---------------------------- REGRESSION MODEL ----------------------------

# Create design matrix X with intercept
X_matrix <- cbind(1, X)

# Calculate regression coefficients using matrix algebra: β = (X'X)^(-1)X'y
beta_hat <- solve(t(X_matrix) %*% X_matrix) %*% t(X_matrix) %*% Y

# Calculate predicted values
Y_hat <- X_matrix %*% beta_hat

# Calculate residuals
residuals <- Y - Y_hat

# Calculate residual sum of squares (RSS)
RSS <- sum(residuals^2)

# ---------------------------- DURBIN-WATSON TEST ----------------------------

# Calculate Durbin-Watson statistic
DW_numerator <- 0
DW_denominator <- 0

for (i in 2:n) {
  DW_numerator <- DW_numerator + (residuals[i] - residuals[i-1])^2
}

for (i in 1:n) {
  DW_denominator <- DW_denominator + residuals[i]^2
}

d_stat <- DW_numerator / DW_denominator

# ---------------------------- CRITICAL VALUES ----------------------------

# Since we can't use packages, we'll use approximate critical values
# For n=40, k=1 (one predictor + intercept)
# These are approximate values for α=0.05
d_l <- 1.44  # Lower critical value
d_u <- 1.54  # Upper critical value

# ---------------------------- HYPOTHESIS TEST ----------------------------

cat("\nDurbin-Watson Test for Autocorrelation:\n")
cat("=======================================\n\n")

cat("Regression Equation: Y =", round(beta_hat[1], 4), "+", round(beta_hat[2], 4), "* X\n\n")

cat("Durbin-Watson statistic (d):", round(d_stat, 4), "\n")
cat("Lower critical value (dL):", d_l, "\n")
cat("Upper critical value (dU):", d_u, "\n")
cat("Sample size (n):", n, "\n")
cat("Number of predictors (k):", 1, "\n\n")

# Decision rule
if (d_stat < d_l) {
  cat("Decision: REJECT H₀ (Positive autocorrelation exists)\n")
} else if (d_stat > d_u && d_stat < (4 - d_u)) {
  cat("Decision: FAIL TO REJECT H₀ (No autocorrelation)\n")
} else if (d_stat > (4 - d_l)) {
  cat("Decision: REJECT H₀ (Negative autocorrelation exists)\n")
} else {
  cat("Decision: TEST IS INCONCLUSIVE\n")
}

# ---------------------------- ADDITIONAL AUTOCORRELATION MEASURES ----------------------------

# Calculate first-order autocorrelation coefficient
rho_numerator <- 0
rho_denominator <- 0

for (i in 2:n) {
  rho_numerator <- rho_numerator + residuals[i] * residuals[i-1]
}

for (i in 1:n) {
  rho_denominator <- rho_denominator + residuals[i]^2
}

rho_hat <- rho_numerator / rho_denominator

cat("\nFirst-order autocorrelation coefficient (ρ):", round(rho_hat, 4), "\n")

# Relationship between d and ρ: d ≈ 2(1 - ρ)
cat("Approximation check: 2(1 - ρ) =", round(2*(1 - rho_hat), 4), "vs d =", round(d_stat, 4), "\n")

# ---------------------------- PLOTS ----------------------------

# Set up plotting area
par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))

# 1. Time series plot of residuals
plot(1:n, residuals, type = "l", col = "blue", lwd = 2, 
     main = "Residuals Time Series", xlab = "Observation", ylab = "Residuals")
abline(h = 0, col = "red", lwd = 1)
points(1:n, residuals, pch = 16, col = "darkblue")

# 2. Residuals vs Lagged residuals
plot(residuals[1:(n-1)], residuals[2:n], pch = 16, col = "darkgreen",
     main = "Residuals vs Lagged Residuals", 
     xlab = "Residuals (t-1)", ylab = "Residuals (t)")
abline(0, rho_hat, col = "red", lwd = 2)
legend("topleft", legend = paste("Slope =", round(rho_hat, 3)), col = "red", lwd = 2)

# 3. Actual vs Predicted values
plot(Y, Y_hat, pch = 16, col = "purple",
     main = "Actual vs Predicted Values", 
     xlab = "Actual Y", ylab = "Predicted Y")
abline(0, 1, col = "red", lwd = 2)

# 4. Autocorrelation function (manual calculation)
max_lag <- 10
acf_values <- numeric(max_lag)

for (lag in 1:max_lag) {
  numerator <- 0
  denominator <- 0
  
  for (i in (lag+1):n) {
    numerator <- numerator + residuals[i] * residuals[i-lag]
  }
  
  for (i in 1:n) {
    denominator <- denominator + residuals[i]^2
  }
  
  acf_values[lag] <- numerator / denominator
}

plot(1:max_lag, acf_values, type = "h", lwd = 3, col = "orange",
     main = "Residual Autocorrelation Function", 
     xlab = "Lag", ylab = "Autocorrelation", ylim = c(-1, 1))
abline(h = 0, col = "red", lwd = 1)
points(1:max_lag, acf_values, pch = 16, col = "darkorange")
abline(h = c(-2/sqrt(n), 2/sqrt(n)), col = "blue", lty = 2)  # Approximate 95% confidence bands

# Reset plotting parameters
par(mfrow = c(1, 1))

# ---------------------------- REMEDIAL MEASURES ----------------------------

cat("\nRemedial Measures for Autocorrelation:\n")
cat("======================================\n")

# Cochrane-Orcutt transformation
Y_star <- Y[2:n] - rho_hat * Y[1:(n-1)]
X_star <- X[2:n] - rho_hat * X[1:(n-1)]

# Fit transformed model
X_star_matrix <- cbind(1, X_star)
beta_star <- solve(t(X_star_matrix) %*% X_star_matrix) %*% t(X_star_matrix) %*% Y_star

cat("Cochrane-Orcutt Transformed Model:\n")
cat("Y* =", round(beta_star[1], 4), "+", round(beta_star[2], 4), "* X*\n")
cat("Where Y* = Y_t -", round(rho_hat, 4), "* Y_{t-1}\n")
cat("And X* = X_t -", round(rho_hat, 4), "* X_{t-1}\n\n")

# Compare original and transformed coefficients
cat("Comparison of Coefficients:\n")
cat("Original intercept:", round(beta_hat[1], 4), "\n")
cat("Transformed intercept:", round(beta_star[1]/(1 - rho_hat), 4), "\n")
cat("Original slope:", round(beta_hat[2], 4), "\n")
cat("Transformed slope:", round(beta_star[2], 4), "\n")

# ---------------------------- PREDICTION ----------------------------

# Forecast next value using the transformed model
last_Y <- Y[n]
last_X <- X[n]
forecast_X <- mean(X)  # Simple assumption for next X

# Using original model (potentially biased)
forecast_original <- beta_hat[1] + beta_hat[2] * forecast_X

# Using transformed model
forecast_transformed <- (beta_star[1]/(1 - rho_hat)) + beta_star[2] * forecast_X

cat("\nForecast Comparison:\n")
cat("Original model forecast:", round(forecast_original, 2), "\n")
cat("Transformed model forecast:", round(forecast_transformed, 2), "\n")

# ---------------------------- MODEL DIAGNOSTICS ----------------------------

cat("\nModel Diagnostics:\n")
cat("==================\n")

# Calculate R-squared
TSS <- sum((Y - mean(Y))^2)
R_squared <- 1 - RSS/TSS

# Calculate standard error
sigma_hat <- sqrt(RSS/(n - 2))

cat("R-squared:", round(R_squared, 4), "\n")
cat("Residual Standard Error:", round(sigma_hat, 4), "\n")
cat("Durbin-Watson statistic:", round(d_stat, 4), "\n")
cat("First-order autocorrelation:", round(rho_hat, 4), "\n")


#______________________Durbin & B_G________________________

# ===============================
# 0) Load CSV Data
# ===============================
# Read CSV file and extract variables
data <- read.csv("LAB-03 & 04 (ENM).csv")
data

X <- data$X           # Independent variable (predictor)
Y <- data$Y           # Dependent variable (response)
n <- length(Y)        # Number of observations
X_mat <- cbind(1, X)  # Add intercept column for manual regression

# ===============================
# 1) Linear Regression: Y = beta0 + beta1*X
# Formula: beta_hat = (X'X)^(-1) X'Y
# ===============================
XtX <- t(X_mat) %*% X_mat          # Compute X'X
XtY <- t(X_mat) %*% Y              # Compute X'Y
beta_hat <- solve(XtX) %*% XtY     # Solve for coefficients: beta0 and beta1

# Fitted values and residuals
Y_hat <- X_mat %*% beta_hat        # Predicted values of Y
res <- Y - Y_hat                   # Residuals = actual - predicted

# Residual standard error (sigma-hat)
s2 <- sum(res^2) / (n - 2)         # Variance of residuals
sigma_hat <- sqrt(s2)               # Residual standard error
sigma_hat
# Interpretation: smaller sigma_hat means model predictions are closer to actual values

# Standard errors of coefficients
SE <- sqrt(diag(s2 * solve(XtX)))  # SE(beta) = sqrt(sigma^2 * diag((X'X)^(-1)))
# Interpretation: smaller SE indicates more precise estimate of beta

# R-squared and Adjusted R-squared
R2 <- sum((Y_hat - mean(Y))^2) / sum((Y - mean(Y))^2)        # Proportion of variance explained
R2
p <- ncol(X_mat) - 1                                          # Number of predictors
Adj_R2 <- 1 - (1 - R2)*(n - 1)/(n - p - 1)                   # Adjusted for number of predictors
Adj_R2
# Interpretation: Adj_R2 accounts for model complexity; closer to 1 is better

# ===============================
# 2) Double-Log Regression: ln(Y) = beta0 + beta1*ln(X)
# ===============================
lnY <- log(Y)    # log of dependent variable
lnX <- log(X)    # log of independent variable

# Design matrix with intercept
X_mat_loglog <- cbind(1, lnX)

# Coefficients: beta_hat = (X'X)^(-1) X'Y
beta_hat_loglog <- solve(t(X_mat_loglog) %*% X_mat_loglog) %*% (t(X_mat_loglog) %*% lnY)
lnY_hat <- X_mat_loglog %*% beta_hat_loglog      # Predicted ln(Y)
res_loglog <- lnY - lnY_hat                      # Residuals

# Residual variance and standard error (sigma_hat)
s2_loglog <- sum(res_loglog^2)/(n-2)  
sigma_hat_loglog <- sqrt(s2_loglog)
sigma_hat_loglog

# Standard errors of coefficients
SE_loglog <- sqrt(diag(s2_loglog * solve(t(X_mat_loglog) %*% X_mat_loglog)))

# R-squared and Adjusted R-squared
R2_loglog <- sum((lnY_hat - mean(lnY))^2) / sum((lnY - mean(lnY))^2)
R2_loglog
p <- ncol(X_mat_loglog) - 1   # number of predictors
Adj_R2_loglog <- 1 - (1 - R2_loglog)*(n - 1)/(n - p - 1)
Adj_R2_loglog
# Interpretation: log-linear model may better fit if growth is exponential

# ===============================
# 3) Plots
# ===============================
# Linear regression plot
plot(X, Y, main="Linear Regression: Y ~ X", xlab="X", ylab="Y", pch=19, col="blue")
abline(beta_hat[1], beta_hat[2], col="red", lwd=2)
# Interpretation: red line is fitted linear model

# Double-log regression plot: log(Y) ~ log(X)
plot(lnX, lnY, main="Double-Log Regression: log(Y) ~ log(X)",
     xlab="log(X)", ylab="log(Y)", pch=19, col="blue")
abline(beta_hat_loglog[1], beta_hat_loglog[2], col="red", lwd=2)
# Interpretation:
# - Slope = elasticity
# - Shows percentage change relationship between Y and X

# ===============================
# 4) Durbin-Watson Statistic (DW)
# Formula: DW = sum(diff(res)^2)/sum(res^2)
# ===============================
DW <- sum(diff(res)^2)/sum(res^2)
# Interpretation:
# DW ≈ 2 → no autocorrelation
# DW < 2 → positive autocorrelation
# DW > 2 → negative autocorrelation

# Autocorrelation Tests for Double log model
# ===============================
# Durbin-Watson statistic
DW1 <- sum(diff(res_loglog)^2)/sum(res_loglog^2)

# ===============================
# 5) Breusch-Godfrey Test (1 lag)
# Tests for higher-order autocorrelation
# ===============================
res_curr <- res[-1]  # Residuals from 2nd observation
res_lag <- res[-n]   # Lagged residuals (1st to n-1)
X_bg <- cbind(1, X[-1], res_lag)                # Include intercept, X, lagged residual
beta_bg <- solve(t(X_bg) %*% X_bg) %*% (t(X_bg) %*% res_curr)  # Coefficients
res_bg <- res_curr - X_bg %*% beta_bg           # Residuals of BG regression
LM_stat <- (sum((res_curr - mean(res_curr))^2) - sum(res_bg^2)) / sum(res_bg^2) * (n-1)
# Interpretation: LM_stat large → evidence of autocorrelation

# Breusch-Godfrey test (1 lag) for Double log model
res_lag1 <- res_loglog[-n]
res_curr1 <- res_loglog[-1]
X_bg1 <- cbind(1, lnX[-1], res_lag)
beta_bg1 <- solve(t(X_bg) %*% X_bg) %*% (t(X_bg) %*% res_curr)
res_bg1 <- res_curr - X_bg %*% beta_bg
SSR_bg1 <- sum(res_bg^2)
SSR01 <- sum((res_curr - mean(res_curr))^2)
LM_stat1 <- (SSR01 - SSR_bg1)/SSR_bg1 * (n-1)

# ===============================
# 6) Coefficient Table (Linear)
# t-value and p-value
# t = beta_hat / SE, p = 2*(1-pt(|t|,df))
# ===============================
t_beta <- beta_hat / SE
p_beta <- 2*(1 - pt(abs(t_beta), df=n-2))

# Combine results
coef_table_linear <- data.frame(
  Estimate   = as.numeric(beta_hat),
  `Std. Error` = as.numeric(SE),
  `t value`  = as.numeric(t_beta),
  `Pr(>|t|)` = as.numeric(p_beta)
)

# Add row names
rownames(coef_table_linear) <- c("Intercept", "X")

# Print table
cat("===== Linear Regression Coefficient Table =====\n")
print(round(coef_table_linear,6))

# Interpretation:
# - Estimate: effect of X on Y
# - Std. Error: precision of estimate
# - t value & Pr(>|t|): test significance
# - Adj R²: goodness-of-fit adjusted for predictors
# - Sigma-hat: residual standard deviation

# ===============================
# 7) Coefficient Table (Double-Log)
# ===============================
# t-statistics
t_beta_loglog <- beta_hat_loglog / SE_loglog
# p-values
p_beta_loglog <- 2 * (1 - pt(abs(t_beta_loglog), df = n - 2))

# Combine into a table
coef_table_loglog <- data.frame(
  Estimate   = as.numeric(beta_hat_loglog),
  `Std. Error` = as.numeric(SE_loglog),
  `t value`  = as.numeric(t_beta_loglog),
  `Pr(>|t|)` = as.numeric(p_beta_loglog)
)

# Row names
rownames(coef_table_loglog) <- c("Intercept", "lnX")

# Print table
cat("\n===== Double-Log Regression Coefficient Table =====\n")
print(round(coef_table_loglog, 6))

# Interpretation:
# - lnX slope = elasticity: % change in Y for 1% change in X
# - Intercept = ln(Y) when lnX = 0
# - Sigma-hat = residual standard error on log scale
# - R2 & Adj R2 indicate goodness-of-fit

# ===============================
# Autocorrelation Test Table
autocorr_table <- data.frame(
  Test = c("Durbin-Watson", "Breusch-Godfrey (1 lag)"),
  Statistic = c(DW, LM_stat)
)

cat("\n===== Autocorrelation Test Table =====\n")
# Round only the numeric column
autocorr_table$Statistic <- round(autocorr_table$Statistic, 6)
print(autocorr_table)

# Interpretation:
# - DW ~2 → no autocorrelation
# - LM_stat large → indicates autocorrelation

# Autocorrelation table for Double log model
autocorr_table <- data.frame(
  Test      = c("Durbin-Watson","Breusch-Godfrey (1 lag)"),
  Statistic = c(DW1, LM_stat1)
)
autocorr_table$Statistic <- round(autocorr_table$Statistic,6)

cat("\n===== Autocorrelation Test Table =====\n")
print(autocorr_table)


#______________________________Ridge_Regression________________

# ===============================
# Load Hald Dataset
# ===============================
data <- read.csv("Hald Data.csv")
head(data)
# -----------------------------------
# Check first few rows of data
# Columns: X1-X4 (predictors), Y (response), RowSumX (derived)
# -----------------------------------

# ===============================
# Prepare Predictor Matrix
# ===============================
X <- as.matrix(data[, c("X1", "X2", "X3", "X4")])  # extract predictors
y <- data$Y                                      # response vector

# Add intercept column (1s) for regression
X <- cbind(1, X)  # X now has 5 columns: intercept + X1-X4

# Standardize predictors (important for Ridge)
X[,2:5] <- scale(X[,2:5])  
# -----------------------------
# Standardization ensures all predictors have mean=0 and SD=1
# This avoids penalization bias due to different scales
# -----------------------------

# ===============================
# Ridge Regression Calculation
# ===============================
lambda <- 1          # Ridge penalty parameter
p <- ncol(X)         # number of predictors (including intercept)

I <- diag(p)         # identity matrix
I[1,1] <- 0          # do not penalize intercept

# Compute Ridge coefficients manually
beta_ridge <- solve(t(X) %*% X + lambda * I) %*% t(X) %*% y
beta_ridge
# -----------------------------------
# Interpretation:
# Each coefficient is shrunk toward zero compared to OLS.
# Larger lambda → more shrinkage.
# Intercept is not penalized.
# beta_ridge[1] → intercept
# beta_ridge[2:5] → coefficients for X1-X4
# -----------------------------------
# ===============================
# Ridge Coefficients Interpretation
# ===============================

# Names of coefficients
coef_names <- c("Intercept", "X1", "X2", "X3", "X4")

# Ridge coefficients from your calculation
beta_vals <- as.vector(beta_ridge)

# Determine effect direction
effect <- ifelse(beta_vals > 0, "Positive effect on Y", "Negative effect on Y")

# Create textual interpretation
interpretation <- c(
  paste0("Intercept: predicted value of Y when all standardized predictors = 0 (~average of Y)"),
  paste0("X1: a 1 SD increase in X1 increases Y by ~", round(beta_vals[2],2), " units"),
  paste0("X2: a 1 SD increase in X2 increases Y by ~", round(beta_vals[3],2), " units"),
  paste0("X3: a 1 SD increase in X3 decreases Y by ~", abs(round(beta_vals[4],2)), " units"),
  paste0("X4: a 1 SD increase in X4 decreases Y by ~", abs(round(beta_vals[5],2)), " units")
)

# Combine in a data frame
ridge_table <- data.frame(
  Coefficient = coef_names,
  Value = round(beta_vals, 4),
  Effect = effect,
  Interpretation = interpretation
)

# Print the table
print(ridge_table)


