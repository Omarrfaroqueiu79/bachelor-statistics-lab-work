# ===============================
# 0) Load CSV Data
# ===============================
# Read CSV file and extract variables
data <- read.csv("D:/STAT-4th Year/STAT-4th Year 1st Semester/LAB (STAT)/LAB (ENM)/LAB-03 & 04 (ENM).csv")
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

