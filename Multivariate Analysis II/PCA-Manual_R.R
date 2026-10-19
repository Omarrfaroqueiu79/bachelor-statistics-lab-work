# Principal Component Analysis (PCA) with Scree Plot

library(readr)
places_tf <- read_csv("C:/Users/Poritosh Biswas/Downloads/places_tf.csv")

data=data.frame(places_tf[1:8])
data
# Step 1: Load the Dataset
# data <- mtcars[, c("mpg", "hp", "wt", "qsec")]  # Example dataset


# Step 2: Standardize the Data
data_std <- scale(data)  # Center and scale the data

# Step 3: Compute Covariance Matrix
cov_matrix <- cov(data_std)
cov_matrix
# Step 4: Perform Eigen Decomposition
eigen_results <- eigen(cov_matrix)
eigenvalues <- eigen_results$values  # Variance explained by each component
eigenvectors <- eigen_results$vectors  # Principal components (eigenvectors)

# Step 5: Calculate Explained and Cumulative Variance
explained_variance <- eigenvalues / sum(eigenvalues)  # Proportion of variance explained
cumulative_variance <- cumsum(explained_variance)     # Cumulative variance explained

# Step 6: Scree Plot
plot(
  explained_variance,
  type = "b",                        # Line and points
  xlab = "Principal Component",
  ylab = "Variance Explained",
  main = "PCA Scree Plot",
  pch = 19,                         # Point shape
  col = "blue"
)


# Step 7: Transform Data into Principal Component Space
# Choose the number of components (e.g., retain components explaining 90% variance)
num_components <- which(cumulative_variance >= 0.90)[1]  # First PC explaining >= 90%
principal_components <- eigenvectors[, 1:num_components]  # Select top PCs
data_pca <- as.matrix(data_std) %*% principal_components  # Project data

# Output Results
cat("Eigenvalues:\n")
print(eigenvalues)
cat("Explained Variance (Proportion):\n")
print(explained_variance)
cat("Cumulative Variance:\n")
print(cumulative_variance)
cat(paste("Number of Components Retained:", num_components, "\n"))
cat("Transformed Data (Top Principal Components):\n")
print(data_pca)


