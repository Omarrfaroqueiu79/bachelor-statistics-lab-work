setwd("C:/Users/Omor Faruk/Desktop/4-2")
# Problem_2
# Read the data 
places_tf=read.csv("places_tf.csv")
View(places_tf)
data=data.frame(places_tf[1:9])
data


# Standardize the Data
data_std <- scale(data)  # Center and scale the data

#  Compute Covariance Matrix
cov_matrix <- cov(data_std)
cov_matrix
# Perform Eigen Decomposition
eigen_results <- eigen(cov_matrix)
eigenvalues <- eigen_results$values  # Variance explained by each component
eigenvalues
eigenvectors <- eigen_results$vectors  # Principal components (eigenvectors)
eigenvectors

#  Calculate Explained and Cumulative Variance
explained_variance <- eigenvalues / sum(eigenvalues)  # Proportion of variance explained
explained_variance
cumulative_variance <- cumsum(explained_variance)     # Cumulative variance explained
cumulative_variance

#  Scree Plot
plot(
  explained_variance,
  type = "b",                        # Line and points
  xlab = "Principal Component",
  ylab = "Variance Explained",
  main = "PCA Scree Plot",
  pch = 19,                         # Point shape
  col = "blue"
)


# Transform Data into Principal Component Space
# Choose the number of components (e.g., retain components explaining 90% variance)
num_components <- which(cumulative_variance >= 0.90)[1]  # First PC explaining >= 90%
num_components
principal_components <- eigenvectors[, 1:num_components]  # Select top PCs
principal_components

data_pca <- as.matrix(data_std) %*% principal_components  # Project data
data_pca
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

