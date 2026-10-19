setwd("C:/Users/Omor Faruk/Desktop/4-2")
# Problem_3
# Read the data 
places_tf=read.csv("places_tf.csv")

head(places_tf)

#Step 1: Load the Dataset
data=data.frame(places_tf[1:9])
data
data_std <- scale(data)  # Standardize the dataset
data_std

# Compute Correlation Matrix
cor_matrix <- cor(data_std)
cor_matrix

# Eigen Decomposition
eigen_decomp <- eigen(cor_matrix)
eigen_decomp
eigenvalues <- eigen_decomp$values
eigenvalues
eigenvectors <- eigen_decomp$vectors
eigenvectors

# Choose Number of Factors (Kaiser's Criterion: Eigenvalues > 1)
num_factors <- sum(eigenvalues > 1)

# Compute Initial Factor Loadings
loadings <- eigenvectors[, 1:num_factors] %*% diag(sqrt(eigenvalues[1:num_factors]))

# Optional: Perform Varimax Rotation
varimax_result <- varimax(loadings)
rotated_loadings <- varimax_result$loadings

# Compute Factor Scores
scores <- scale(data_std) %*% rotated_loadings

# Output Results
cat("Eigenvalues:\n")
print(eigenvalues)

cat("\nInitial Factor Loadings:\n")
print(loadings)

cat("\nRotated Factor Loadings:\n")
print(rotated_loadings)

cat("\nFactor Scores (First 6 Observations):\n")
print(head(scores))

