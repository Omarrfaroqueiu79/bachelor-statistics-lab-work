#_________________Problem_1______________________
# Construct matrix, covariance matrix, and correlation matrix 
# Creating data frame (df) from given data.
df=data.frame(
  x1=c(9, 2, 6, 5, 8),
  x2=c(12, 8, 6, 4, 10),
  x3=c(3, 4, 0, 2, 1))
print(df)
# Extract the first column 
x1=df[,1]
x1
# Extract the second column
x2=df[,2]
x2
# Plotting scatter plot with first and second column
plot(x1,x2)
#mean vector of first and second column
m1=mean(df$x1)
m2=mean(df$x2)
m=c(m1,m2)
m
#Calculating sample mean and creating mean vector
mean_vect=as.matrix(colMeans(df))
mean_vect
#Creating the covariance matrix for given data frame
cov(df)
# Creating the correlation matrix for the given data frame
cor(df)


#___________________Problem_2_________________________
# Spectral decomposition for the given data.
S=matrix(c(13, -4, 2, -4, 13, -2, 2, -2, 10),3,3,byrow = TRUE)
print(S)

# Calculating the eigenvalues and eigenvectors for the given matrix.
ev=eigen(S)
ev
#Extract eigenvectors as P
P=ev$vectors
P
#Extract eigenvalues and make diagonal D
Q=ev$values
Q
D=diag(Q)
D
#___verifying spectral decomposition__ 
V=P %*% D %*% solve(P)
V
#Are the decomposed matrix and original matrix same?
all.equal(S, V)

#""""The basic idea here is that each eigenvalue–eigenvector pair generates a rank 1 matrix, λiviv′i,
#and these sum to the original matrix, A=∑iλiviv′"""

S1 = Q[1] * P[,1] %*% t(P[,1])
S1

S2 = Q[2] * P[,2] %*% t(P[,2])
S2

S3 = Q[3] * P[,3] %*% t(P[,3])
S3


##Then, summing them gives S, so they do decompose S:

S1 + S2 + S3
#Verify
all.equal(S, S1+S2+S3)

#_____________Problem_3________________
#Singular Value Decomposition




