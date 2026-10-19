### Correlation matrix in R
#create data frame
df <- data.frame(x1=c(9, 2, 6, 5, 8),
                 x2=c(12, 8, 6, 4, 10),
                 x3=c(3, 4, 0, 2, 1))
#view data frame
df
# Construct a scatter plot of the data taking variable x1 an x2 and marginal dot diagrams. 
x1=df[,1]
x1
x2=df[,2]
x2
plot(x1,x2)
# infer the sign of the sample· covariance s12 from the scatter plot. 

# create mean vector or Calculate the sample means x1 and x2
mean_vector=as.matrix(colMeans(df))
mean_vector

 # create covariance matrix or write down the sample variances s11 and s22  and the sample covariance s12. 
cov(df)
#create correlation matrix and extract sample correlation coefficient r12
#  Interpret these quantities. 
cor(df)


########### Eigenvalues: Spectral Decomposition
# https://cran.r-project.org/web/packages/matlib/vignettes/eigen-ex2.html
A <- matrix(c(13, -4, 2, -4, 13, -2, 2, -2, 10), 3, 3, byrow=TRUE)
A
# Get the eigenvalues and eigenvectors for A
ev <- eigen(A)
ev
# extract components "eigen values" from ev
(L <- ev$values)
# extract components "eigen vectors" from ev
(V <- ev$vectors)

"""#Factorization of A: A = V diag(L) V’. That is, the matrix A
 can be represented as the product A=VΛV′"""

V %*% diag(L) %*% t(V)

diag(L)

# Spectral decomposition

""""The basic idea here is that each eigenvalue–eigenvector pair generates a rank 1 matrix, λiviv′i
, and these sum to the original matrix, A=∑iλiviv′"""


A1 = L[1] * V[,1] %*% t(V[,1])
A1

A2 = L[2] * V[,2] %*% t(V[,2])
A2

A3 = L[3] * V[,3] %*% t(V[,3])
A3


##Then, summing them gives A, so they do decompose A:

A1 + A2 + A3

all.equal(A, A1+A2+A3)


######### Singular Value Decomposition in R
# https://rpubs.com/aaronsc32/singular-value-decomposition-r
A = as.matrix(data.frame(c(4,7,-1,8), c(-5,-2,4,2), c(-1,3,-3,6)))
A

ATA <- t(A) %*% A
ATA

"""The V component of the singular value decomposition is then found by calculating the eigenvectors of the resultant ATA
 matrix."""

ATA.e <- eigen(ATA)
v.mat <- ATA.e$vectors
v.mat






############## Spectral Decomposition
# https://zief0002.github.io/matrix-algebra/spectral-decompostion.html

# Create A square matrix
A = matrix( data = c(-3, 4, 5, -2),  nrow = 2,ncol=2,byrow=FALSE)
A
# Compute eigenvalues and eigenvectors
# Spectral Decomposition of A
spec_decomp = eigen(A)
spec_decomp
# Extract eigen vectors as P
# Create P
P = spec_decomp$vectors
P
# Extract diagonal matrix D  as eigen values
# Create D 
D = diag(spec_decomp$values)
D
# Verify the decomposition
H=P %*% D %*% solve(P)
# are the decomposed matrix and original matrix same?
all.equal(A, H)


A <- matrix(c(13, -4, 2, -4, 13, -2, 2, -2, 10), 3, 3, byrow=TRUE)
A
# Compute eigenvalues and eigenvectors
# Spectral Decomposition of A
spec_decomp = eigen(A)
spec_decomp
# Extract eigen vectors as P
# Create P
P = spec_decomp$vectors
P
# Extract diagonal matrix D  as eigen values
# Create D 
D = diag(spec_decomp$values)
D
Q=P %*% D %*% solve(P)
# are the decomposed matrix and original matrix same?
all.equal(A, Q)



################# Singular Value Decompostion
# Create A
A = matrix(
  data = c(5, -4, 1, 2), 
  nrow = 2
  )
A
# Singular value decomposition
sv_decomp = svd(A)

# View results
sv_decomp

# Verify the results of the SVD
Q=sv_decomp$u %*% diag(sv_decomp$d) %*% t(sv_decomp$v)
all.equal(A, Q)

################## Cholesky Decomposition using R
# Create A
A = matrix(
  data = c(5, -4, -4, 5), 
  nrow = 2
  )

# Cholesky decomposition
cholesky_decomp = chol(A)

# View results
cholesky_decomp

## The output from chol() is actually the transpose matrix,  L⊺
# To obtain L, we need to take the transpose of this output. We can also verify that the decomposition worked.
# Obtain L
t(cholesky_decomp)

# Check results L(L^T) = A
t(cholesky_decomp) %*% cholesky_decomp


####### Example 5.l (Evaluating T 2 ) Let the data matrix for a random sample of size n = 3 
# from a bivariate normal population be 
# solve this probem manuaylly by R code
# page 214
###### Example 5.2 {Testing a multivariate mean vector with T2 ) 

### Hotelling T-squre tet
library(mvtnorm)
n <- 100 # observations
p <- 18 # parameter dimension
mu <- rep(0,p) # no signal: mu=0
x <- rmvnorm(n = n, mean = mu)
dim(x)


hotellingOneSample <- function(x, mu0=rep(0,ncol(x))){
  n <- nrow(x)
  p <- ncol(x)
  stopifnot(n > 5* p)
  bar.x <- colMeans(x)
  Sigma <- var(x)
  Sigma.inv <- solve(Sigma)
  T2 <- n * (bar.x-mu0) %*% Sigma.inv %*% (bar.x-mu0)
  p.value <- pchisq(q = T2, df = p, lower.tail = FALSE)
  return(list(statistic=T2, pvalue=p.value))
}
hotellingOneSample(x)





