#------------------------Problem_1----------------

# Construct matrix and mean vector, covariance matrix, and correlation matrix 
# from the matrix.

#Creating data frame (df) from given data.
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
# Plotting scatter plot with the first and second column
plot(x1,x2)


# Calculating sample mean and creating mean vector.

mean_vect=as.matrix(colMeans(df))
mean_vect
# Creating the covariance matrix for given data frame
cov(df)
# Creating the correlation matrix for the given data frame
cor(df)





#----------------------Problem_2-------------------------

# Spectral decomposition for the given data.
S=matrix(c(13, -4, 2, -4, 11, -2, 2, -2, 8),3,3,byrow = TRUE)
print(S)
# Calculating the eigenvalues and eigenvectors for the given matrix.

eig=eigen(S)
# Find the eigen vectors or creating the P

P=eig$vectors
# Find the eigan values or creating the V vector

v=eig$values
v
# Creating the diagonal matrix form eigen values

D=diag(eig$values)

# The formula of the spectral decomposition (PDP-1)

# Matrix Factorization

#-1 Factorization of S: S = P diag(D) P’. That is, the matrix S
#can be represented as the product 

P%*% D%*% t(P)
#-2: V diagonalizes S: D = P’ S P. That is, the matrix V transforms A
#into the diagonal matrix D, corresponding to orthogonal (uncorrelated) variables.
print(D)
zapsmall(t(P)%*% S %*% P)
# Spectral Decomposition 
#The basic idea here is that each eigenvalue–eigenvector pair generates a rank 1
#matrix, λiviv′i, and these sum to the original matrix, A=∑iλiviv′i
s1=v[1]* P[,1]%*% t(P[,1])
s1
s2=v[2]* P[,2]%*% t(P[,2])
s2
s3=v[3]* P[,3]%*% t(P[,3])
s3
#Then, summing them gives S, so they do decompose S:

s1+s2+s3
all.equal(S,s1+s2+s3)




# ------------------------Problem_3-------------------

# Singular value decomposition using function (svd):

# Creating a matrix
A = data.frame(
  c1=c(4,7,-1,8), 
  c2=c(-5,-2,4,2), 
  c3=c(-1,3,-3,6))
A
# the singular value decomposition is
decom=svd(A)
print(decom)
# Here verifying the results
R=decom$u %*% diag(decom$d) %*% t(decom$v)
R
all.equal(A,R)
#or
B = matrix(
  data = c(5, -4, 1, 2), 
  nrow = 2,byrow = TRUE)

B
# Singular value decomposition
sv_decomp = svd(A)

# View results
sv_decomp
# Verify the results of the SVD
Q=sv_decomp$u %*% diag(sv_decomp$d) %*% t(sv_decomp$v)
all.equal(B,Q)




#------------------------Problem_4--------------------

# Hotelling t-square related problem 5.1: Let the data matrix for a random sample
# of size n=3 from a bivariate normal. Evaluate the observed T^2 for mu\prime_0=
#[9,5]. What is the sampling distribution of T^2 in this case?
# Creating the matrix.

n=3
p=2
men_prime=as.matrix(c(9,5))
men_prime
X=as.matrix(data.frame(x1=c(6,10,8),
                       x2=c(9,6,3)))
X
# the sample mean vector
men=as.matrix(colMeans(X))
men
# Find the covariance matrix for the given matrix
s=cov(X)
s
# The inverse matrix of the covariance matrix.
s_invers=solve(s)
print(s_invers)
# Finding the mean difference
mean_diff=men-men_prime
mean_diff
transpose=t(mean_diff)
transpose
# hotelling t^2 
T_2=n*transpose%*%s_invers%*%mean_diff
T_2
# Finding the critical value for F statistic at 10% level of significance.
F_critical_value=qf(.10,df1 = p,df2 = n-p,lower.tail = FALSE)
F_critical_value
# Now calculated the Tabulated T^2 value
T_table=(((n-1)*p)/(n-p))*F_critical_value
T_table
# Comment:   
# There is evidence to reject Null hypothesis because calculated T^2 is smaller 
# than tabulated value.

#----------------------------Problem_5--------------------------

# Perspiration from 20 healthy females was analyzed. Three components, X1=sweet raster Image()
# x2=sodium content, and x3= Potassium content, were measured, and the results, which 
# recall the sweat data, are presented.
# The test hypothesis H_0: mu_prime =[4,50,10] against=H_1:mu is not equal to given mean
# at the label of significance 10%

# the mean vector

# the variance and covariance matrix
n=20
p=3
mu=as.matrix(c(4,50,10))
mu
df=data.frame(
  X1=c(3.7,5.7,3.8,3.2,3.1,4.6,2.4,7.2,6.7,5.4,3.9,4.5,3.5,
       4.5,1.5,8.5,4.5,6.5,4.1,5.5),
  X2=c(48.5,65.1,47.2,53.2,55.5,36.1,24.8,33.1,47.4,54.1,36.9,
       58.8,27.8,40.2,13.5,56.4,71.6,52.8,44.1,40.9),  
  X3=c(9.3,8.0,10.9,12.0,9.7,7.9,14.0,7.6,8.5,11.3,12.7,12.3,
       9.8,8.4,10.1,7.1,8.2,10.9,11.2,9.4)
)
#View(df)
x_bar=as.matrix(colMeans(df))
x_bar
# Finding the variance and covariance matrirx
S=cov(df)
S
# Finding the inverse of variance and covariance matrix
S_inv=solve(S)
S_inv
mu_diff=x_bar-mu
mu_diff
# mean diff transpose 
mu_diff_tanspose=t(mu_diff)
mu_diff_tanspose
# Observed hotelling T^2
T_sq=n*mu_diff_tanspose%*%S_inv%*% mu_diff
T_sq
#Finding the tabulated value for F at the level of significance 10%
F_tab=qf(.10,df1 = p,df2 = n-p,lower.tail = FALSE)
F_tab
# critical value
F_crit=(((n-1)*p)/(n-p))*F_tab
F_crit
# Comment:
# We see that T^2=9.74 >8.18, and consequently, we reject null hypothesis at the 
# 10% level of significance.
