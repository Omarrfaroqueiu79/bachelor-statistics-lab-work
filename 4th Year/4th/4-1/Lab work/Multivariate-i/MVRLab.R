#_________________Problem_1______________________
# Construct matrix, Extract and Plot first two column, covariance matrix, and correlation matrix 
#Creating data frame (df) from given data
df=data.frame(
  x1=c(9, 2, 6, 5, 8),
  x2=c(12, 8, 6, 4, 10),
  x3=c(3, 4, 0, 2, 1))
df
as.matrix(df)
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
m=as.matrix(c(m1,m2))
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
#create matrix
S=matrix(c(13, -4, 2, -4, 13, -2, 2, -2, 10),3,3,byrow = TRUE)
print(S)

# Calculating the eigenvalues and eigenvectors for the given matrix.
ev=eigen(S)
ev
#Extract eigenvectors as P
P=ev$vectors
P
#Extract eigenvalues as Q and make diagonal as D
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

K=S1 + S2 + S3
K
#Verify
all.equal(S, K)

#_____________Problem_3________________
#Singular Value Decomposition
# Creating a matrix
A = as.matrix(data.frame(
  c1=c(4,7,-1,8), 
  c2=c(-5,-2,4,2), 
  c3=c(-1,3,-3,6)))
A
# the singular value decomposition is
decom=svd(A)
decom


#Here verifying the results
R=decom$u %*% diag(decom$d) %*% t(decom$v)
R

all.equal(A,R)

#Or
B = matrix(
  data = c(5, -4, 1, 2), 
  nrow = 2
)
B
# Singular value decomposition
sv_decomp = svd(B)

# View results
sv_decomp

# Verify the results of the SVD
Q=sv_decomp$u %*% diag(sv_decomp$d) %*% t(sv_decomp$v)
all.equal(B,Q)

#_____________Problem_4________________
# Example 5.l (Evaluating T 2 ) Let the data matrix for a random sample of size n = 3 
# from a bivariate normal population. Evaluate the observed T^2 for mu\prime_0=
#[9,5]. What is the sampling distribution of T^2 in this case?
# solve this problem manually by R code

# Creating the matrix
n=3
p=2
mu0=as.matrix(c(9,5))
mu0
X=as.matrix(data.frame(x1=c(6,10,8),
                       x2=c(9,6,3)))
X
# the sample mean vector
x_bar=as.matrix(colMeans(X))
x_bar
# Find the covariance matrix for the given matrix
s=cov(X)
s
# The inverse matrix of the covariance matrix.
s_invers=solve(s)
s_invers
# Finding the mean difference
mean_diff=x_bar-mu0
mean_diff
transpose=t(mean_diff)
transpose

#Hotelling T^2 
T_2=n*transpose%*%s_invers%*%mean_diff
T_2
# Finding the tabulated value for F statistic at 10% level of significance.
F_tab_value=qf(.10,df1 = p,df2 = n-p,lower.tail = FALSE)
F_tab_value
# Now calculate the critical value
F_critical=(((n-1)*p)/(n-p))*F_tab_value
F_critical

#Comment:
#Since calculated T^2 is smaller than critical value.
#There is evidence to reject Null hypothesis

#__________________Problem_5______________________________

# Perspiration from 20 healthy females was analyzed. Three components, X1=sweet rasterImage()
# x2=sodium content, and x3= Potassium content, were measured, and the results, which 
# we call the sweat data, are presented.
# The test hypothesis H0: mu_prime =[4,50,10] against=H1:mu is not equal to given mean
# at the label of significance 10%

#Creating matrix
n=20
p=3
mu=as.matrix(c(4,50,10))
mu
df=data.frame(
  X1=c(3.7,5.7,3.8,3.2,3.1,4.6,2.4,7.2,6.7,5.4,3.9,4.5,3.5,4.5,1.5,8.5,4.5,6.5,4.1,5.5),
  X2=c(48.5,65.1,47.2,53.2,55.5,36.1,24.8,33.1,47.4,54.1,36.9,58.8,27.8,40.2,13.5,56.4,71.6,52.8,44.1,40.9),  
  X3=c(9.3,8.0,10.9,12.0,9.7,7.9,14.0,7.6,8.5,11.3,12.7,12.3,9.8,8.4,10.1,7.1,8.2,10.9,11.2,9.4)
)
df
# the sample mean vector
x_bar=as.matrix(colMeans(df))
x_bar
# Finding the variance and covariance matrix
S=cov(df)
S
# Finding the inverse of variance and covariance matrix
S_inv=solve(S)
S_inv

mu_diff=x_bar-mu
mu_diff
#transpose of mean diff 
mu_diff_tanspose=t(mu_diff)
mu_diff_tanspose

#Observed hotelling T^2
T_sq=n*mu_diff_tanspose%*%S_inv%*% mu_diff
T_sq

#Finding the tabulated F value at the level of significance 10%
F_tab=qf(.10,df1 = p,df2 = n-p,lower.tail = FALSE)
F_tab
# critical value
F_crit=(((n-1)*p)/(n-p))*F_tab
F_crit

#Comment:
#we see that T2 = 9.74 > 8.18, and consequently, we reject H0 at the 10% level of significance.