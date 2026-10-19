v<-c(1,2,3,4)
print(v)
A=matrix(v,nrow =2,byrow=FALSE )
print(A)
A=matrix(c(1,2,3,4),nrow =2,byrow=TRUE )
A
#Matrix Operations..
A+A
A-A
#Matrix Multiplication
A%*%A
#Determinant
det(A)
print(diag(A))
print(sum(diag(A)))
e=eigen(A)
e
e$values
e$vectors


#Inverse Matrix
solve(A)
#Slice
A[1,1]
A[1,]
A[,1]
#Data Type
class(v)
v2=("Green")
v2
class(v2)
#Read in data
k=iris
k
#Name
names(k)
m=head(iris,12)
m[,-5]
s1=m[,1]
s1
s2=m[,3]
s2
X=cbind(s1,s2)
X
#Correlaton
cor(X)
#Regression
model1=lm(s1~s2)
model1
names(model1)
model1$residuals
summary(model1)
length(model1)
dim(model1)

v=seq(-5,5,by=0.2)
v
rep(v,times=5)


my_cor <- function(x, y){
  if(length(x) != length(y)) {
    stop("The length of x and y must be the same.")
  }
  n <- length(x)
  mean_x <- mean(x)
  mean_y <- mean(y)
  numerator <- sum((x - mean_x) * (y - mean_y))
  denominator <- sqrt(sum((x - mean_x)^2) * sum((y - mean_y)^2))
  corr <- numerator / denominator
  return(corr)






