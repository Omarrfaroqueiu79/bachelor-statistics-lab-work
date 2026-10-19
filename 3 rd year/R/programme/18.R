
#2018
#2-c

#i
a<-c(4,5,6,7,5,6,9,4,3)
#ii
summary(a)
#iii
sort(a, decreasing = TRUE)
#iv
A<-matrix(a, nrow = 3, ncol = 3, byrow = TRUE)
A
#v
A.square<-A%*%A
A.square
#vi
det(A)
solve(A)
eigen(A)
#vii
r1<-A[1,]
r1
r2<-A[2,]
r2
r3<-A[3,]
r3
#viii
c1<-A[,1]
c1
c2<-A[,2]
c2
c3<-A[,3]
c3
#ix
diag(A)
#x
sum(diag(A))



#3(b)
x<-c(2,3,4,8,9)
y<-c(1,4,5,6,4)
l<-lm(y~x)
summary(l)
names(l)
l$coefficients
l$residuals
l$effects
l$fitted.values
l$df.residual
l$model
l$qr



