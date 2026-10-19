x=4
y=6
x+y
#gfhgfdhhhhhhh
v=c(1,2,3,4)
print(v)
A=matrix(v,nrow = 2,byrow = T)
A
asquare=A%*%A
asquare
det(A)
## inverse
solve(A)
vv=c(1,2,3,4)
print(vv)
AA=matrix(vv,ncol=1,byrow =T)
AA

A[1,1]
A[1,]
A[,1]
A
class(A)
class(v)
names(A)
View(cars)
names(cars)

A[1,1]
A[1,]
A[,1]
A[,2]
#read in r
iris
d=head (iris,12)
d
e<-tail(iris,12)
e
f<-d[,1]
f1<-d[,3]
f
print(f1)
cbind(f,f1)
rbind(f,f1)
p=cor(f,f1)
p
model1<-lm(f~f1)
print(model1)
names(model1)
model1$effects
summary(model1)
length(iris)
dim(d)

plot(p)

plot(iris,col=484)
#read data
swiss
View(swiss)

model2=lm(swiss$Fertility~.,data = swiss)
print(model2)
summary(model2)



#forward selection
forward=step(model2,method=forward)
print(forward)
backward=step(model2,method=backward)
print(backward)
