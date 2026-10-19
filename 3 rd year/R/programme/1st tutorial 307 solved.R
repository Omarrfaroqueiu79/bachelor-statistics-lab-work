#1st tutuorial
#1_(a)
a<-c(10,12,4,3,6,78,65,21,24,26,21,20,23,14,27)
a
M<-matrix(a,nrow=5, byrow = FALSE)
M

#1_(b)
c1<-M[,2]
c1
c2<-M[,3]

#addition
p<-c1+c2

#subtraction
q<-c1-c2

#Multiplication
r<-c1*c2
p
q
r

#1_(c)
y<-M[,1]
y
x1<-M[,2]
x1
x2<-M[,3]
x2

model<-lm(y~x1+x2)
model
names(model)
model$residuals

print(model)
summary(model)
plot(model)

plot(y,x1)

data<-data.frame(y,x1,x2)
data
