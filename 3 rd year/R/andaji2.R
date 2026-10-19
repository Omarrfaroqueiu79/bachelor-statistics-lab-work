d<-matrix(c(2,4,6,8),nrow = 2,ncol = 2,byrow = F)
d
df=data.frame(c)
df
t(d)
d
inverse=solve(d)
d
beta<-solve(t%*%d)%*%t*y
beta
summary(beta)


x1<-c(1,2,3,5,7)
x2<-c(3,4,5,6,2)
x3<-c(6,7,4,6,8)
x4<-c(4,5,6,7,2)
y<-c(4,5,6,7,8)
#Creating data frame
df1=cbind(x1,x2,x3,x4)
df1
df2=rbind(x1,x2,x3,x4)
df2
#Draw a data frame
df=data.frame(x1,x2,x3,x4)
df
#slice first two variables of my data table
df1=df[,1:2]
df
df2=df[1,]
df
regression=lm(x1~x2)
regression
summary(regression)

____________1(b)___________
mydata<-rep(LETTERS[1:5],c(7,4,2,9,11))
mytable<-table(mydata)
mytable
barplot(mytable,main = "BP",xlab = "RL",ylab = "OC",col = "black",border = "red")
mytable1<-sort(mytable,decreasing = T)
mytable1





set.seed(40)
men_height<-rnorm(100,72,2)
men_height
hist(men_height,main = "Hist of men_height",xlab="Height in inches",ylab="Frequency",col="Green",border = "Red")


#scatter diagram
temp<-c(12,15,9,13,12,25,11,5,9,2,19,24,29,15,31)
sales<-c(350,400,250,410,380,1050,350,100,150,50,560,850,900,650,1160)
plot(temp,sales,main = "SCD",pch="21",cex="0.2",col="red",bg="Black",lwd="3",xlab = "Tempareture in celcius",ylab = "ICS",col.lab="blue")


_____________Boxplot______________
set.seed(40)
A<-rnorm(100,4,2)
B<-rnorm(100,6,1)
C<-rnorm(100,5.2,1.2)
df<-data.frame(A,B,C)
boxplot(df)


__________ggplot____________
#importing essential libraries
library(dplyr)
#load the data
