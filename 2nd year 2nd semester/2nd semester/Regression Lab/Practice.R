# Create example data
rm(list = ls())
set.seed(87)
x <- rnorm(250)
y <- rnorm(250) + 2 *x
data <- data.frame(x, y)
# Print first rows of data
head(data)
# Install & load ggplot2
library("ggplot2")
# Create basic ggplot
# and Add regression line
ggp <- ggplot(data, aes(x, y)) +
  geom_point()
ggp
ggp +
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth")





#_______2_______#
rm(list = ls())
# Install & load ggplot2
library("ggplot2")
set.seed(87)
# Create example data
x <- rnorm(250)
y <- rnorm(250) + 2 *x
data <- data.frame(x, y)
reg<-lm(formula = y ~ x,
        data=data)
#get intercept and slope value
coeff<-coefficients(reg)
intercept<-coeff[1]
slope<- coeff[2]
# Create basic ggplot
ggp <- ggplot(data, aes(x, y)) +
  geom_point()
# add the regression line
ggp+geom_abline(intercept = intercept, slope = slope, color="red",
                linetype="dashed", size=1.5)+
  ggtitle("geeksforgeeks")


set.seed(1)
men_height<-rnorm(100,72,2)
men_height
hist(men_height,main = "men height",xlab="inches",ylab="Frequency",col="pink")


temp<-c(12,15,9,13,12,25,11,5,9,2,19,24,29,15,31)
sales<-c(350,400,250,410,380,1050,350,100,150,50,560,850,900,650,1160)
plot(temp,sales,main = "scatter diagram",xlab="Temperature",ylab="Sales",col.lab="red",pch=21,cex=1.5,col="yellow",bg="green",lwd=2)

set.seed(1)
A<-rnorm(100,4,2)
B<-rnorm(100,6,1)
C<-rnorm(100,5.2,1.2)
df<-data.frame(A,B,C)
df
#head(df,10)
#tail(df,12)
head(df)
boxplot(df)


h<-matrix(c(1,2,3,4),nrow = 2,ncol = 2,byrow = T)
h
h[2,2]


a<-matrix(c(4,5,8,3),nrow = 2,ncol = 2,byrow = T)
a
a[1,2]
a[1,]
a[,2]
a[,]


#_____new_____
x1<-c(1,2,3,5,7)
x2<-c(3,4,5,6,2)
x3<-c(6,7,4,6,8)
x4<-c(4,5,6,7,2)

y<-c(4,5,6,7,8)
k<-length(x1)
k

#_____creating data frame____
dataf<-cbind(x1,x2,x3,x4)
dataf
dataf2<-rbind(x1,x2,x3,x4)
dataf2

df<-data.frame(x1,x2,x3,x4)
df

d<-df[,1:2]

d

d2<-df[1,]

reg<-lm(x1~x2)

reg


summary(reg)

beta<-solve(t(df)%*%df)%*%t(df)%*%y

#######
df2<-data.frame(Name=c("H","O","J","F"),Roll=c(40,21,04,12))
df2

