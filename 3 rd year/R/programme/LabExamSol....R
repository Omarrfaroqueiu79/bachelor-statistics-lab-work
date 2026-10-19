#1:
a1<-seq(1:50)
a1

a<-seq(11,50,3)
a
#####
vec<-c(seq(11,50,by=3))
vec
#a:
vec1<-c(seq(11,50,3))
vec1
print(vec1)

#b
vec2<-c(seq(1,50,1))
vec2

matrix1<-matrix(vec2,nrow=10,ncol=5,byrow = F)
matrix1

#c
matrix1[,]

#d
matrix1[,1:2]
#or
matrix1[,c(1,2)]

#e

matrix1[,3:4]
#or
matrix1[,c(3,4)]


#f
data<-data.frame(
  d<-matrix1[,c(1,2)],
  e<-matrix1[,c(3,4)]
  
  
)
data




#-------------------------------------------------
#2number:
#a
data1<-data.frame(
  Roll<-c(04,05,06),
  Name<-c("Tanvir","Parvez","Rakib"),
  session<-c(2018,2018,2018)
)
data1
#b:

data2<-data.frame(
  Roll<-c(08,09,10),
  Name<-c("shahed","ripon","poritosh"),
  session<-c("2017-18","2017-18","2017-18")
)
data2

#c:
#combine vertically:
cbind(data1,data2)
#Horizontal:
rbind(data1,data2)



#------------------------------------
#3

#a

print(iris)
#or 
iris

#b:

plot(iris)
plot(iris$Sepal.Length,iris$Sepal.Width,
     main="sepal.Length vs sepal.width scatter plot",
     col="blue",
     cex=1.5,
     xlab="sepal.Length",
     ylab="width",
     
     
     )

#c
#remove
iris1<-iris[,-5]
iris1
#regression:
model1<-lm(iris1$Sepal.Length~.,data=iris1)
model1
summary(model1)

#forward selection:
forwarddata<-step(model1,direction = "forward")
summary(forwarddata)

#interpret coefficient:

forwarddata$coefficients
