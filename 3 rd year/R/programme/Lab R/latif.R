#Question no 1(a)
#creating a vector
v=seq(1,50,by=1)
print(v)
#-----1(b)
A=matrix(v,nrow = 10,byrow = FALSE)
print(A)
#---1(c)-----slicing all row and column
s<-A[1:10,]
print(s)
col<-A[,1:5]
col
#------1(d)
#slice first two col and row
s1=A[1:2,]
s1
col1=A[,1:2]
col1
#------1(E)--
col2=A[,3:4]
col2
#-----------1(f)------------data frame
data=data.frame(s1,col1,col2
  
)
print(data)
#data improt
#from csv file
df=read.table(file.choose(),header = T,sep = ",")
df
df1=read.table(file.choose(),header = T,sep = "")
df1
#-------1(h)----
df1=head(df,11)
df1


#Answe 2(a)-------
data1=data.frame(
  id=1:3,
  name=c("l","s","p"),
  session=c(2018,2018,2017)
)
print(data1)
######2(b)------
data2=data.frame(
  id=1:4,
  name=c("l","s","p","r"),
  session=c(2018,2018,2017,2017)
)
print(data2)
#########------2(c)

id=1:4
name=c("l","s","p","r")
session=c(2018,2018,2017,2017)
rbind(id,name,session)
cbind(id,name,session)
#---------2(d)-----------
GPA=c(3.5,3.33,.55,3.70)
year=c(2018,2018,2018,2018)
data3=data.frame(data2,GPA,year)
print(data3)
###-------2(e)---------
ex=data3[3,]
ex
ex1=data3[3,5,]
ex1



###### answer3(a)
#call iris data
l=iris
l
#-------3(b)-----------
plot(l,main="Iris data",col=4,
     xlab=pairs,
     ylab=name,
     cex=.5
     )
### 3(c)--------------
####remove column
r=iris[,-5]
r
####regression 
model=lm(r$Sepal.Length~.,data = r)
model
summary(r)
names(model)
model$model
model$residuals
plot(model$residuals)
#forward 
forward=step(model,methods=forward)
forward
#backward
back=step(model,methods=backward)
back


##### answare 4 slr 
reg=lm(r$Sepal.Length~.,data = r)
reg
