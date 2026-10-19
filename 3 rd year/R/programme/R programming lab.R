#------------consideration of the following numbers -------------------
#---------4,5,6,7,5,6,9,4,3----------------
#1 Construct a vector 
v<-c(4,5,6,7,5,6,9,4,3)
print(v)

#2 Find the descriptive statistics 
summary(v)

# 3_Arrange the data descending oder 
sort(v,decreasing =TRUE)

# 4_Construct a three by three oder matrix-----
A<-matrix(v,nrow = 3,byrow = TRUE)
print(A)

#Considering A matrix find out A square matrix

A_square<-A%*%A
print(A_square)
#---------find out the determinant ,inverse and eigen value and eigen vector of the matrix
det(A)
solve(A)
e<-eigen(A)
e$values
e$vectors

#Row names and column names
rownames(A)<-c("row1","row2","row3")
rownames(A)
print(A)
colnames(A)<-c("C1","C2","C3")
colnames(A)
print(A)

#From the A matrix write a command for first row ,second row,third row
row1<-A[1,]
print(row1)
row3<-A[3,]
print(row3)

#column
c1<-A[,1]
print(c1)
#-------show diagonal elements of matrix A-----------
diag(A)

#trace of A
sum(diag(A))

####----------- linear Regression---------------------- 
#finding all components of regression 
x<-c(2,3,4,8,9)
y<-c(1,4,5,6,4)

reg<-lm(y~x)
print(reg)
summary(reg)
names(reg)

reg$coefficients
reg$residuals
reg$model
## Correlation between x and y 

cor<-cor(x,y)
print(cor)
plot(cor)
#---------data frame----------------
data<-data.frame(
  
     id<-1:3,
     name<-c("L","R","S"),
     age<-c(23,44,48)
     
)
print(data)
print(A)
r1<-A[1,]
r1
r2<-A[2,]
r2
r3<-A[3,]
bind<-rbind(r1,r2,r3)
bind
#-----------cbind---------
bind2<-cbind(r1,r2,r3)
bind2








#data insert from different extension
#
#--- read.table()

df <- read.table("chick_data.csv",header=T,sep=",")
df <- read.table(file.choose(),header = T,sep = ",")
df[1:5,]
dim(df)

# read.csv/csv2
df2 <- read.csv("ChickData.csv",header = T)
names(df)
# a

# ---- txt -------------
df3 <- read.table(file.choose(),header=T,sep = "\t")
View(df3)







