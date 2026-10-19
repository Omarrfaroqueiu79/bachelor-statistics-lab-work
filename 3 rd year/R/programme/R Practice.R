varx<- "Hello World"
print(varx)

vary<- "name"
class(vary)

ls()
rm(vary)
ls()

5-> var1
var2<- 8
print(var1+var2)
print(var1-var2)
print(var1/var2)
print(var1*var2)


print(var1==var2)
print(var1>var2)
print(var1<var2)
print(var1!=var2)
print(var1>=var2)
print(var1<=var2)
vara<- TRUE
varb<- FALSE
print(!vara)
print(vara & varb)
print(vara | varb)
isTRUE(varb)

#Dicision making
v<-5
if(v>4)
{
  print("true")
}

#######
var<- 5
if(var<4)(
  print("false")

)else
{
  print("true")
}

######
x<- switch(
  3,
  "first",
  "second",
  "third",
  "fourth"
)
print(x)

############loop

x<- LETTERS[1:11]
for (i in x) {
  print(i)
  
}

########
values<-c("while loop")
count<-2
while(count<7)
{
  print(values)
  count=count+1
}

##########
x<-c("Hello world")
count<-2
repeat{
  print(x)
  count<-count+1
  if(count>7){
    break
    
  }
}


#######
vector1<-c(6,2,1)
vector2<-c(10,16,22,43,15)
#vector as input to the array
result<-array(c(vector1,vector2),dim = c(3,3,2))
print(result)

#accessing array element
print(result[1,1,1])
print(result[3,3,2])

#manipulating array
mat1<-result[2,2,1]
print(mat1)
mat2<-result[3,3,1]
print(mat2)
print(mat1+mat2)

#Defining a function
aFunc<- function(a){
  for (i in 1:a) {
    b<-i*2
    print(b)
    
  }
}

#Calling a function
aFunc(4)


#Valid string

c<- "single quote ' in double quotea"
print(c)

d<- 'double quotes " in single quote'
print(d)

#Inva;id string
e<- 'Mixes Quotes in start and end"

print(e)

f<- 'single quotes ' inside single quotes'
print(f)


#manipulation

abc<- "Hello man"
def<- 'How'
ghi<- "are you feeling?"
print(paste(abc,def,ghi))
print(paste(abc,def,ghi, sep = "-"))
print(paste(abc, def,ghi, sep = "", collapse = NULL))


#Vector

#Atomic Vector of type Character
print("abc")
#Atomic vector of type double
print(12.5)
#Atomic vector of type integer
print(63L)
#Atomic vector of type logical
print(TRUE)
#Atomic vector of type Complex
print(2+4i)
#Atomic vector of type raw
print(charToRaw('hello'))

#Modifying Vectors

#Create two vectors
v1<-c(3,8,4,5,11)
v2<-c(4,11,8,1,2)

#Vector addition

add.result<-v1+v2
print(add.result)

#vector division
divi.result<-v1/v2
print(divi.result)

###Create a list containing strings, numbers, vectors and a logical values.

list_data<-list("Red", "Green", c(21,32,11), TRUE, 51.23, 119.1)
print(list_data)

#Access the first element of the list
print(list_data[1])
#Access the third element of the list
print(list_data[3])

#Give names to the elements in the list
names(list_data) <-c("1st quarter","A_matrix", "A inner list")
list_data

#Add elements at some position in the list
list_data[4]<- "New element"
print(list_data[4])

list_data[4]<- 5
print(list_data[4])

#Remove the element
list_data[4]<-NULL

#print the fourth element after remove
print(list_data[4])

#update the third element

list_data[3] <- "Update element"
print(list_data[3])


###Matrices###

P<-matrix(c(3:14),nrow=4, byrow=TRUE)
print(P)

#Access the element at 1st row and 3rd column
print(P[1,3])

#Access the element at 4th row and 2nd column
print(P[4,2])


Q<-matrix(c(14:25), nrow=4, byrow=T)

print(P+Q)
print(P-Q)
print(P*Q)
print(P/Q)

###Factors###

data<-c("East","West","East","North","North","East","West","West","West","East")

##Apply the factor function
factor_data<-factor(data)
print(factor_data)

##generating factor levels
##3 is the integer giving the number of levels.
##4 is a integer giving the number of replications
v<-gl(3,4, labels=c("abc","def","asc"))
print(v)

########Data Frame

employee.data<-data.frame(
  employee_id<-c(1:4),
  employee_name=c("Raj","Rohit","Amit","Sahil"),
  salary=c(60303,51606,61002,44440),
  stringsAsFactors = FALSE
  )
print(employee.data)

######Get structure of data
str(employee.data)
summary(employee.data)

######Adding column

employee.data$department<-c("Network","Mobile","Software","Web")
print(employee.data)


#####Data Reshaping Function
cbind()
rbind()
merge()

#create vector object
city<-c("delhi","bangalore","mumbai","chennai")
zipcode<-c(303602,918104,046161,580294)
oldAddress<-cbind(city,zipcode)
print(oldAddress)

#create another data frame with similar column

newAddress<- data.frame(
  city=c("Punjab","Chondigorh"),
  zipcode=c("802330","133949")
  
)
print(newAddress)

######Combine rows from both the data frames
totalAddresses<- rbind(oldAddress, newAddress)
print(totalAddresses)

#######Opening Files

data1<-iris
data1
print(is.data.frame(data1))

print(ncol(data1))
print(nrow(data1))
print(max(data1$Petal.Length))

maxDetails<-subset(data1, Petal.Length==max(Petal.Length))
print(maxDetails)

##make csv data
write.csv(maxDetails,"output.csv")
#read this csv data
newdata<-read.csv("output.csv")
newdata


# Create the data frame.
emp.data <- data.frame(
  emp_id = c (1:5), 
  emp_name = c("Rick","Dan","Michelle","Ryan","Gary"),
  salary = c(623.3,515.2,611.0,729.0,843.25), 
  
  start_date = as.Date(c("2012-01-01", "2013-09-23", "2014-11-15", "2014-05-11",
                         "2015-03-27")),
  stringsAsFactors = FALSE
)
# Get the structure of the data frame.
str(emp.data)

# Create the data frame.
emp.data <- data.frame(
  emp_id = c (1:5),
  emp_name = c("Rick","Dan","Michelle","Ryan","Gary"),
  salary = c(623.3,515.2,611.0,729.0,843.25),
  
  start_date = as.Date(c("2012-01-01","2013-09-23","2014-11-15","2014-05-11","2015-03-27")),
  stringsAsFactors = FALSE
)
emp.data
# Extract Specific columns.
result <- data.frame(emp.data$emp_name,emp.data$salary)
print(result)


# Extract first two rows.
result <- emp.data[1:2,]
print(result)

# Extract 3rd and 5th row with 2nd and 4th column.
result <- emp.data[c(3,5),c(2,4)]
print(result)

# Add the "dept" column.
emp.data$dept <- c("IT","Operations","IT","HR","Finance")
emp.data

install.packages("RCurl")
library(RCurl)
install.packages("RMySQL")
library(RMySQL)


###----R-Plotting---###

##Pie

slices<-c(10,12,4,16,8)
lbls<-c("Mango","Orange","Banana","Jack","Pine")
pie(slices,labels=lbls, main = "Pie chart of Fruits")


##-3-D-Pie-Chart--##

slices<-c(10,12,4,16,8)
lbls<-c("Mango","Orange","Banana","Jack","Pine")
pie(slices,labels=lbls,explode=0.1, main = "Pie chart of Fruits")

###Bar-Chart###

counts<-table(mtcars$gear)
barplot(counts,main="Car Distribution",xlab="Number of Gears")

###--Box plot---###
boxplot(mpg~cyl,data=mtcars,main="Car Milage Data", xlab="Number of Cylinders",ylab="Miles Per Gallon")


###--Histogram--###
hist(mtcars$mpg)

hist(mtcars$mpg,breaks = 9,col = "Blue")

###----Line-Graphs--###
f<-c(1:3)
g<-f #same data
par(pch=19,col="red") #plotting symbol and color
par(mfrow=c(2,4)) #all plots on one page
ops=c("p","l","o","b","c","s","S","h")
for (i in 1:length(ops)) 
{
  heading=paste("type=",ops[i])
  plot(f,g,type="n",main=heading)
  lines(f,g,type=ops[i])
}

###---Scatter-Plots--###
attach(mtcars)
plot(wt,mpg,main="Scatterplot Example",xlab="Car Weight",ylab="Miles per Gallon",pch=19)

###------Mean,Median-----###

n<-cars$speed
print(n)
mean(n)
median(n)

###-----Linear-Regression----###

#The predictor vector
p<-c(5.1,5.5,5.8,6.1,6.4,6.1,6.7,6.4,6.1,5.10,5.7)
#The response vector
q<-c(63,66,69,72,75,78,75,72,69,66)
#Apply the lm() function
relation<-lm(q~p)

######List#####
list1=list(28,'data science',c(10,TRUE,'abc'),FALSE)
print(list1)

list2=unlist(list1)
print(class(list1))

list3=append(list1,30)
print(list3)


list4=append(list1,30,after =1)
print(list4)


####################
install.packages("readxl")
library("readxl")
data=read_excel("")

 

v<-c(2,5.5,6)
t<-c(8,3,4)
print(v%%t)

###########
x<-matrix(c(2,3,5,6,7,8,9,10,11),nrow = 3,ncol = 3, byrow = FALSE)
print(x)
##
i<-matrix(c(1:3,3:1),nrow = 3,ncol = 2,byrow = FALSE)
print(i)
###
x[i]
###
x[i]<-0
print(x)
