
#### Finding the correlation of two vectors
v1=c(1,3,5,7)
v2=c(6,4,6,8)
# Finding average of vectors
sum1=0
for (i in v1){
  sum1=sum1+i
}
sum1

ave1=sum1/length(v1)
ave1

sum2=0
for (i in v2){
  sum2=sum2+i
}
sum2

ave2=sum2/length(v2)
ave2

## variances of the vectors
# variance of v1
# mean deviation 
dv1=(v1-ave1)^2
dv1
sum3=0
for (i in dv1){
  sum3=sum3+i
}
sum3

# variance of v1
var1=sum3/length(v1)
var1

# variance of v2
dv2=(v2-ave2)^2
dv2
sum4=0
for (i in dv2){
  sum4=sum4+i
}
sum4

var2=sum4/length(v2)
var2

## sum of product of v1 & v2
sp=dv1*dv2
sp

spsum5=0
for (i in sp){
  spsum5=spsum5+i
}
spsum5

# Standard deviation of vectors
sd1=sqrt(var1)
sd1

sd2=sqrt(var2)
sd2
# correlation r
r=spsum5/(sd1*sd2)
r

### Empty vector creation
# Sometimes you need to initialize an empty vector in R and fill it within a loop. 
#Whatever your needs, you can use the c function without specifying arguments. You could
#also use the vector function.
# Empty vector
my_vector <- c()

# Filling the vector using a for loop
for(i in 1:10) {
  m
my_vector

my_vector

### Create a function to print squares of numbers in sequence.
# Create a function to print squares of numbers in sequence.
new.function <- function(a) {
  for(i in 1:a) {
    b <- i^2
    print(b)
  }
}

# Call the function new.function supplying 6 as an argument.
new.function(6)


### Calling a Function without an Argument
# Create a function without an argument.
new.function <- function() {
  for(i in 1:5) {
    print(i^2)
  }
}	

# Call the function without supplying an argument.
new.function()

##sult <- a * b + c
  print(result)
}# Calling a Function with Argument Values (by position and by name)
# Create a function with arguments.
new.function <- function(a,b,c) {
  re

# Call the function by position of arguments.
new.function(5,3,11)

# Call the function by names of the arguments.
new.function(a = 11, b = 5, c = 3)
