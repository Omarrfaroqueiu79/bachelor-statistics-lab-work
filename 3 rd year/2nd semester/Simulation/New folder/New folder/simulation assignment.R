#----------------------MD LOTIFUR RAHMAN--------------------------
#----------------------ROLL:1822017--------------------------
#----------------------Session:2018-19----------------------
### Random number generator from uniform distribution
n=5
d=runif(n)
print(d)
hist(d)
#------------------increasing random number---------

hist(runif(1000))
#generate fixed random number from uniform distribution
set.seed(17)
num=10
runif(num)
hist(runif(num))
#Check is the numbers are same or not
set.seed(17)
n=10
runif(n)
hist(runif(n))
plot(runif(n))
punif(n,min = 3,max = 10,lower.tail = T,FALSE)

#-------Random number using lcg---------##
set.seed(17)
lcg=function(N,x0,a,c,m){
  x=rep(0,N)
  x[1]=x0
  for (i in 2:N) x[i]=(a*x0+c)%%m
  R=x/m
  return(x)
  
}
lcg(11,33,2,4,100)

#-----------Testing uniformity of RN-----------
set.seed(17)
x <- runif(5000)
hist(x)
ks.test(x,"punif")

data=c(.4,.4,.4,.4,.4,.4,.4,.4)
ks.test(data,"punif")







