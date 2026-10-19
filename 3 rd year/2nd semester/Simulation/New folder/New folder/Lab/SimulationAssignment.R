#____________________OMAR FAROQUE___________________
#______________________ROLL:1822021_____________________
#______________________Session:(2018-19)___________________
### Random number generator from uniform distribution
set.seed(1822021)
n=5
d=runif(n)
print(d)
hist(d)

#______________Increasing Random Number_____________________
set.seed(1822021)
hist(runif(1000))

#_______Generate fixed random number from uniform distribution_____
set.seed(1822021)
set.seed(17)
num=10
runif(num)
hist(runif(num))

#_______Check are the numbers same or not_____
set.seed(1822021)
n=10
runif(n)
hist(runif(n))
plot(runif(n))
punif(n,min = 3,max = 10,lower.tail = T,FALSE)

#__________Random number using lcg_____________
set.seed(1822021)
lcg=function(N,x0,a,c,m){
  x=rep(0,N)
  x[1]=x0
  for (i in 2:N) x[i]=(a*x0+c)%%m
  R=x/m
  return(x)
  
}
lcg(11,33,2,4,100)

#_______Testing uniformity of Random Number_______
set.seed(17)
x <- runif(5000)
hist(x)
ks.test(x,"punif")

data=c(.4,.4,.4,.4,.4,.4,.4,.4)
ks.test(data,"punif")
