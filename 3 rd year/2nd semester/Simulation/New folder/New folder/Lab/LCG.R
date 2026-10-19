#---Linear Congruential Method from pseudo random number creating  random num:
set.seed(17)
n=200
a=17
x0=27
c<-43
m=100
r=numeric(n) #empty vector of n
r[1]=x0

for(i in 1:(n-1)){
  r[i+1]=(a *r[i]+c)%%m
}
r #list of random numbers

r=r/m
r
