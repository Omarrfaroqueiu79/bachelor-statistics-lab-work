#--------------integration-------

g=function(x){
  y=4*(x^2)
}

integrate(g,0,4)
#---------random number generate---from uniform dist---------
set.seed(42)
x=runif(10000,0,1)
x
gx=4*(x^2)
gx
integral=mean(gx)*(4-0)




plot(x)
hist(x)
r=runif(10000,30,60)
r
hist(r,col = 5)
