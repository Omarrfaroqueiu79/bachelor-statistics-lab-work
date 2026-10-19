##-----lcg finding by function

LCG=function(n,m,a,c,x0){
  x=c()
  xn=x0
  for(i in 1:n){
    xn=(a*xn+c)%%m
    x[i]=xn
  }
  x=x/m
  return(x)
}
LCG(300,100,17,43,27)
