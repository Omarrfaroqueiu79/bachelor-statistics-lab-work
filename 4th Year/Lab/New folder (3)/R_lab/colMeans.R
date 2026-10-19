


x = rnorm(10000)

y = matrix(x,nrow = 10)

d =dim(y)
Ls = rep(0,10)
for(i in 1:10){
  sums = 0
  for(j in 1:1000){
        sums = sums+abs(y[i,j])
  }
  Ls[i] = sums/d[2]
}



xx = rnorm(100)
xx = matrix(xx,ncol = 10)
dim(xx)
