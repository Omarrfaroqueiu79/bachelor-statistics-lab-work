x = rexp(n=100, rate = 2)

ks.test(x,"pnorm")


y = rnorm(n=1000)
z = rnorm(n=1000)


ks.test(y,"pnorm")


ks.test(y,z)

