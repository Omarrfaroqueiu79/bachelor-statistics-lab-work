i=c(1,2,3,4,5,6,7,8,9,10)
x=c(108,112,117,130,111,131,113,113,105,128)
x
ri=sort(x)
ri
Fn1=(i-1)/10
Fn1

F0=pnorm(ri,120,10)
F0

Fn=i/10
Fn

Fn1minF0=abs(Fn1-F0)
Fn1minF0

F0minFn=abs(F0-Fn)
F0minFn
D=max(Fn1minF0,F0minFn)
D
install.packages('BoutrosLab.plotting.general')
library('BoutrosLab.plotting.general')

# critical_value=critical.value.ks.test(10,.10,alternative = 'two-sided')
# critical_value
