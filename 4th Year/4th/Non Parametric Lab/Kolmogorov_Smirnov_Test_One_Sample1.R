i=c(1,2,3,4,5,6,7,8)
x=c(1.41, 0.26, 1.97, 0.33, 0.55,0.77, 1.46, 1.18)
x
ri=sort(x)
ri
Fn1=(i-1)/8
Fn1

F0=punif(ri,0,2)
F0

Fn=i/8
Fn

Fn1minF0=abs(Fn1-F0)
Fn1minF0

F0minFn=abs(F0-Fn)
F0minFn
D=max(Fn1minF0,F0minFn)
D

#critical
