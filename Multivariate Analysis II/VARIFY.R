Z=c(1,1,1,1,1,0,1,2,3,4)
Y1=c(1,4,3,8,9)
Y2=c(-1,-1,2,3,2)

Y=matrix(c(Y1,Y2),5)
Y
Z=matrix(Z,2,5,byrow=T)
Z

VARIFY=function(Z,Y){
  BETA_HAT=function(Z,Y1,Y2){
    
    ztz=c((solve(Z%*%t(Z)))%*%(Z)%*%Y)
    m=matrix(ztz,2)
    m
    
    
  }
  B=BETA_HAT(Z,Y1,Y2)
  
  
  Y_HAT=function(Z,B){
    yh=round(t(Z)%*%B,2)
    yh
  }
  
  YH=Y_HAT(Z,B)
  YH
  
  E_HAT=function(Y,YH){
    e=round(Y-YH,2)
    e
  }
  
  E=E_HAT(Y,YH)
  
  
  A=Y_HAT(Y,YH)+Y_HAT(E,E)
  B=Y_HAT(Y,Y)
  
  print("Y'Y:")
  print(A) 
  
  print("YH'YH + EH'EH:")
  print(B)
 

  
}
VARIFY(Z,Y)

