# No_Of_Female =53
# No_Of_Male = No_Of_Female-53
# Expect = 60

Test = function(Observed,Expected,alpha){
  x=(Observed-Expected)^2/(Expected)+((100-Observed)-(100-Expected))^2/((100-Expected))
  x=round(x,2)
  
  y=round(qchisq(alpha,1,lower.tail = F),2)
  
  z =round(1-pchisq(x,1),2)
  
  return(data.frame(Chi_Statistic=x,Chi_table=y,P=z,alpha=alpha))
}



Test(53,50,0.05)
