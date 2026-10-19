setwd("F:\\Documents\\Book\\1\\3 rd year\\2nd semester\\DOE\\R Code\\Practical\\New folder\\Lab\\Myself")
library(readr)
df<-read.csv("p-1.csv")
df
#View(df)
tail(df)
df$treatment<-as.factor(df$treatment)
str(df)

r=4
k=5


Treatment_total=aggregate(yield~treatment,df,sum)
Treatment_total

Block_total=aggregate(yield~block,df,sum)
Block_total


Grand_total=sum(df$yield)
Grand_total
grand_mean=Grand_total/(r*k)
grand_mean

Total_ss=sum(df$yield^2)-Grand_total^2/(r*k)
Total_ss

Block_ss=sum(Block_total$yield^2)/k-Grand_total^2/(r*k)
Block_ss

Treatment_ss=sum(Treatment_total$yield^2)/r-Grand_total^2/(r*k)
Treatment_ss

Error_ss=Total_ss- Block_ss- Treatment_ss
Error_ss

Block_ms=Block_ss/(r-1)
Block_ms

Treatment_ms=Treatment_ss/(k-1)
Treatment_ms

Error_ms=Error_ss/((r-1)*(k-1))
Error_ms

F_cal= round((Treatment_ms/Error_ms),2)
F_cal

F_tab= round(qf(.05, k-1,(r-1)*(k-1),lower.tail = F),2)
F_tab

##______________ANOVA TABLE_____________________

ANOVA_table=data.frame(
  SV=c("Block","Treatment","Error"),
  DF=c(r-1,k-1,(r-1)*(k-1)),
  SS=c(Block_ss,Treatment_ss,Error_ss),
  MS=c(Block_ms,Treatment_ms,Error_ms),
  F_cal=c('-', F_cal, '-'),
  F_tab=c('-', F_tab, '_')
)
ANOVA_table


###_____________________Efficiency of RBD relative to CRD____________________



RE=(Block_ss+((k-1)+(r-1)*(k-1))*Error_ms)/((r*k-1)*Error_ms)
RE


Decision=if(RE>1){
  print("Block factor has been effective slightly in reducing error variation")
} else{
  print("Block factor has not been effective slightly in reducing error variation")
}


#____________________Test the hypothesis___________________
##_______________________H0:mu(A)=mu(C)____________________
##_______________________H1:mu(A)!=mu(C)____________________

Treatment_mean=aggregate(yield~treatment,df,mean)
Treatment_mean
y_barA=Treatment_mean$yield[1]
y_barC=Treatment_mean$yield[3]
Diff=y_barA-y_barC
Diff

s=sqrt(Error_ms)
s
T_test=Diff/(s*sqrt(2/r))
T_test

T_ta=round(qt(.05/2, ((r-1)*(k-1)), lower.tail = F),2)
T_ta


##########95% CI____________________________________

y_barB=Treatment_mean$yield[2]
y_barD=Treatment_mean$yield[4]
difer=y_barB- y_barD
difer
LCL_of_AD=difer-T_ta*s*sqrt(2/r)
LCL_of_AD

UCL_of_AD=difer+T_ta*s*sqrt(2/r)
UCL_of_AD

print("LCL_of_AD<=mu(A)-mu(D)<=UCL_of_AD")
