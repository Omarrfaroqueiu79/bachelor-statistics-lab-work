setwd("F:\\Documents\\Book\\1\\3 rd year\\2nd semester\\DOE\\R Code\\Practical\\New folder\\Lab\\Myself")
library(readxl)
df<-read_excel("p-3.xlsx")
df
head(df)
df

df$Treatmentl=as.factor(df$Treatment)
str(df)

#_____________________ANOVA of CRD_______________________________
r=6
k=5



Treatment_total=aggregate(Yield~Treatment,df,sum)
Treatment_total

Grand_total=sum(df$Yield)
Grand_total

Grand_mean=Grand_total/(r*k)
Grand_mean

G=Grand_total^2/(r*k)
G

Total_ss=sum(df$Yield^2)-G
Total_ss

Treatment_ss=sum(Treatment_total$Yield^2)/r-G
Treatment_ss

Error_ss=Total_ss-Treatment_ss
Error_ss

Treatment_ms=Treatment_ss/(k-1)
Treatment_ms

Error_ms=round((Error_ss/(r*k-k)),0)
Error_ms

F_cal=round((Treatment_ms/Error_ms),2)
F_cal

F_tab=round(qf(.05,k-1,(r*k-k),lower.tail = F),2)
F_tab

##__________________ANOVA TABLE________________________

Anova_table=data.frame(
  SV=c("Treatment", "Error"),
  DF=c(k-1,(r*k-k)),
  SS=c(Treatment_ss,Error_ss),
  MS=c(Treatment_ms,Error_ms),
  F_cal=c(F_cal, ' '),
  F_tab=c(F_tab, ' ')
)
Anova_table


##_______________LSD__________________________
t_tab=qt(.05/2,(r*k-k),lower.tail = F)
t_tab
s=sqrt(Error_ms)
s

LSD=t_tab*(s*(sqrt(2/r)))
LSD

treatment_mean=aggregate(Yield~Treatment,df,mean)
treatment_mean

multiple_comparison=data.frame(
  treatment=c("y_bar_AB","y_bar_AC","y_bar_AD","y_bar_AE","y_bar_BC","y_bar_BD","y_bar_BE","y_barCD","y_bar_CE","y_bar_DE"),
  Differ=c(
    round(abs(treatment_mean$Yield[1]-treatment_mean$Yield[2]),2),
    round(abs(treatment_mean$Yield[1]-treatment_mean$Yield[3]),2),
    round(abs(treatment_mean$Yield[1]-treatment_mean$Yield[4]),2),
    round(abs(treatment_mean$Yield[1]-treatment_mean$Yield[5]),2),
    round(abs(treatment_mean$Yield[2]-treatment_mean$Yield[3]),2),
    round(abs(treatment_mean$Yield[2]-treatment_mean$Yield[4]),2),
    round(abs(treatment_mean$Yield[2]-treatment_mean$Yield[5]),2),
    round(abs(treatment_mean$Yield[3]-treatment_mean$Yield[4]),2),
    round(abs(treatment_mean$Yield[3]-treatment_mean$Yield[5]),2),
    round(abs(treatment_mean$Yield[4]-treatment_mean$Yield[5]),2)
  )
)
multiple_comparison


significant_difference=subset(multiple_comparison,multiple_comparison$Differ>LSD)
significant_difference
