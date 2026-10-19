#_____________________________________(i)____________________________________

setwd("C:\\Users\\Omor Faruk\\Desktop\\3203-practice")      #working directory
library(readr)           #call library
df<-read.csv("example2CRD.csv")    #open data file
df
head(df)    #see first 6 rows
names(df)    #checking variable names
df$Treatment=as.factor(df$Treatment)      #making as factor
str(df)    #checking structure

#######__________________________ANOVA_OF_CRD_________________________########
a=0.05
r=5   
k=6
T_t<-aggregate(Yield~Treatment,df,sum)    #treatment total
T_t  
G_t=sum(df$Yield)   #grand total
G_t
G_m=G_t/(r*k)   #grand mean
G_m

Total_SS=sum(df$Yield^2)-G_t^2/(r*k)   #total sum of square
Total_SS

T_SS=sum(T_t$Yield^2)/r-(G_t^2/(r*k))  #treatment sum of square
T_SS

E_SS=Total_SS-T_SS             #error sum of square
E_SS

T_MS=T_SS/(k-1)       #treatment mean sum of square
T_MS
E_MS=E_SS/((r*k)-k)        #error mean sum of square
E_MS

F_cal=round((T_MS/E_MS),2)           #calculated f value
F_cal

F_tab=round(qf(.05,df1=(k-1),df2=((r*k)-k),lower.tail = F),2)       #tabulated t value
F_tab


###########_______________________ANOVA_TABLE_FOR_CRD________________________________________

ANOVA_TABLE=data.frame(SV=c("Treatment","Error"),
                       DF=c(k-1,(r*k-k)),
                       SS=c(T_SS,E_SS),
                       MS=c(T_MS,E_MS),
                       F_cal=c(F_cal, '--'),
                       F_tab=c(F_tab, '--')
  
)
ANOVA_TABLE



#___________________________________________(ii)_____________________________________________


###_____________________________Comparison_treatment_means_of_A_&_E_____________________
#Ho:mu(A)=mu(E)
T_m=aggregate(Yield~Treatment,df,mean)    #treatment means
T_m
y_bar_A=T_m$Yield[1]
y_bar_A
y_bar_E=T_m$Yield[5]
y_bar_E

se=sqrt(E_MS)
se
t_cal=(y_bar_A-y_bar_E)/(se*sqrt(2/r)) #calculated t value
t_cal

t_tab=qt(p=0.05/2, df=(r*k-k), lower.tail = F)     #tabulated t value
t_tab

#_______________________________________95%CI_____________________________________

lower_limit=round(((y_bar_A-y_bar_E)-t_tab*se*sqrt(2/r)),3)
lower_limit
upper_limit=round(((y_bar_A - y_bar_E)+t_tab*se*sqrt(2/r)),3)
upper_limit
print("lower_limit<=mu(A)-mu(E)<=upper_limit")
