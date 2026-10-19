#____________________________________________(i)____________________________________


setwd("C:\\Users\\Omor Faruk\\Desktop\\3203-practice")      #working directory
library(readr)           #call library
df<-read.csv("example3CRD.csv")    #open data file
df
head(df)    #see first 6 rows
names(df)    #checking variable names
df$Treatment=as.factor(df$Treatment)      #making as factor
str(df)    #checking structure

#######__________________________ANOVA_OF_CRD_________________________########
a=0.05
r=6    # No. of block
k=5    # No. of treatment
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


#__________________________________________________________(ii)_______________________________________


#____________________LSD_or_CD_treatment comparison_______________

se=sqrt(E_MS)
T_tab=qt(0.05/2,(r*k-k),lower.tail = FALSE)
T_tab


LSD=T_tab*se*sqrt(2/r)
LSD

t_m = aggregate(Yield~Treatment, df, mean) # treatment Means
t_m

multiple_comparison=data.frame(
  
  treatments=c("y_bar_A-B","y_bar_A-C","y_bar_A-D","y_bar_A-E" ,"y_bar_B-C","y_bar_B-D","y_bar_B-E","y_bar_C-D","y_bar_C-E","y_bar_D-E"),
  
  diff= c(
    round(abs(t_m$Yield[1]-t_m$Yield[2]),2),
    round(abs(t_m$Yield[1]-t_m$Yield[3]),2),
    round(abs(t_m$Yield[1]-t_m$Yield[4]),2),
    round(abs(t_m$Yield[1]-t_m$Yield[5]),2),
    round(abs(t_m$Yield[2]-t_m$Yield[3]),2),
    round(abs(t_m$Yield[2]-t_m$Yield[4]),2),
    round(abs(t_m$Yield[2]-t_m$Yield[5]),2),
    round(abs(t_m$Yield[3]-t_m$Yield[4]),2),
    round(abs(t_m$Yield[3]-t_m$Yield[5]),2),
    round(abs(t_m$Yield[4]-t_m$Yield[5]),2))
)

multiple_comparison

significant_average = subset(multiple_comparison,multiple_comparison$diff > LSD)
significant_average
