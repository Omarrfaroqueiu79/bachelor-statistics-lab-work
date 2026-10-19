#____________________________________________(i)____________________________________


setwd("C:\\Users\\Omor Faruk\\Desktop\\3203-practice")      #working directory
library(readr)           #call library
df<-read.csv("example5RBD.csv")    #open data file
df
head(df)    #see first 6 rows
names(df)    #checking variable names
##df$blockt=as.factor(df$block)
df$treatment=as.factor(df$treatment)      #making as factor
str(df)    #checking structure
#######__________________________ANOVA_OF_RBD_________________________########
r=5    # No. of block
k=5    # No. of treatment
B_t=aggregate(yield~block,df,sum)   #block total
B_t
T_t<-aggregate(yield~treatment,df,sum)    #treatment total
T_t  

G_t<-sum(df$yield)      #grand total
G_t
G_m<-G_t/(r*k)            #grand mean
G_m


Total_SS=sum(df$yield^2)-G_t^2/(r*k)   #total sum of square
Total_SS

B_SS=sum(B_t$yield^2)/k-(G_t^2/(r*k))  #block sum of square
B_SS

T_SS=sum(T_t$yield^2)/r-(G_t^2/(r*k))  #treatment sum of square
T_SS

E_SS=Total_SS-B_SS-T_SS             #error sum of square
E_SS

B_MS=B_SS/(r-1)       #block mean sum of square
B_MS

T_MS=T_SS/(k-1)       #treatment mean sum of square
T_MS

E_MS=E_SS/((r-1)*(k-1))        #error mean sum of square
E_MS

F_cal=round((T_MS/E_MS),2)           #calculated f value
F_cal

F_tab=round(qf(.05,df1=(k-1),df2=((r-1)*(k-1)),lower.tail = F),2)       #tabulated f value
F_tab

###########_______________________ANOVA_TABLE_FOR_RBD________________________________________

ANOVA_TABLE=data.frame(SV=c("Block","Treatment","Error"),
                       DF=c(r-1,k-1,(r-1)*(k-1)),
                       SS=c(B_SS,T_SS,E_SS),
                       MS=c(B_MS,T_MS,E_MS),
                       F_cal=c('--', F_cal, '--'),
                       F_tab=c('--', F_tab, '--')
                       
)
ANOVA_TABLE





##_______________________________________________________(ii)______________________________________
#_________________________________________90%_CI_for_mu(B)-mu(D)________________________________

treatment_mean=aggregate(yield~treatment,df,mean)
treatment_mean

Y_bar_A = treatment_mean$yield[1]
Y_bar_D = treatment_mean$yield[4]

se= sqrt(E_MS)
se
t_tab=qt(0.05,16,lower.tail = F)
t_tab
Y_AD = (Y_bar_A-Y_bar_D)
Y_AD

LCL_AD =round((Y_AD-(t_tab*(se*sqrt(2/r)))),2)
LCL_AD
UCL_AD =round((Y_AD+(t_tab*(se*sqrt(2/r)))),2)
UCL_AD

#mu(A)-mu(D)= Y_bar_B-Y_bar_D or Y_BD

print("LCL_AD<=mu(A)-mu(D)<=UCL_AD")


##__________________________________________(iii)_______________________________________________


#_______________Duncan method for multiple comparison____________________


treatment_mean=aggregate(yield~treatment,df,mean)
treatment_mean


se=sqrt(E_MS)
s_y_bar=(se)/sqrt(r)
s_y_bar


#########################











#______________________pairwise comparison___________________________


multiple_comparison=data.frame(
  
  treatment=c("y_bar_E-A","y_bar_E-B","y_bar_E-C","y_bar_E-D" ,"y_bar_D-A","y_bar_D-B","y_bar_D-C","y_bar_C-A","y_bar_C-B","y_bar_B-A"),
  
  diff= c(
    round(abs(treatment_mean$yield[5]-treatment_mean$yield[1]),2),
    round(abs(treatment_mean$yield[5]-treatment_mean$yield[2]),2),
    round(abs(treatment_mean$yield[5]-treatment_mean$yield[3]),2),
    round(abs(treatment_mean$yield[5]-treatment_mean$yield[4]),2),
    round(abs(treatment_mean$yield[4]-treatment_mean$yield[1]),2),
    round(abs(treatment_mean$yield[4]-treatment_mean$yield[2]),2),
    round(abs(treatment_mean$yield[4]-treatment_mean$yield[3]),2),
    round(abs(treatment_mean$yield[3]-treatment_mean$yield[1]),2),
    round(abs(treatment_mean$yield[3]-treatment_mean$yield[2]),2),
    round(abs(treatment_mean$yield[2]-treatment_mean$yield[1]),2))
)

multiple_comparison



##############
