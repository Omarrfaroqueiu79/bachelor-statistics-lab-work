#____________________________________________(i)____________________________________


setwd("C:\\Users\\Omor Faruk\\Desktop\\3203-practice")      #working directory
library(readr)           #call library
df<-read.csv("example4RBD.csv")    #open data file
df
head(df)    #see first 6 rows
names(df)    #checking variable names
df$treatment=as.factor(df$treatment)      #making as factor
str(df)    #checking structure

#######__________________________ANOVA_OF_RBD_________________________########
a=0.05
r=4    # No. of block
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
#__________________________________efficiency_of_RBD_relative_to_CRD__________________________________
treatment_df=k-1
treatment_df
error_df=((k-1)*(r-1))
error_df
total_df=r*k-1
total_df

RE=(B_SS+(treatment_df+error_df)*E_MS)/((total_df)*E_MS)   #relative efficiency
RE
comment = if(RE>1){
  print("So, block factor has been effective slightly in reducing error variation")
}
##_______________________________________________________iii(a)______________________________________
#_________________________________________________null:Ho:mu(A)=mu(c)_______________________________________
#_________________________________________________alt:Ho:mu(A)!=mu(c)_______________________________________

treatment_mean=aggregate(yield~treatment,df,mean)     #treatment mean
treatment_mean
Y_bar_A = treatment_mean$Y[1]     #mean of A
Y_bar_C = treatment_mean$Y[3]       #mean of C

se= sqrt(E_MS)                   #standard error

t_cal = (Y_bar_A-Y_bar_C)/(se*sqrt(2/r))    #calculated t value
t_cal

t_tab = qt(0.05/2,((r-1)*(k-1)),lower.tail = F)    #tabulated t value
t_tab


#there is no statistical evidence to reject null hypothesis

##_______________________________________________________iii(b)______________________________________
#_________________________________________________null:Ho:mu(B)=mu(D)_______________________________________
#_________________________________________________alt:Ho:mu(B)!=mu(D)_______________________________________

Y_bar_B = treatment_mean$Y[2]
Y_bar_E = treatment_mean$Y[5]

t1_cal = (Y_bar_B-Y_bar_E)/(se*sqrt(2/r))
t1_cal

t1_tab = qt(0.05/2,((r-1)*(k-1)),lower.tail = F)
t1_tab

#there is no statistical evidence to reject null hypothesis




#_______________________________________________(iv)_______________________________________
#_________________________________________95%_CI_for_mu(B)-mu(D)________________________________
Y_bar_B = treatment_mean$Y[2]
Y_bar_D = treatment_mean$Y[4]

Y_BD = (Y_bar_B-Y_bar_D)
Y_BD

lower_limit =round((Y_BD-t_tab*(se*sqrt(2/r))),2)
lower_limit
upper_limit =round((Y_BD+t_tab*(se*sqrt(2/r))),2)
upper_limit

#mu(A)-mu(D)= Y_bar_B-Y_bar_D or Y_BD

print("lower_limit<=mu(A)-mu(D)<=upper_limit")
