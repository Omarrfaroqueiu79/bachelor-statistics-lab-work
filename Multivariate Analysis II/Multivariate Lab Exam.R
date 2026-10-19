setwd("C:/Users/Omor Faruk/Desktop/4-2")
# Problem_1
# Read the data 
df=read.csv("Amitriptyline_Data.csv",header = T, sep = ",")
View(df)
# y1 is the first dependent variabe
y1=df$Y1
# Extract second column from the data frame y2 as second Dependent variable
y2=df$Y2
# Extract independent variable
x=df[c(3:7)]
print(x)

# (a) Perform a regression analysis using only the first response y1
# (i)
#fitting the linear regression models
model1=lm(y1~df$Z1+df$Z2+df$Z3+df$Z4+df$Z5)

print(model1)
summary(model1)
# Prediction interval
new_data=c(1,1200,140,70,85)
pred=predict(model1,data=new_data)
pred


#(b)
# Using the second response
#fitting the linear regression models
model2=lm(y2~df$Z1+df$Z2+df$Z3+df$Z4+df$Z5)

print(model2)
summary(model1)
# Prediction interval
new_data=c(1,1200,140,70,85)
pred=predict(model2,data=new_data)
pred


#c
#Multivariate multiple regression 
mul_reg=lm(cbind(Y1,Y2)~Z1+Z2+Z3+Z4+Z5,data = df)
 
# Univariate analysis
summary(mul_reg)
coef(mul_reg)






