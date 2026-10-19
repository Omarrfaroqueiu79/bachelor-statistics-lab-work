# Create example data & run regression with smoothing function
rm(list = ls())
set.seed(87)							
x <- rnorm(250)
y <- rnorm(250) + 2 *x
data <- data.frame(x, y)

# Print first rows of data
head(data)								


# Install & load ggplot2				
library("ggplot2")

# Create basic ggplot
# and Add regression line
ggp <- ggplot(data, aes(x, y)) +		
geom_point()
ggp
ggp +									
stat_smooth(method = "lm",
			formula = y ~ x,
			geom = "smooth")



# Install & load ggplot2
library("ggplot2")
set.seed(87)
# Create example data
x <- rnorm(250)
y <- rnorm(250) + 2 *x
data <- data.frame(x, y)
data

### Run the regression line
reg<-lm(formula = y ~ x, data=data)					
reg

#get intercept and slope value
coeff<-coefficients(reg)	
coeff	
intercept<-coeff[1]
intercept
slope<- coeff[2]
slope

### Create basic ggplot
ggp <- ggplot(data, aes(x, y)) +
geom_point()

### add the regression line
ggp+geom_abline(intercept = intercept, slope = slope, color="red",
			linetype="dashed", size=1.5)+
ggtitle("Lab Exam 2nd Year")		




#### ExampleIII: Setting "loyess" curve on the graph
library("ggplot2")
set.seed(87)
# Create example data
x <- rnorm(250)
y <- rnorm(250) + 2 *x^2
data <- data.frame(x, y)
data

### Run the regression line
reg<-lm(formula = y ~ x, data=data)					
reg

#get intercept and slope value
coeff<-coefficients(reg)	
coeff	
intercept<-coeff[1]
intercept
slope<- coeff[2]
slope

### Create basic ggplot
ggp <- ggplot(data, aes(x, y)) +
geom_point()

### add the regression line
ggp+geom_abline(intercept = intercept, slope = slope, color="red",
			linetype="dashed", size=1.5)+
ggtitle("Lab Exam 2nd Year")

# plotting the data
ggp<-ggplot(data, aes(x,y) ) +
  geom_point() 
  
# adding the regression line to it
ggp+geom_smooth(method = "loess",
              formula = y ~ x)



library(ggplot2)
data("mtcars"); head(mtcars)
mtcars


d <- mtcars
d
#### fit the model
fit <- lm(mpg ~ wt, data = d)
fit
#Save the predicted values
d$predicted <- predict(fit)  
d$predicted 
# Save the residual values 
d$residuals <- residuals(fit) 
d$residuals
### regression line  
ggplot(d, aes(x = wt, y = mpg)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +  
## draw line from point to line
geom_segment(aes(xend = wt, yend = predicted), alpha = .2) +  
# size of the points   
  geom_point(aes(color = abs(residuals), size = abs(residuals))) + 
# colour of the points mapped to residual size - green smaller, red larger
 scale_color_continuous(low = "green", high = "red") +              
 guides(color = FALSE, size = FALSE) +                         
 # Size legend removed
  geom_point(aes(y = predicted), shape = 1) +
  theme_bw()

### Summary
summary(fit) 
########## Residuals analysis
 #### Residuals vs Fitted Plot
plot(fit, which=1, col=c("blue")) 

###Normal Q–Q (quantile-quantile) Plot
plot(fit, which=2, col=c("red"))  # Q-Q Plot
### Scale-Location: Homoscedasticity
plot(fit, which=3, col=c("blue"))  # Scale-Location Plot
##########Residuals vs Leverage
plot(fit, which=5, col=c("blue"))  # Residuals vs Leverage
# Histogram of the residuals
#create histogram of residuals
ggplot(data = d, aes(x = fit$residuals)) +
    geom_histogram(fill = 'steelblue', color = 'black') +
    labs(title = 'Histogram of Residuals', x = 'Residuals', y = 'Frequency')





