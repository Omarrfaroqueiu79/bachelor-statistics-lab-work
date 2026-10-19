##########Demonstration of what normally distributed residuals look like
#########https://rpubs.com/stevenlsenior/normal_residuals_with_code
#Generating some data from an regression equation
Yi=0.5+Xi+ei
## Set seed for reproducibility
set.seed(250419)

# Generate random x values
x <- rnorm(n = 500, mean = 5,  sd = 2)

# Generate y values y = 0.5x + e
y <- 0.5*x + rnorm(500,
                   mean = 0,
                   sd = 1)

d <- data.frame(x, y)
d


#### Make a scatter plot
g1 <- qplot(x, y, 
            data = d,
            geom = "point") + 
      labs(x = "X",
           y = "Y",
           title = "Scatterplot of simulated data")

g1

##############Fitting a regression line
### Function for doing residual plots
residplot <- function(d){
## Fit a linear model
m <- lm(y ~ x, data = d)
m   
# Add residuals to data
d$residual <- m$residuals
d$residual

# Make a scatter plot

library(ggplot2)
library(cowplot)
g1 <- qplot(x, y, 
            data = d,
            geom = "point") + 
      geom_abline(intercept = m$coefficients[1],
                  slope = m$coefficients[2],
                  colour = "red") +
      labs(x = "X",
           y = "Y",
           title = "Scatterplot of simulated data")
g1
   
# Make a scatter plot of residuals against fitted values
g2 <- qplot(m$fitted, m$residuals,
            geom = "point") +
         geom_abline(intercept = 0,
                     slope = 0,
                     colour = "red") +
         labs(title = "Plot of residuals vs fitted values",
              x = "fitted value",
              y = "residual")
g2

# Make a histogram of the residuals
g3 <- qplot(m$residuals,
               geom = "histogram",
               bins = 10) +
         labs(title = "Histogram of residuals",
              x = "residual")
g3

# Make a quantile-quantile plot
   g4 <-ggplot(data = d,
               aes(sample = m$residuals)) +
        geom_qq() +
        geom_qq_line(colour = "red") +
        labs(title = "Quantile plot of residuals")
g4
# Plot the plots
g <- plot_grid(g1, g2, g3, g4, ncol = 2, labels = "auto")

plot_grid(g1, g2, ncol = 2, labels = "auto")
   
   # Return the plots
return(g)
}

residplot(d)



#### Set seed for reproducibility
set.seed(250419)

# Generate random x values
x <- rnorm(n = 500, 
           mean = 5, 
           sd = 2)

# Adding outliers
x2 <- rnorm(n = 50,
            mean = 15,
            sd = 1.5)

x <- c(x, x2)

# calculate y values
y <- 0.5*x + rnorm(n = 550,
                     mean = 0,
                     sd = 1)

d <- data.frame(x, y)

# Re-do the plots
residplot(d)



# Make histograms of x and y
g5 <- qplot(x = x,
            data = d,
            geom = "histogram",
            bins = 20) +
      ggtitle("Hisotgram of X")

g6 <- qplot(x = y,
            data = d,
            geom = "histogram",
            bins = 20) + 
      ggtitle(("Histogram of Y"))

plot_grid(g5, g6, ncol = 2, labels = "auto")




### Violating the normality assumption
# Set seed
set.seed(250419)

# Simulate the data: y ~ x^2
x <- rnorm(n = 500,
           mean = 6,
           sd = 1)

y <- 0.5*(x^2) + rnorm(n = 500,
                       mean = 0,
                       sd = 1)

d <- data.frame(x, y)

# Re-do the plots
residplot(d)

#### Non-constant variance
# Set the seed
set.seed(250419)

# Make the data
x <- rnorm(n = 500,
           mean = 6,
           sd = 1)

y <- log(x + rnorm(n = 500, mean = 0, sd = 1))

d <- data.frame(x, y)

# Re-do the plots
residplot(d)

















########## Create example data & run regression with smoothing function
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


gplot=function(data){
ggp <- ggplot(data, aes(x, y)) +		
geom_point()
ggp
ggp +									
stat_smooth(method = "lm",
			formula = y ~ x,
			geom = "smooth")


}
gplot(data)

################# Testing function
x=rnorm(500,3,5)
x
y=rnorm(500,4,5)
y
data=data.frame(x,y)
data
gplot(data)




# Install & load ggplot2
library("ggplot2")
set.seed(87)
# Create example data
x <- rnorm(250)
y <- rnorm(250) + 2 *x
data <- data.frame(x, y)
data


### Run the regression line creating function
regresid=function(data){
# Fit the regression line
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
	
}
regresid(data)


## Testing function for another regression
set.seed(87)							
x <- rnorm(2500)
y <- rnorm(2500) + 2 *x^2
data <- data.frame(x, y)

regresid(data)




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



















































































































































































































































































































































































































































































































































































































































