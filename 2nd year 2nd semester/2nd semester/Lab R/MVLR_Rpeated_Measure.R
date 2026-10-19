### Calling library
library(car)
#Loading required package: carData
some(iris)
 
### Scatter plot of the variables
scatterplotMatrix(~ Sepal.Length + Sepal.Width + Petal.Length
+ Petal.Width | Species,
data=iris, smooth=FALSE, regLine=FALSE, ellipse=TRUE,
by.groups=TRUE, diagonal=FALSE, legend=list(coords="bottomleft"))
par(mfrow=c(2, 2))
for (response in c("Sepal.Length", "Sepal.Width",
"Petal.Length", "Petal.Width"))
Boxplot(iris[, response] ~ Species, data=iris, ylab=response)

mod.iris <- lm(cbind(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)
~ Species, data=iris)
class(mod.iris)

mod.iris=lm(formula = cbind(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width) ~
Species, data = iris)
mod.iris
summary(mod.iris)


### MANOVA testing
(manova.iris <- Anova(mod.iris))
class(manova.iris)
summary(manova.iris)

anova(mod.iris)


##### repeated measure Design
some(OBrienKaiser)

contrasts(OBrienKaiser$treatment)

contrasts(OBrienKaiser$gender)

xtabs(~ treatment + gender, data=OBrienKaiser)


### Repeated measure design for idata
phase <- factor(rep(c("pretest", "posttest", "followup"), c(5, 5, 5)),
levels=c("pretest", "posttest", "followup"))
hour <- ordered(rep(1:5, 3))
idata <- data.frame(phase, hour)
idata


OBrien.long <- reshape(OBrienKaiser,
varying=c("pre.1", "pre.2", "pre.3", "pre.4", "pre.5",
"post.1", "post.2", "post.3", "post.4", "post.5",
"fup.1", "fup.2", "fup.3", "fup.4", "fup.5"),
v.names="score",
timevar="phase.hour", direction="long")
OBrien.long$phase <- ordered(
c("pre", "post", "fup")[1 + ((OBrien.long$phase.hour - 1) %/% 5)],
levels=c("pre", "post", "fup"))
OBrien.long$hour <- ordered(1 + ((OBrien.long$phase.hour - 1) %% 5))
dim(OBrien.long)

### Calling data
head(OBrien.long, 25) # first 25 rows

Means <- as.data.frame(ftable(with(OBrien.long,
tapply(score,
list(treatment=treatment, gender=gender, phase=phase, hour=hour),
mean))))
names(Means)[5] <- "score"
dim(Means)


head(Means, 25) # first 25 means

##### SEM Package
library(sem)
data(Klein)
Klein
instatll.packages('sem')
