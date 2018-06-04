## Chapter 3 lab
## Brienna Herold
## Summer 2018


options(width=50)
options(digits=4)
library(MASS)

## Install ISLR package
install.packages('ISLR', repos='http://cran.us.r-project.org')
library(ISLR)

## Explore data set Boston from MASS library
## It records medv (median house value) for 506 neighborhoods around Boston
## We seek to predict medv using 13 predictors such as rm (avg. num of rooms per house), age (average age of houses), and lstat (percent of households with low socioeconomic status)

?Boston
?fix
fix(Boston) # View in a spreadsheet like window
names(Boston) # See variables


######### SIMPLE LINEAR REGRESSION #########

## Fit a simple linear regression model with medv as response & lstat as predictor
## Basic syntax of lm() is lm(y~x, data) 
lm.fit = lm(medv~lstat, data=Boston)
## Another way
attach(Boston)
lm.fit = lm(medv~lstat)

lm.fit
summary(lm.fit)
names(lm.fit) # See what pieces of info are stored in lm.fit
coef(lm.fit) # See coefficients
confint(lm.fit) # Get confidence interval for coefficient estimates

## Produce confidence intervals & prediction intervals for the prediction of medv for a given value of lstat
predict(lm.fit, data.frame(lstat=(c(5,10,15))), interval="confidence")
predict(lm.fit, data.frame(lstat=(c(5,10,15))), interval="prediction")
## For instance, the 95% confidence interval associated with a lstat value of 10 is (24.47, 25.63), and the 95% prediction interval is (12.828, 37.28). As expected, the confidence and prediction intervals are centered around the same point (a predicted value of 25.05 for medv when lstat equals 10), but the latter are substantially wider as it has to take into account the variability not only of the mean but of all of the data.

## Plot medev and lstat along with least squares regression line
plot(lstat, medv) 
abline(lm.fit, lwd=3, col="red") # Can be used to draw any line abline(intercept, slope), lwd changes width of regression line 

## Note: There are 20 diff characters we can use to plot
plot(1:20, 1:20, pch=1:20) # First one, an empty circle, is default

## Plot lm.fit, splitting display screen into 2 x 2 grid of panels
par(mfrow=c(2,2))
plot(lm.fit)

## Alteratively, we can compute the residuals from a linear regression fit using residuals()
plot(predict(lm.fit), residuals(lm.fit))

## rstudent() returns the studentized residuals, and we can use this to plot the residuals against the fitted values
plot(predict(lm.fit), rstudent(lm.fit))

## Compute leverage statistics 
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit)) # Identifies index of largest element of a vector


######### MULTIPLE LINEAR REGRESSION #########

## Fit a multiple linear regression model using least squares
## Syntax lm(y~x1+x2+x3) is used to fit a model w 3 predictors
lm.fit = lm(medv~lstat+age, data=Boston)
summary(lm.fit)

## To avoid typing all 13 variables, can use shorthand y~. to include all
lm.fit = lm(medv~., data=Boston)
summary(lm.fit)

## Access individual components of a summary object by name
?summary.lm
summary(lm.fit)$r.sq # R^2
summary(lm.fit)$sigma # RSE

## Compute variance inflation factors
install.packages('car', repos='http://cran.us.r-project.org')
library(car)
vif(lm.fit)

## Perform regression using all variables but one
## For example, in the above regression output, age has a high p-value. So we may wish to run a regression excluding this predictor. 
lm.fit1 = lm(medv~.-age, data=Boston)
summary(lm.fit1)

## Alternatively, the update function can be used
lm.fit1 = update(lm.fit, ~.-age)
summary(lm.fit1)


######### INTERACTION TERMS #########

## lstat:age tells R to include an interaction term between lstat and age
## lstat*age includes lstat, ae, and the interaction term lstat x age
## it is shorthand for lstat+age+lstat:age
summary(lm(medv~lstat*age, data=Boston))


######### NON-LINEAR TRANSFORMATIONS OF THE PREDICTORS #########

## Perform a quadratic fit 
## Note: We need to wrap lstat^2 in I() cuz the ^ has a special meaning in a formula, while we want it to mean raise to power of 2 here
lm.fit2 = lm(medv~lstat + I(lstat^2))
summary(lm.fit2)
## The near-zero p-value associated with the quadratic term suggests that it leads to an improved model.

## Further quantify the extend to which the quadratic fit is superior to the linear fit
lm.fit = lm(medv~lstat) # Linear fit
anova(lm.fit, lm.fit2)
## anova() performs a hypothesis test comparing the two models. The null hypothesis is that the two models fit the data equally well, and the alternative hypothesis is that the full model is superior. Here the F-statistic is 135 and the associated p-value is virtually zero. This provides very clear evidence that the quadratic fit is far superior to the linear fit. 

## If we plot the fits, we can see there is little discernible pattern in the residuals for the quadratic fit
par(mfrow=c(2,2))
plot(lm.fit2)
plot(lm.fit)

## Perform a fifth-order polynomial fit
## We can use I(X^5) but this approach can get cumbersome for higher-order polynomials. Use poly()
lm.fit5 = lm(medv~poly(lstat, 5))
summary(lm.fit5)

## Try a log transformation
summary(lm(medv~log(rm), data=Boston))


######### QUALITATIVE PREDICTORS #########

## Use Carseats data, part of ISLR library, to predict Sales (child car seat sales) in 400 locations based on a number or predictors
fix(Carseats) 
names(Carseats)

## Given a qualitative variable such as Shelveloc, R generates dummy variables automatically. contrasts() returns the coding R uses for the dummy variables.
lm.fit = lm(Sales~.+Income:Advertising+Price:Age, data=Carseats)
summary(lm.fit)
attach(Carseats)
?contrasts
contrasts(ShelveLoc)
## R has created a ShelveLocGood dummy variable that takes on a value of 1 if the shelving location is good, and 0 otherwise. It has also created a ShelveLocMedium dummy variable that equals 1 if the shelving location is medium, and 0 otherwise. A bad shelving location corresponds to a zero for each of the two dummy variables. 
## The fact that the coefficient for ShelveLocGood in the regression output is positive indicates that a good shelving location is associated with high sales (relative to a bad location). And ShelveLocMedium has a smaller positive coefficient, indicating that a medium shelving location leads to higher sales than a bad shelving location but lower sales than a good shelving location.


######### WRITING FUNCTIONS #########

## Check if a function exists
LoadLibraries
LoadLibraries()

## Create the function
LoadLibraries = function () {
	library(ISLR)
	library(MASS)
	print("The libraries have been loaded.")
}

## Check what the function contains, and call it
LoadLibraries
LoadLibraries()


