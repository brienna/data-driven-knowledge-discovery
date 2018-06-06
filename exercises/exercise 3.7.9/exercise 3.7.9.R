## Exercise 3.7.9
## Brienna Herold
## Summer 2018

# 9. This question involves the use of multiple linear regression on the Auto data set. 

## Load data (change working directory first)
Auto = read.csv("Auto.csv", header=T, na.strings="?")

## View summary stats & dimensions
fix(Auto)
summary(Auto)
dim(Auto)

## Remove rows containing missing observations
Auto = na.omit(Auto) 
dim(Auto) ## 5 rows were eliminated

# (a)  Produce a scatterplot matrix which includes all of the variables in the data set. 

## Two ways to produce scatterplot matrix
plot(Auto)
pairs(Auto) 

# (b)  Compute the matrix of correlations between the variables using the function cor(). You will need to exclude the name variable, which is qualitative. 

## Confirm name is qualitative (just for my interest)
sapply(Auto, is.factor)

## Compute correlations, excluding name
?cor
cor(Auto[, names(Auto) != "name"])
## Another way, generic
cor(Auto[, !sapply(Auto, is.factor)])

# (c)  Use the lm() function to perform a multiple linear regression with mpg as the response and all other variables except name as the predictors. Use the summary() function to print the results. Comment on the output. For instance: 

?lm
lm.fit = lm(mpg~.-name, data=Auto)
summary(lm.fit)
 # i. Is there a relationship between the predictors and the response? 

## According to R^2, approx. 82% of the variance in the response can be explained by at least one of the predictors. Also, the F-statistic is 252.4 on 7 variables with 384 degrees of freedom, and since it is statistically significant (p-value is very small), it provides compelling evidence against the null hypothesis of there being no relationship. 
 # ii. Which predictors appear to have a statistically significant relationship to the response? 

## displacement, weight, year, origin
 # iii. What does the coefficient for the year variable suggest?

## The coefficient for the year variable is 0.750773, and being a coefficient it suggests that every time a year goes by, we will see a 0.75 unit increase in the response, mpg. 

# (d)  Use the plot() function to produce diagnostic plots of the linear regression fit. Comment on any problems you see with the fit. Do the residual plots suggest any unusually large outliers? Does the leverage plot identify any observations with unusually high leverage? 

par(mfrow=c(2,2))
plot(lm.fit)
?plot.lm
## Residuals vs Fitted suggests that the fit is not linear. Normal Q-Q suggests the residuals deviate from the normal distribution in the upper right part. The residual plots together suggest some unusually large outliers which seem to have been conveniently labeled. The leverage plot identifies observation 14 as having unusually high leverage. 

## Compute leverage stats (measures how far away observation values are from each other) 
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))
## Confirms observation 14 has an unusually high leverage

# (e)  Use the * and : symbols to fit linear regression models with interaction effects. Do any interactions appear to be statistically significant? 

## Note: Didn't use * cuz the . symbol in the lm(mpg~.) function includes all the variables. 

summary(lm(mpg~.-name+ displacement:weight, data=Auto)) 
## Yes: displacement:weight
summary(lm(mpg~.-name+ displacement:weight+year:origin, data=Auto))
## Yes: displacement:weight, year:origin
summary(lm(mpg~.-name+ displacement:weight+year:origin+displacement:origin, data=Auto)) 
## Yes: displacement:weight, year:origin
## No: displacement:origin
summary(lm(mpg~.-name+ year:origin, data=Auto)) 
## Yes: year:origin
summary(lm(mpg~.-name+ displacement:origin, data=Auto)) 
## Yes: displacement:origin
summary(lm(mpg~.-name+ year:origin+year:weight, data=Auto)) 
## Yes: year:weight
## No: year:origin
summary(lm(mpg~.-name+ displacement:weight+displacement:year+displacement:origin+weight:year+weight:origin+year:origin, data=Auto)) 
## Yes: displacement:weight
## No: displacement:year, displacement:origin, weight:year, weight:origin, year:origin

## Could probably test many other combinations of interactions terms...

# (f)  Try a few different transformations of the variables, such as log(X), √X, X2. Comment on your findings.  
## Noticed from scatterplot matrix that displacement and horsepower might be good for transformation, since their plots are not that linear.

## Perform a quadratic fit
lm.fit2 = lm(mpg~displacement + I(displacement^2))
summary(lm.fit2)
## Compare with linear fit
anova(lm(mpg~displacement), lm.fit2)
## F-statistic is high, p-value is significant, quadratic seems to be a better fit than linear

## Perform 6th-order polynomial fit
lm.fit6 = lm(mpg~poly(displacement, 5))
summary(lm.fit6)
anova(lm.fit2, lm.fit6)
## F-statistic is low, and p-value is not significant, this is worse than quadratic fit
anova(lm(mpg~displacement), lm.fit6)
## F-statistic is highish, and p-value is significant, this is better than linear fit

## Perform log transformation
lm.fitlog = lm(mpg~log(displacement))
anova(lm(mpg~displacement), lm.fitlog)
## No F-statistic, models seem incomparable?

## Perform a quadratic fit
lm.fit2 = lm(mpg~horsepower + I(horsepower^2))
summary(lm.fit2)
## Compare with linear fit
anova(lm(mpg~horsepower), lm.fit2)
## F-statistic is high, p-value is significant, quadratic seems to be a better fit than linear

## Perform 6th-order polynomial fit
lm.fit6 = lm(mpg~poly(horsepower, 5))
summary(lm.fit6)
anova(lm.fit2, lm.fit6)
## F-statistic is highish, and p-value is significant, this is better than quadratic fit



