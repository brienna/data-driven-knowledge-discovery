## Exercise 3.7.8
## Brienna Herold
## Summer 2018

# 8. This question involves the use of simple linear regression on the Auto data set. 

Auto = read.csv("Auto.csv", header=T, na.strings="?")
Auto = na.omit(Auto) # Remove rows containing missing observations
nrow(Auto)
attach(Auto) # Make variables accessible by name

# (a) Use the lm() function to perform a simple linear regression with mpg as the response and horsepower as the predictor. Use the summary() function to print the results. Comment on the output. For example: 

## lm() is used to fit linear models lm(formula). A typical formula has the form response ~ terms
?lm() 
lm(mpg ~ horsepower)
summary(lm(mpg ~ horsepower))
lmMpgHp = lm(mpg ~ horsepower)
summary(lmMpgHp)

# i.	Is there a relationship between the predictor and the response?  
## Yes, as evidenced by the large t-value/t-statistic associated with the coefficient for horsepower, or $\hat\beta_1

# ii.	How strong is the relationship between the predictor and the response?  
## The relationship is fairly strong as evidenced by the R^2 = 0.6.
## Note: We would like to see a R^2 in the 0.90s instead of the 0.60s. 

# iii.	Is the relationship between the predictor and the response positive or negative? 

## The relationship is negative as evidenced by the minus sign in front of -0.157845, the Estimate for horsepower.  
# iv.	What is the predicted mpg associated with a horsepower of 98? What are the associated 95 % confidence and prediction intervals? 

## Y = B0 + B1X
## Y = 39.94 + (-0.16 * 98) 
## Y = approx 25 mpg 
# (b) Plot the response and the predictor. Use the abline() function to display the least squares regression line. 

plot(horsepower, mpg)
?abline
abline(lmMpgHp, col="red")

# (c) Use the plot() function to produce diagnostic plots of the least squares regression fit. Comment on any problems you see with the fit. 

par(mfrow=c(2,2))
plot(lmMpgHp)

## The residuals show a pattern indicating we've left something out. The Q-Q plot tells us that the residuals are not normally distributed, when we expect them to be.  The leverage statistic can range between 1/n and 1. We expect it on average to be (p+1)/n or 0.005 and we expect Cook's distance for any point to be below 4/n, or about 0.01, so we have about twenty points with high leverage. 
