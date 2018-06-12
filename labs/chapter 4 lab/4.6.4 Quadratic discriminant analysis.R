## Chapter 4 lab - Quadratic discriminant analysis

library(MASS)
library(ISLR)
options(width=90)
options(digits=3)

## create a vector corresponding to the observations from 2001 through 2004 
attach(Smarket)
train = (Smarket$Year < 2005)
summary(train)
Smarket.2005 = Smarket[!train,] # not train 
dim(Smarket.2005)
Direction.2005=Direction[!train]

## Fit the QDA model to the Smarket data using observations before 2005
qda.fit = qda(Direction ~ Lag1 + Lag2, data=Smarket, subset=train)
qda.fit
## Output contains group means, but not the coefficients of the linear discriminants because the QDA classifier involves a quadratic, rather than a linear, function of the predictors. 

qda.class = predict(qda.fit, Smarket.2005)$class
table(qda.class, Direction.2005)
mean(qda.class == Direction.2005) # 0.599
## Interestingly, the QDA predictions are accurate almost 60% of the time, even though the 2005 data was not used to fit the model (the Smarket data has data for 2001 - 2005 and 'train' contains 2001 - 2004). This level of accuracy is quite impressive for stock market data, which is known to be quite hard to model accurately. This suggests that the quadratic form assumed by QDA may capture the true relationship more accurately than the linear forms assumed by LDA and logistic regression. 

