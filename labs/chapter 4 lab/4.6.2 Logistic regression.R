
# Chapter 4 lab - Logistic regression 

## We will use the Smarket data, which is part of the ISLR library. This data set consists of percentage returns for the S&P 500 stock index over 1, 250 days, from the beginning of 2001 until the end of 2005. For each date, we have recorded the percentage returns for each of the five previous trading days, Lag1 through Lag5. We have also recorded Volume (the number of shares traded on the previous day, in billions), Today (the percentage return on the date in question) and Direction (whether the market was Up or Down on this date). 

library(ISLR)
options(width=90)
options(digits=4)  
names(Smarket)  # See headings
dim(Smarket)  # See dimensions
summary(Smarket)  

## Generate scatterplot matrix
pairs(Smarket)  
# This is difficult to read, though & if there are patterns they're very limited

## Generate correlation matrix for all of the pairwise correlations among the predictors in the data set
?cor
cor(Smarket)
## Throws an error, need to exclude Direction column, which is qualitative
cor(Smarket[, colnames(Smarket) != 'Direction'] )
## As one would expect, the correlations between the lag variables and to- day’s returns are close to zero. In other words, there appears to be little correlation between today’s returns and previous days’ returns. The only substantial correlation is between Year and Volume. 
## Like covariance matrix, only the correlation matrix is scaled
## Instead of the variance on the main diagonal, we see ones because the correlation of a vector with itself is always 1

# If we plot Volume, we can see it increases over time. In other words, the average number of shares traded daily increased from 2001 to 2005. 
attach(Smarket)
plot(Volume)

## Fit a logistic regression model to predict Direction using Lag 1 thru Lag5 and Volume. 
?glm 
## Formula: response ~ terms
## Takes family argument, which is a description of the error distribution, e.g. gaussian, binomial. Binomial tells it to run logistic regression rather than some other type of generalized linear model.
glm.fit = glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume, family=binomial)
summary(glm.fit)
## Doesn't look pretty, the p values are high. Best p value though is Lag1. The negative coefficient for this predictor suggests that if the market had a positive return yesterday, then it is less likely to go up today. However, at a value of 0.15, the p-value is still relatively large, and so there is no clear evidence of a real association between Lag1 and Direction. 
## Estimate column shows coefficient values, other ways to get coefficients:
coef(glm.fit)
summary(glm.fit)$coef
## Only return p values
summary(glm.fit)$coef[,4] 

## Predict the probability that the market will go up, given values of the predictors
?predict
glm.probs = predict(glm.fit, type="response")
## The type="response" tells R to output probabilities of the form P(Y = 1|X), as opposed to other information such as the logit 
## Print the first 10 probabilities
glm.probs[1:10]

## We know that these values correspond to the probability of the market going up, rather than down, because the contrasts() function indicates that R has created a dummy variable with a 1 for Up. 
?contrasts 
contrasts(Direction)

## In order to make a prediction as to whether the market will go up or down on a particular day, we must convert these predicted probabilities into class labels, Up or Down. Create a vector of class predictions based on whether the predicted probability of a market increase is greater than or less than 0.5 
glm.pred=rep("Down", 1250) # creates 1250 predictions of down elements
glm.pred[glm.probs > 0.5] = "Up" # transforms to Up all of the elements for which the predicted probability of a market increase exceeds 0.5 
## produce a confusion matrix in order to determine how many observations were correctly or incorrectly classified 
table(glm.pred, Direction)
## The diagonal elements of the confusion matrix indicate correct predictions, while the off-diagonals represent incorrect predictions. Hence our model correctly predicted that the market would go up on 507 days and that it would go down on 145 days, for a total of 507 + 145 = 652 correct predictions 

## compute the fraction of days for which the prediction was correct 
## several ways
(507+145)/1250
mean(glm.pred==Direction)
# In this case, logistic regression correctly predicted the movement of the market 52.2 % of the time. 

## create a vector corresponding to the observations from 2001 through 2004 
train = (Year < 2005) 
summary(train)
## train is a vector of 1,250 elements, corresponding to the observations in our data set. The elements of the vector that correspond to observations that occurred before 2005 are set to TRUE, whereas those that correspond to observations in 2005 are set to FALSE 
## create a held out data set of observations from 2005, using the train vector which, containing TRUE/FALSE, is a Boolean vector and can thus be used to obtain a subset of the rows or columns of a matrix. Because it includes !train, it will be the opposite of what's in train. Smarket[!train,] yields a submatrix of the stock market data containing only the observations for which train is FALSE—that is, the observations with dates in 2005 
Smarket.2005 = Smarket[!train,] # not train
dim(Smarket.2005)
Direction.2005=Direction[!train]

## fit a logistic regression model using only the subset of the observations that correspond to dates before 2005
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, family=binomial,subset=train)
## obtain predicted probabilities of the stock market going up for each of the days in our test set—that is, for the days in 2005
glm.probs=predict(glm.fit,Smarket.2005,type="response")

## Notice that we have trained and tested our model on two completely sep- arate data sets: training was performed using only the dates before 2005, and testing was performed using only the dates in 2005. Finally, we compute the predictions for 2005 and compare them to the actual movements of the market over that time period. 
glm.pred=rep("Down",252) # creates 252 predictions of down elements, which matches dimensions from Smarket.2005
table(glm.pred, Direction.2005)
## transforms to Up all of the elements for which the predicted probability of a market increase exceeds 0.5 
glm.pred[glm.probs > 0.5] = "Up"
## produce a confusion matrix in order to determine how many observations were correctly or incorrectly classified 
table(glm.pred, Direction.2005)
## compute the fraction of days for which the prediction was correct
mean(glm.pred==Direction.2005)
mean(glm.pred!=Direction.2005) # test set error rate, 42%
## The test set error rate is actually worse than random guessing. Of course this result is not all that surprising, given that one would not generally expect to be able to use previous days’ returns to predict future market performance. 

## We recall that the logistic regression model had very underwhelming p- values associated with all of the predictors, and that the smallest p-value, though not very small, corresponded to Lag1. Perhaps by removing the variables that appear not to be helpful in predicting Direction, we can obtain a more effective model. After all, using predictors that have no relationship with the response tends to cause a deterioration in the test error rate (since such predictors cause an increase in variance without a corresponding decrease in bias), and so removing such predictors may in turn yield an improvement. Below we have refit the logistic regression using just Lag1 and Lag2, which seemed to have the highest predictive power in the original logistic regression model.
glm.fit = glm(Direction ~ Lag1 + Lag2, family=binomial, subset=train)
glm.probs = predict(glm.fit, Smarket.2005, type="response")
glm.pred = rep("Down", 252)
glm.pred[glm.probs > 0.5] = "Up"
table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005) 
mean(glm.pred != Direction.2005)
106/(106+76)
## Now the results appear to be more promising: 56 % of the daily movements have been correctly predicted. The confusion matrix suggests that on days when logistic regression predicts that the market will decline, it is only correct 50 % of the time. However, on days when it predicts an increase in the market, it has a 58 % accuracy rate. 

## Suppose that we want to predict the returns associated with particular values of Lag1 and Lag2. In particular, we want to predict Direction on a day when Lag1 and Lag2 equal 1.2 and 1.1, respectively, and on a day when they equal 1.5 and −0.8 
predict(glm.fit,newdata=data.frame(Lag1=c(1.2,1.5), Lag2=c(1.1,-0.8)),type="response")



