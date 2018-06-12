
## Chapter 4 lab - An application to Caravan Insurance Data

## This data set includes 85 predictors that measure demographic characteristics for 5,822 individuals. The response variable is Purchase, which indicates whether or not a given individual purchases a caravan insurance policy. 

library(ISLR)
library(class)
options(digits=3)
options(width=90)

dim(Caravan)
attach(Caravan)
summary(Purchase)
348/5822
## In this data set, only 6% of people purchased caravan insurance. 

## Standardize the data so that all variables are given a mean of zero and a standard deviation of one. Then all variables will be on a comparable scale. 
## We do this because the scale of variables matters with the KNN classifier, which predicts the class of a given test observation by identifying the observations that are nearest to it. Any variables that are on a large scale will have a much larger effect on the distance between the observations, and hence on the KNN classifier, than variables that are on a small scale. For instance, imagine a data set that contains two variables, salary and age (measured in dollars and years, respectively). As far as KNN is concerned, a difference of $1,000 in salary is enormous compared to a difference of 50 years in age. Conse- quently, salary will drive the KNN classification results, and age will have almost no effect. This is contrary to our intuition that a salary difference of $1, 000 is quite small compared to an age difference of 50 years. Further- more, the importance of scale to the KNN classifier leads to another issue: if we measured salary in Japanese yen, or if we measured age in minutes, then we’d get quite different classification results from what we get if these two variables are measured in dollars and years. 
## In standardizing the data, we exclude column 86, because that is the qualitative Purchase variable
standardized.X = scale(Caravan[, -86])
var(Caravan[, 1])
var(Caravan[, 2])
var(standardized.X[, 1])
var(standardized.X[, 2])
## Now every column of standardized.X has a standard deviation of 1 and a mean of zero.

## Split observations into test set (w/ first 1,000 observations) and training set (containing the rest).
test = 1:1000
train.X = standardized.X[-test, ] ## using numeric vector test, yields submatrix containing observations whose indices do not range from 1 to 1,000
test.X = standardized.X[test, ]
train.Y = Purchase[-test]
test.Y = Purchase[test]

## Fit a KNN model on the training data using K = 1, and evaluate its performance on the test data.
set.seed(1)
knn.pred = knn(train.X, test.X, train.Y, k = 1)
mean(test.Y != knn.pred)
mean(test.Y != "No")
## The KNN error rate on the 1,000 test observations is just under 12%. At first glance, this may appear to be fairly good. However, since only 6% of customers purchased insurance, we could get the error rate down to 6% by always predicting No regardless of the values of the predictors!
## Suppose that there is some non-trivial cost to trying to sell insurance to a given individual. For instance, perhaps a salesperson must visit each potential customer. If the company tries to sell insurance to a random selection of customers, then the success rate will be only 6%, which may be far too low given the costs involved. Instead, the company would like to try to sell insurance only to customers who are likely to buy it. So the overall error rate is not of interest. Instead, the fraction of individuals that are correctly predicted to buy insurance is of interest. 
 ##  It turns out that KNN with K = 1 does far better than random guessing among the customers that are predicted to buy insurance. Among 77 such customers, 9, or 11.7 %, actually do purchase insurance. This is double the rate that one would obtain from random guessing. 
table(knn.pred, test.Y)
9/(68+9) ## YesYes / (YesYes + YesNo)

## Using K = 3, the success rate increases to 19 % 
knn.pred = knn(train.X, test.X, train.Y, k = 3)
table(knn.pred, test.Y)
5/(20+5) ## YesYes / (YesYes + YesNo)

## and with K = 5 the rate is 26.7 %. This is over four times the rate that results from random guessing. 
knn.pred = knn(train.X, test.X, train.Y, k = 5)
table(knn.pred, test.Y)
4 / (4+11) ## YesYes / (YesYes + YesNo)
## It appears that KNN is finding some real patterns in a difficult data set! 

## As a comparison, we can also fit a logistic regression model to the data. (Warning message is OK)
glm.fit = glm(Purchase ~., data=Caravan, family=binomial, subset=-test)
glm.probs = predict(glm.fit, Caravan[test,], type="response")
## If we use 0.5 as the predicted probability cut-off for the classifier, then we have a problem: only seven of the test observations are predicted to purchase insurance. Even worse, we are wrong about all of these! 
glm.pred = rep("No", 1000)
glm.pred[glm.probs > 0.5] = "Yes"
table(glm.pred, test.Y)
## However, we are not required to use a cut-off of 0.5. If we instead predict a purchase any time the predicted probability of purchase exceeds 0.25, we get much better results: we predict that 33 people will purchase insurance, and we are correct for about 33% of these people. This is over five times better than random guessing! 
glm.pred = rep("No", 1000)
glm.pred[glm.probs > 0.25] = "Yes"
table(glm.pred, test.Y)
11/(22+11)
 


