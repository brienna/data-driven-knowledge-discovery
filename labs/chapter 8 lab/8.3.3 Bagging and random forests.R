
## 8.3.3 Bagging and random forests

library(MASS)
fix(Boston)
attach(Boston)
names(Boston)


########## BAGGING ##########

## Bagging is simply a special case of a random forest with m = p. Therefore, the randomForest() function can be used to perform both random forests and bagging. 

## Create a training set and test set for the Boston data set
set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2)
boston.test = Boston[-train, "medv"]

## Perform bagging: 
library(randomForest)
set.seed(1)
bag.boston = randomForest(medv ~., data=Boston, subset=train, mtry=13, importance=TRUE)
bag.boston

## The argument mtry = 13 indicates that all 13 predictors should be considered for each split of the tree — in other words, that bagging should be done. 

## How well does this bagged model perform on the test set? 

yhat.bag = predict(bag.boston, newdata=Boston[-train,])
plot(yhat.bag, boston.test)
abline(0, 1)
mean((yhat.bag - boston.test) ^ 2)

## The test set MSE associated with the bagged regression tree is 13.51, almost half that obtained using an optimally-pruned single tree. 

## We could change the number of trees grown by randomForest using the ntree argument.

bag.boston = randomForest(medv ~ ., data=Boston, subset=train, mtry=13, ntree=25)
yhat.bag = predict(bag.boston, newdata=Boston[-train,])
mean((yhat.bag - boston.test) ^ 2)


########## RANDOM FORESTS ##########

## Growing a random forest proceeds in exactly the same way, except that we use a smaller value of the mtry argument. By default, randomForest() uses p/3 variables when building a random forest of regression trees, and sqrt(p) variables when building a random forest of classification trees. Here we use mtry = 6. 

set.seed(1)
rf.boston = randomForest(medv ~., data=Boston, subset=train, mtry=6, importance=TRUE)
yhat.rf = predict(rf.boston, newdata=Boston[-train,])
mean((yhat.rf - boston.test) ^ 2)

## The test set MSE is 11.31; this indicates that random forests yielded an improvement over bagging in this case. 

## importance() lets us see the importance of each variable.
importance(rf.boston)
## Two measures of variable importance are reported. The former is based upon the mean decrease of accuracy in predictions on the out of bag samples when a given variable is excluded from the model. The latter is a measure of the total decrease in node impurity that results from splits over that variable, averaged over all trees. In the case of regression trees, the node impurity is measured by the training RSS, and for classification trees by the deviance. Plots of these importance measures can be produced using the varImpPlot() function.
varImpPlot(rf.boston)

## The results indicate that across all of the trees considered in the random forest, the wealth level of the community (lstat) and the house size (rm) are by far the two most important variables. 

















