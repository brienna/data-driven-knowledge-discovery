
## 8.3.2 Fitting regression trees

library(tree)
library(MASS)
fix(Boston)
attach(Boston)
names(Boston)

## Create a training set and fit the tree to the training data

set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston = tree(medv ~ ., Boston, subset=train)
summary(tree.boston)

## summary() indicates that only 3 of the variables have been used in constructing the tree. In the context of a regression tree, the deviance is simply the sum of squared errors for the tree. 

## Plot the tree

plot(tree.boston)
text(tree.boston, pretty=0, cex=0.5)

## The variable lstat measures the percentage of individuals with lower socioeconomic status. The tree indicates that lower values of lstat correspond to more expensive houses. The tree predicts a median house price of $46,400 for larger homes in suburbs in which residents have high socioeconomic status (rm >= 7.437 and lstat < 9.715).

## See if pruning the tree will improve performance. Do this by using cross-validation to see how subtrees might perform.

cv.boston = cv.tree(tree.boston)
plot(cv.boston$size, cv.boston$dev, type='b')

## To actually prune the tree:
prune.boston = prune.tree(tree.boston, best=5)
plot(prune.boston)
text(prune.boston, pretty=0, cex=0.5)

## In keeping with cross-validation results which shows the unpruned tree as the best choice, we use the unpruned tree to make predictions on the test set.
yhat = predict(tree.boston, newdata=Boston[-train,])
boston.test = Boston[-train, "medv"]
plot(yhat, boston.test)
abline(0, 1)
mean((yhat - boston.test) ^ 2)

## The test MSE associated with the regression tree is 25.05. The square root of the MSE is therefore around 5.005, indicating that this model leads to test predictions that are within around $5,005 of the true median home value for the suburb. 






