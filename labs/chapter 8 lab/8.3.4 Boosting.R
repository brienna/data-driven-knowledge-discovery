
## 8.3.4 Boosting

library(MASS)
fix(Boston)
attach(Boston)
names(Boston)

## gbm() fits boosted regression trees to data. Run gbm() with the option distribution="gaussian" for regression, and distribution="bernoulli" for binary classification. The argument n.trees=5000 indicates that we want 5000 trees, and the option interaction.depth=4 limits the depth of each tree.

## install.packages('gbm', repos='http://cran.us.r-project.org')
library(gbm)

## Create a training set and test set for the Boston data set
set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2)
boston.test = Boston[-train, "medv"]

## Fit boosted regression trees to the Boston data set.
boost.boston = gbm(medv ~., data=Boston[train,], distribution="gaussian", n.trees=5000, interaction.depth=4)

## summary() produces a relative influence plot and also outputs the relative influence statistics
summary(boost.boston)
## We see that lstat and rm are by far the most important variables. 

## We can also produce partial dependence plots for these two variables. These plots illustrate the marginal effect of the selected variables on the response after integrating out the other variables. In this case, as we might expect, median house prices are increasing with rm and decreasing with lstat. 

par(mfrow=c(1,2))
plot(boost.boston, i="rm")
plot(boost.boston, i="lstat")

## We now use the boosted model to predict medv on the test set. 

yhat.boost = predict(boost.boston, newdata=Boston[-train,], n.trees=5000)
mean((yhat.boost-boston.test) ^ 2)
## The test MSE obtained is 11.8, similar to the test MSE for random forests and superior to that for bagging. 

## If we want to, we can perform boosting with a different value of the shrinkage parameter λ. The default value is 0.001, but this is easily modified. Here we take λ = 0.2.
boost.boston = gbm(medv ~ ., data=Boston[train,], distribution="gaussian", n.trees=5000, interaction.depth=4, shrinkage=0.2, verbose=F)
yhat.boost = predict(boost.boston, newdata=Boston[-train,], n.trees=5000)
mean((yhat.boost - boston.test) ^ 2)
## In this case, using λ = 0.2 leads to a slightly lower test MSE than λ = 0.001. 



