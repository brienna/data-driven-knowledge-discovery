## Exercise 8.4.8
## Brienna Herold
## Summer 2018



# In the lab, a classification tree was applied to the Carseats data set after converting Sales into a qualitative response variable. Now we will seek to predict Sales using regression trees and related approaches, treating the response as a quantitative variable. 

library(tree)
library(ISLR)
fix(Carseats)
attach(Carseats)
names(Carseats)
## Source for info about data set: https://cran.r-project.org/web/packages/ISLR/ISLR.pdf




# (a)  Split the data set into a training set and a test set. 

set.seed(1)
Carseats.train = sample(1:nrow(Carseats), nrow(Carseats)/2)
Carseats.test = Carseats[-Carseats.train, ]




# (b)  Fit a regression tree to the training set. Plot the tree, and interpret the results. What test error rate do you obtain? 

## Fit a regression tree to the training set to predict Sales using all variables
tree.carseats = tree(Sales ~ ., Carseats, subset=Carseats.train)

## Plot the tree
plot(tree.carseats)
text(tree.carseats, pretty=0, cex=0.5)

## ![18-node regression tree](viia.pdf)

## ShelveLoc is the first variable that splits the tree. Price is the second one. It makes sense that shelf location and price would be the two most important variables.
## Example results: The tree predicts a sales of 12,080 car seats if the shelving location is medium and the price is under $113, and a sales of 2,249 car seats if the shelving location is bad, price is over $120.5, average age of the local population is over 66.5, and price is over $132. 

## List predictors that are used as internal nodes in the tree, the number of terminal nodes, & training error rate
summary(tree.carseats)
## 6 of the 10 predictors are used in constructing the tree: ShelveLoc, Price, Age, Advertising, Income, CompPrice. We have 18 terminal nodes. 

## Test the model on the test data to evaluate the performance of the regression tree
set.seed(1) 
tree.pred = predict(tree.carseats, Carseats.test)
plot(tree.pred, Carseats.test$Sales) ## Note: Carseats.test$Sales = Carseats.test[, "Sales"]
abline(0, 1)

## ![Performance of 18-node regression tree on test data](viib.pdf)

mean((tree.pred - Carseats.test$Sales) ^ 2) 
## Test MSE associated with the regression tree = 4.15. The square root of this test MSE is around 2.04, indicating that this model leads to test predictions that are within around 2,004 units of the true sales amount of car seats. 




# (c)  Use cross-validation in order to determine the optimal level of tree complexity. Does pruning the tree improve the test error rate? 

## Determine optimal level of tree complexity using cross-validation
set.seed(1)
cv.carseats = cv.tree(tree.carseats)
names(cv.carseats) 
cv.carseats
lowestCVError = min(cv.carseats$dev)
lowestCVError
cv.carseats$size[which.min(cv.carseats$dev)]
plot(cv.carseats$size, cv.carseats$dev, type='b', cex=2, pch=20)
points(cv.carseats$size[which.min(cv.carseats$dev)], lowestCVError, col="red", cex=2, pch=20)

## ![Pruned trees and their deviances](viic.pdf)

## Lowest deviance is 1109.84, which comes from the tree with 8 nodes, suggesting that this tree is more optimal than the 18 node tree 

## Prune the 18 node tree to obtain the 8 node tree
prune.carseats = prune.tree(tree.carseats, best=8)
plot(prune.carseats)
text(prune.carseats, pretty=0, cex=0.5)

## ![8-node regression tree](viid.pdf)

## Test the pruned tree on the test data to see if it improves the test error rate
tree.pred = predict(prune.carseats, Carseats.test)
plot(tree.pred, Carseats.test$Sales)
abline(0, 1)

## ![Performance of 8-node regression tree on test data](viie.pdf)

mean((tree.pred - Carseats.test$Sales) ^ 2)
## Test MSE associated with the 8 node tree = 5.09. Apparently pruning the tree doesn't improve the test error rate. Shrug. 




# (d)  Use the bagging approach in order to analyze this data. What test error rate do you obtain? Use the importance() function to determine which variables are most important. 

## Perform bagging (on all 10 predictors, since bagging is a special case of a random forest with m = p)
## install.packages('randomForest', repos='http://cran.us.r-project.org')
library(randomForest)
set.seed(1)
p = ncol(Carseats) - 1 ## num of predictors; - 1 accounts for Sales being response variable
bag.carseats = randomForest(Sales ~ ., data=Carseats, subset=Carseats.train, mtry=p, importance=T)
bag.carseats

## Test how well this bagged regression tree performs on the test data
bag.pred = predict(bag.carseats, Carseats.test)
plot(bag.pred, Carseats.test$Sales)
abline(0, 1)

## ![Performance of bagged regression tree on test data](viif.pdf)

mean((bag.pred - Carseats.test$Sales) ^ 2)
## Test MSE is 2.615, which is much smaller than that obtained using the 18-node regression tree.

## Determine which variables are most important
importance(bag.carseats)
varImpPlot(bag.carseats)

## ![Variable importance measures](viig.pdf)

## Price and ShelveLoc are the two most important variables considered in the bagged regression tree.




# (e) Use random forests to analyze this data. What test error rate do you obtain? Use the importance() function to determine which variables are most important. Describe the effect of m, the number of variables considered at each split, on the error rate obtained. 

## Grow a random forest of regression trees with mtry = 6
## Note: Growing a random forest is like bagging, but w/ a smaller value of mtry. By default, randomForest() uses p/3 variables when building a random forest of regression trees. 
set.seed(1)
rf.carseats = randomForest(Sales ~ ., data=Carseats, subset=Carseats.train, mtry = 6, importance=TRUE)

## Test the random forest on the test data
rf.pred = predict(rf.carseats, Carseats.test)
mean((rf.pred - Carseats.test$Sales) ^ 2)
## The test MSE is 2.74. This is a little bit worse than the bagged regression tree but better than the 18-node regression tree. 
## Seems like a lower mtry, the number of variables considered at each split, increases the error rate. This indicates it's better to consider more variables at each split to better predict the true Sales. 

## Determine which variables are most important
importance(rf.carseats)
varImpPlot(rf.carseats)

## ![Variable importance measures](viih.pdf)

## Price and ShelveLoc again are the two most important variables across all of the trees considered in the random forest. 




