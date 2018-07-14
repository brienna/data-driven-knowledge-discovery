
## 8.3.1 Fitting classification trees

## The tree library is used to fit classification and regression trees
## install.packages('tree', repos='http://cran.us.r-project.org')
library(tree)

## We will use classification trees to analyze the Carseats data set. 

library(ISLR)
fix(Carseats)
attach(Carseats)
names(Carseats)

## In Carseats, we will use Sales as the response. It is a continuous/quantitative variable, so we begin by recoding it as a binary variable. We use ifelse() to create a variable, called High, which takes on a value of Yes if the Sales variable exceeds 8, and takes on a value of No otherwise. 

?ifelse()
High = ifelse(Sales >= 8, "Yes", "No")

## Use data.frame() to merge High with the rest of the Carseats data, so that High becomes a new column.

Carseats = data.frame(Carseats, High)

## Fit a classification tree in order to predict High using all variables but Sale. 
tree.carseats = tree(High ~ .-Sales, Carseats)

## List the variables that are used as internal nodes in the tree, the number of terminal nodes, and the (training) error rate.
summary(tree.carseats)
## We've used 8 of the original 11 variables. Sales is a response variable, so there are really 10 predictors. And of those 10 predictors we've used 8 in this model. We have 27 terminal nodes. Our training error rate is 9%. 
## A small deviance indicates a tree that provides a good fit to the training data. The deviance is 170.7 (not the residual mean deviance). 

## Display the tree structure and the node labels
plot(tree.carseats)
text(tree.carseats, pretty=0)
## pretty=0 instructs R to include the category names for any qualitative predictors, rather than simply displaying a letter for each category
## The root of the tree is split at ShelveLoc. ShelveLoc is the first variable that splits this tree. Price is the second one. This makes sense, that shelf location and price would be the two most important variables. 

## If we just type the name of the tree object, R prints output corresponding to each branch of hte tree. R displays the split criterion (e.g. Price < 92.5), the number of observations in that branch, the deviance, the overall prediction for the branch (Yes or No), and the fraction of observations in that branch that take on values of Yes and No. Branches that lead to terminal nodes are indicated using asterisks. 
tree.carseats

## Test the model on the test data
## To properly evaluate the performance of a classification tree on these data, we must estimate the test error rather than simply computing the training error. We split the observations into a training set and a test set, build the tree using the training set, and evaluate its performance on the test data. We can use predict(), passing it type="class" to tell R to return the actual class prediction. 
set.seed(2)
train = sample(1:nrow(Carseats), 200)
Carseats.test = Carseats[-train, ]
High.test = High[-train]
tree.carseats = tree(High ~ .-Sales, Carseats, subset=train)
tree.pred = predict(tree.carseats, Carseats.test, type="class")
## Take a table of the results
table(tree.pred, High.test)
(86 + 57) / 200
## We correctly predict the No/No and Yes/Yes. So we made correct predictions for 71.5% of the data. 

## Is this 27 node tree optimal? We can consider whether pruning the tree might lead to improved results. Use cross validation cv.tree() to determine the optimal level of tree complexity; cost complexity pruning is used in order to select a sequence of trees for consideration. 
## FUN=prune.misclass tells R we want the classification error rate to guide the cross-validation and pruning process, rather than the default for cv.tree(), which is deviance. 
## cv.tree() reports the number of terminal nodes of each tree considered (size) as well as the corresponding error rate and the value of the cost-complexity parameter used.
set.seed(3)
cv.carseats = cv.tree(tree.carseats, FUN=prune.misclass)
names(cv.carseats)
cv.carseats
## Note that, despite the name, dev corresponds to the cross-validation error rate in this instance. 
## Find the lowest cross-validation error rate. It is 50 cross-validation errors. This is the tree with 9 terminal nodes. Its k (or alpha as in the book) is 1.75.

## Plot the error rate as a function of both size and k. 
par(mfrow=c(1,2))
plot(cv.carseats$size, cv.carseats$dev, type="b")
plot(cv.carseats$k, cv.carseats$dev, type="b")
## Left plot shows as expected, where classification error rate is lowest is when size is 9. 
## Lowest alpha on right plot is 1.75.

## Apply prune.misclass() to prune the tree to obtain the best subtree.
prune.carseats = prune.misclass(tree.carseats, best=9)
plot(prune.carseats)
text(prune.carseats, pretty=0)

## How well does this pruned tree perform on the test data set? 
tree.pred = predict(prune.carseats, Carseats.test, type="class")
table(tree.pred, High.test)
(94 + 60) / 200
## Now 77% of the test observations are correctly classified, so not only has the pruning process produced a more interpretable tree, but it has also improved the classification accuracy.

## If we increase the value of best, we obtain a larger pruned tree with lower classification accuracy. 
prune.carseats = prune.misclass(tree.carseats, best = 15)
plot(prune.carseats)
text(prune.carseats, pretty=0)
tree.pred=predict(prune.carseats, Carseats.test, type="class")
table(tree.pred, High.test)
(86 + 62) / 200
## 74% 