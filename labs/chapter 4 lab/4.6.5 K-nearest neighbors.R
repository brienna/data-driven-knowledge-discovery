
## Chapter 4 lab - K-nearest neighbors

## knn() works rather differently from the other model-fitting functions. Rather than a two-step approach in which we first fit the model and then we use the model to make predictions, knn() forms predictions using a single command. The function requires 4 inputs: 
## 1) a matrix containing the predictors associated with the training data, labeled train.X below
## 2) a matrix containing the predictors associated with the data for which we wish to make predictions, labeled test.X below
## 3) a vector containing the class labels for the training observations, labeled train.Direction below
## 4) a value for K, the number of nearest neighbors to be used by the classifier

library(ISLR)
attach(Smarket)

## create a vector corresponding to the observations from 2001 through 2004 
train = (Smarket$Year < 2005)
summary(train)
Smarket.2005 = Smarket[!train,] # not train 
dim(Smarket.2005)
Direction.2005=Direction[!train]

## Bind Lag1 and Lag2 predictors together into two matrices, one for the training set and the other for the test set
library(class)
train.X = cbind(Lag1, Lag2)[train,]
test.X = cbind(Lag1, Lag2)[!train,]
train.Direction = Direction[train]

## Predict the market's movement for the dates in 2005. (We set a random seed before we apply knn() because if several observations are tied as nearest neighbors, then R will randomly break the tie. Therefore, a seet must be set in order to ensure reproducibility of results.)
set.seed(1)
knn.pred = knn(train.X, test.X, train.Direction, k=1) 
table(knn.pred, Direction.2005)
mean(knn.pred == Direction.2005) ## also calculated via (83 + 43) / 252
## The results using K = 1 are not very good, since only 50% of the observations are correctly predicted. Of course it may be that K = 1 results in an overly flexible fit to the data. 

## Repeat the analysis using K = 3
knn.pred = knn(train.X, test.X, train.Direction, k = 3)
table(knn.pred, Direction.2005)
mean(knn.pred == Direction.2005)
## The results have improved slightly. But increasing K further turns out to provide no further improvements. It appears that for this data, QDA provides the best results of the methods we have examined so far. 


