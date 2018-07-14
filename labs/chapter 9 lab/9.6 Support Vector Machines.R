
####### 9.6 Lab: Support Vector Machines #######

## There are two approaches: 
## 1. One versus one classification. If we have more than two classes, this does a bunch of support vector machines for every pair of classes. If we have two classes, we obviously only need one support vector machine. But if we have three classes, then we would need three. 
## 2. k support vector machines. For each one we choose one dimension and compare it to one predictor and compare it to all the other predictors. So it's as if there are two classes. The dimension being considered is one class. And all the other dimensions sort of glommed together are the other class. 



####### 9.6.1 Support vector classifier #######

## The e1071 library contains implementations for a number of statistical learning methods. In particular, svm() can be used to fit a support vector classifier when the argument kernel="linear" is used. A cost argument allows us to specify the cost of a violation to the margin. When the cost argument is small, then the margins will be wide and many support vectors will be on the margin or will violate the margin. When the cost argument is large, then the margins will be narrow and there will be few support vectors on the margin or violating the margin.

## install.packages('e1071', repos='http://cran.us.r-project.org')

## Begin by generating the observations, which belong to two classes. 
## x is now normally distributed with 40 observations in 20x2 matrix
## y is now a vector consisting of ten copies of negative one followed by ten copies of positive one

set.seed(1)
x = matrix(rnorm(20*2), ncol=2)
y = c(rep(-1, 10), rep(1, 10))
x
y
plot(x, col=(3-y))

## Add 1 to all the x's that correspond to a y == 1. And all the x's that correspond to a minus 1 are left alone. This will have the effect of making them closer to being separable. 
x[y==1,] = x[y==1,] + 1

## Check whether the classes are linearly separable.

plot(x, col=(3-y))

## They are not. Basically because of the one red dot on the far left. 

## Next, we fit the support vector classifier. Note that in order for svm() to perform classification (as opposed to SVM-based regression), we must encode the response as a factor variable. We now create a data frame with the response coded as a factor.
## Notice that R lets x and y that we have created appear on the right side of the parameter assignment statements without confusing them with the parameter names, which can only appear on the left hand side of the assignments. 

dat = data.frame(x=x, y=as.factor(y))

## Fit a linear support vector classifier. Not the same thing as a support vector machine. This is less flexible than a support vector machine. 
library(e1071)
svmfit = svm(y ~ ., data=dat, kernel="linear", cost=10, scale=FALSE)
## scale=FALSE tells the svm() function not to scale each feature to have mean 0 or standard deviation 1; depending on the application, one might prefer to use scale=TRUE.

## Plot the support vector classifier obtained.
plot(svmfit, dat)
## The region of feature space that will be assigned to the -1 class is shown in light blue, and the region that will be assigned to the +1 class is shown in purple. The decision boundary between the two classes is linear (because we used the argument kernel="linear"), though due to the way in which the plotting function is implemented in this library the decision boundary looks somewhat jagged in the plot. We see that in this case only one observation is mislcassified (a red x in the blue space). (Note that here the second feature is plotted on the x-axis and the first feature is plotted on the y-axis, in contrast to the behavior of hte usual plot() function in R.) The support vectors are plotted as crosses and the remaining observations are plotted as circles. We see here that there are seven support vectors.

## Determine the identities of the seven support vectors, which determine the margin around the hyperplane. 
svmfit$index
## Indices of those x's: 1, 2, 5, 7, 14, 16, 17
## This means they correspond to these rows in x
x

## Using summary() we can obtain some basic information about the support vector classifier fit. 
summary(svmfit)
## We see that a linear kernel was used with cost=10, and that there are these seven support vectors, and that four of them (4 3) correspond to -1 (Levels: -1 1), and 3 correspond to 1. If we look at the plot, we can see that there are four black x's that are in the -1/blue area, and 2 red x's that are in the 1/purple area and 1 red x that is in the blue area. 

## What if we instead used a smaller value of the cost parameter? (This is the tuning parameter C in the book.) 
svmfit = svm(y~., data=dat, kernel="linear", cost=0.1, scale=FALSE)
plot(svmfit, dat)
svmfit$index
## Now that a smaller value of the cost parameter is being used, we obtain a larger number of support vectors, because the margin is now wider. Unfortunately, svm() doesn't explicitly output the coefficients of the linear decision boundary obtained when the support vector classifier is fit, nor does it output the width of the margin. 

## Now we cross-validate to determine what the best value of C is. 
## The e1071 library includes a built-in function, tune(), to perform cross-validation. By default, tune() performs ten-fold cross-validation on a set of models of interest. To use tune(), we pass in relevant info about the set of models that are under consideration. The following command indicates that we want to compare SVMs with a linear kernel, using a range of values of the cost parameter. 
set.seed(1)
tune.out = tune(svm, y~., data=dat, kernel="linear", ranges=list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100)))

## We can easily access the cross-validation errors for each of these models using summary()
summary(tune.out)
## Turns out a cost of 0.1 has the lowest error rate (which happens to be 0.1 too).

## The tune() function stores the best model obtained, which can be accessed as follows:
names(tune.out)
bestmod = tune.out$best.model
summary(bestmod)
## It has eight support vectors in both classes, eight for minus 1 and eight for 1.

## Test this model with test data
## First create test data using the same expression we used to create the training data. 
set.seed(1)
xtest = matrix(rnorm(20 * 2), ncol=2)
ytest = sample(c(-1, 1), 20, rep=TRUE)
xtest[ytest==1,] = xtest[ytest==1,] + 1
testdat = data.frame(x=xtest, y=as.factor(ytest))
## Now we predict the class labels of these test observations. Here we use the best model obtained through cross-validation in order to make predictions. 
ypred = predict(bestmod, testdat)
table(predict=ypred, truth=testdat$y)
## Thus with this value of cost (0.1), 18 of the test observations are correctly classified. (This table of results turns out differently sometimes...)

## What if we had instead used cost=0.01? See if cross-validating really made a difference.
set.seed(1)
svmfit = svm(y~., data=dat, kernel="linear", cost=0.01, scale=FALSE)
ypred = predict(svmfit, testdat)
table(predict=ypred, truth=testdat$y)
## Again I get randomly different results every time I run this...? But they're always worse than the best model. Here I have 15 observations that are correctly classified, meaning that 3 additional observations got misclassified.

## Now consider a situation in which the two classes are linearly separable. Then we can find a separating hyperplane using svm(). We first further separate the two classes in our simulated data so that they are linearly separable. 
x[y==1, ] = x[y==1,] + 0.5
plot(x, col=(y+5)/2, pch=19)
## Now the observations are just barely linearly separable.

## We fit the support vector classifier and plot the resulting hyperplane, using a very large value of cost so that no observations are misclassified. 
dat = data.frame(x=x, y=as.factor(y))
svmfit = svm(y~., data=dat, kernel="linear", cost=1e5)
summary(svmfit)
plot(svmfit, dat)
## No training errors were made and only 3 support vectors were used. However, we can see from the figure that the margin is very narrow (because the observations that are not support vectors, indicated as circles, are very close to the decision boundary). It seems likely that this model will perform poorly on test data.

## Try a smaller value of cost:
svmfit = svm(y~., data=dat, kernel="linear", cost=1)
summary(svmfit)
plot(svmfit, dat)
## Again lowering the cost, we get a lot more support vectors and a wider margin. Using cost=1, we misclassify a training observation. Despite this, it seems likely that this model will perform better on test data than the model with cost=1e5. 



####### 9.6.2 Support vector machine #######

## In order to fit an SVM using a non-linear kernel, we once again use the svm() function. However, now we use a different value of hte parameter kernel. To fit a SVM with a polynomial kernel, we use kernel="polynomial", and to fit a SVM with a radial kernel we use kernel="radial". In the former case we also use the degree argument to specify a degree for the polynomial kernel, and in the latter case we use gamma to specify a value of γ for the radial basis kernel.

## First generate some data with a non-linear class boundary.

set.seed(1)
x = matrix(rnorm(200*2), ncol=2)
## x is now normally distributed with 400 observations in 200x2 matrix
## And we're pushing 150 of those points. The first hundred are being pushed in the positive direction, and the next 50 are pushed in the negative direction. The last 50 are left alone. 
x[1:100,] = x[1:100,] + 2
x[101:150,] = x[101:150,] - 2
y = c(rep(1,150), rep(2,50))
## y, we're making into a bunch of ones and twos, so we'll have 150 ones and 50 twos
dat = data.frame(x=x, y=as.factor(y))

## Plotting the data makes it clear that the class boundary is indeed non-linear
plot(x, col=y)

## The data is randomly split into training and testing groups. We then fit the training data using svm() with a radial kernel and γ = 1. 
train = sample(200, 100)
## Note: this notation means sample size of 100 from among integers 1 to 200 without replacement
train
svmfit = svm(y ~., data=dat[train, ], kernel="radial", gamma=1, cost=1)
plot(svmfit, dat[train,])
## The plot shows that the resulting SVM has a decidely non-linear boundary. 
## The purple area represents the last 50 observations in x that were left alone. The x symbols are all the support vectors around it. There are some training errors in this SVM fit. 

## summary() can be used to obtain some info about the SVM fit
summary(svmfit)
## We have 37 support vectors, 17 for the class labeled 1 and 20 for the class labeled 2. 

## If we increase the value of cost (decreasing the margin and decreasing the number of support vectors), we can reduce the number of training errors. However, this comes at the price of a more irregular decision boundary that seems to be at risk of overfitting the data. 
svmfit = svm(y~ ., data=dat[train,], kernel="radial", gamma=1, cost=1e5)
plot(svmfit, dat[train,])
summary(svmfit)

## Cross-validate to select the best choice of γ and cost for an SVM with a radial kernel:
set.seed(1)
tune.out = tune(svm, y~., data=dat[train,], kernel="radial", ranges=list(cost=c(0.1, 1, 10, 100, 1000), gamma=c(0.5, 1, 2, 3, 4)))
summary(tune.out)
## The lowest error is 0.12, which means that the best choice of parameters involves cost = 1 and gamma = 2. 

## Use this model to make a prediction on the test set.
table(true=dat[-train, "y"], pred=predict(tune.out$best.model, newdata=dat[-train,]))
(74 + 16) / (74 + 16 + 7 + 3)
## 10% of test observations are misclassified by this SVM. 90% are classified correctly. 



####### 9.6.3 ROC curves #######

## install.packages('ROCR', repos='http://cran.us.r-project.org')
library(ROCR)

## First write a short function to plot an ROC curve given a vector containing a numerical score for each observation, pred, and a vector containing the class label for each observation, truth.
rocplot = function(pred, truth, ...) {
	predob = prediction(pred, truth)
	perf = performance(predob, "tpr", "fpr")
	plot(perf, ...)
}

## SVMs and support vector classifiers output class labels for each observation. However, it is also possible to obtain fitted values for each observation, which are the numerical scores used to obtain the class labels. 
## In essence, the sign of the fitted value determines on which side of hte decision boundary the observation lies. Therefore, the relationship between the fitted value and hte class prediction for a given observation is simple: if the fitted value exceeds zero then the observation is assigned to one class, and if it is less than zero then it is assigned to the other. In order to obtain the fitted values for a given SVM model fit, we use decision.values=TRUE when fitting svm(). Then the predict() function will output the fitted values.

## Output fitted values with decision.values=T and the optimal values of a SVM with a radial kernel found above with cross-validation
svmfit.opt = svm(y~., data=dat[train,], kernel="radial", gamma=2, cost=1, decision.values=T)
fitted = attributes(predict(svmfit.opt, dat[train,], decision.values=T))$decision.values

## Now we can produce the ROC plot.
par(mfrow=c(1,2))
rocplot(fitted, dat[train, "y"], main="Training Data")
## SVM appears to be producing accurate predictions. What we want is for the curve to hug the top left corner. The more it hugs the corner, the tighter the fit. 

## By increasing γ we can produce a more flexible fit and generate further improvements in accuracy.
svmfit.flex = svm(y~., data=dat[train, ], kernel="radial", gamma=50, cost=1, decision.values=T)
fitted = attributes(predict(svmfit.flex, dat[train,], decision.values=T))$decision.values
rocplot(fitted, dat[train, "y"], add=T, col="red")
## This curve doesn't look very good, but it does hug the corner more tightly

## However, these ROC curves are all on the training data. We are really more interested in the level of prediction accuracy on the test data. 

## Try computing the ROC curves on the test data
fitted = attributes(predict(svmfit.opt, dat[-train,], decision.values=T))$decision.values
rocplot(fitted, dat[-train, "y"], main="Test Data")
fitted = attributes(predict(svmfit.flex, dat[-train,], decision.values=T))$decision.values
rocplot(fitted, dat[-train, "y"], add=T, col="red")
## The model with γ = 2 appears to provide the most accurate results. This is intuitive. We should expect that the more flexible we are with modeling the training data, the more likely we are to overfit on the training data and to be out of touch with the test data.



####### 9.6.4 SVM with multiple classes #######

## If the response is a factor containing more than two levels, then svm() will perform multi-class classification using the one-versus-one approach. 

## Explore this here by generating a third class of observations.
set.seed(1)
x = rbind(x, matrix(rnorm(50*2), ncol=2)) ## now we have 250 observations (before we had 200)
y = c(y, rep(0,50))
x[y==0,2] = x[y==0,2] + 2
dat = data.frame(x=x, y=as.factor(y))
par(mfrow=c(1,1))
plot(x, col=(y+1))

## Fit an SVM with a radial kernel to the data. 
svmfit = svm(y~ ., data=dat, kernel="radial", cost=10, gamma=1)
plot(svmfit, dat)

## The e1071 library can also be used to perform support vector regression, if the response vector that is passed in to svm() is numerical rather than a factor. 



####### 9.6.5 Application to gene expression data #######

## We now examine the Khan data set, which consists of a number of tissue samples corresponding to four distinct types of small round blue cell tumors. For each tissue sample, gene expression measurements are available. The data set consists of training data, xtrain and ytrain, and testing data, xtest and ytest.

library(ISLR)
names(Khan)
dim(Khan$xtrain) ## 63 rows and 2308 columns
dim(Khan$xtest) ## 
length(Khan$ytrain)
length(Khan$ytest)

## The data set consists of expression measurements for 2,308 genes. The training and test sets consist of 63 and 20 observations respectively.

table(Khan$ytrain)
table(Khan$ytest)

## We will use a support vector approach to predict cancer subtype using gene expression measurements. In this data set, there are a very large number of features relative to the number of observations. This suggests that we should use a linear kernel, because the additional flexibility that will result from using a polynomial or radial kernel is unnecessary.

dat = data.frame(x = Khan$xtrain, y=as.factor(Khan$ytrain))
out = svm(y~., data=dat, kernel="linear", cost=10)
summary(out)
## We have 58 support vectors. 
table(out$fitted, dat$y)
## We see that there are no training errors. We have 12 correctly classified as 3, 20 correctly classified as 4
## In fact, this is not surprising, because the large number of variables relative to the number of observations implies that it is easy to find hyperplanes that fully separate the classes. 

## We are most interested in not the support vector classifier's performance on the training observations, but rather its performance on the test observations. 
dat.te = data.frame(x=Khan$xtest, y=as.factor(Khan$ytest))
pred.te = predict(out, newdata=dat.te)
table(pred.te, dat.te$y)
## we see that using cost=10 yields two test set errors on this data.
