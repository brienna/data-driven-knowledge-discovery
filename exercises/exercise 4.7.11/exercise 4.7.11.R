## Exercise 4.7.11
## Brienna Herold
## Summer 2018



# In this problem, you will develop a model to predict whether a given car gets high or low gas mileage based on the Auto data set. 

## Load data (change working directory first)
Auto = read.csv("Auto.csv", header=T, na.strings="?")

## View info about the data
## Note: origin - Origin of car (1. American, 2. European, 3. Japanese)
fix(Auto)
summary(Auto)
dim(Auto)

## Remove any rows containing missing observations
Auto = na.omit(Auto)
dim(Auto) ## 5 rows were removed 


# (a) Create a binary variable, mpg01, that contains a 1 if mpg contains a value above its median, and a 0 if mpg contains a value below its median. You can compute the median using the median() function. Note you may find it helpful to use the data.frame() function to create a single data set containing both mpg01 and the other Auto variables. 

## I just added mpg01 to the Auto dataset itself... hope this is OK
?ifelse
Auto$mpg01 = ifelse(Auto$mpg > median(Auto$mpg), 1, 0)
summary(Auto)
attach(Auto)


# (b) Explore the data graphically in order to investigate the association between mpg01 and the other features. Which of the other features seem most likely to be useful in predicting mpg01? Scatterplots and boxplots may be useful tools to answer this question. Describe your findings. 

## Generate scatterplot matrix of features minus "Name"
plot(Auto[,-9])
## Looking at the column/row for mpg01, it seems like we can't see any relationship on this type of graph, because the value of mpg01 is 0 or 1. We could probably look at mpg though, because mpg01 is based on mpg. Displacement, horsepower, weight seem to have a defined negative curvilinear relationship with mpg. Year might have a positive linear relationship with mpg, and origin and cylinders might have categorical relationships with mpg. 

## Color code the scatterplots by mpg01 levels
colors = ifelse(mpg01, 'green', 'red') # green, mpg = 1; red, mpg = 0
plot(Auto[,-9], col=colors)
## We can see some relationships now. Based on the color separations and slopes, displacement, horsepower, and weight seem to have a substantial relationship with mpg01. But year might not, because although the data points appeared to have a small slope, the colors show otherwise. Similarly, as the colors spread across the categories, origin and cylinders probably don't have categorical relationships with mpg01. 

## Generate correlation matrix for all pairwise correlations among the features
cor(Auto[, -9])
## Looking at mpg01 column, aside from mpg, we can see that displacement, horsepower, weight, cylinders have a substantial negative correlation with mpg01. Cylinders has the strongest correlation. Origin has a perhaps weak correlation.

## Plot boxplot for each quantitative feature vs. mpg01
quantAuto = Auto[,-9]
summary(quantAuto)
par(mfrow=c(3,3))
for (i in 1:length(quantAuto)) {
	boxplot(quantAuto[, i] ~ mpg01, xlab="mpg01", ylab=names(quantAuto[i]), col=c('red', 'green'))
}
## Box plots are are a quick and efficient way to visualize a relationship between a qualitative and a quantitative variable. We can see a clear, strong relationship between mpg/mpg01, because mpg01 is based on mpg. 0 mpg01 happens at lower mpg, whereas 1 mpg01 happens at higher mpg. This is what we expect to see, since we assigned mpg01 to contain a 1 if mpg contains a value above its median, and a 0 if mpg contains a value below its median.
## All variables show some relationship with mpg01, but the most pronounced are weight, displacement, and horsepower.
## The weight, displacement, and horsepower distributions of 0 mpg01 are all visually higher than the 1's distributions. This suggests that higher mpg happens more often at lower weights, displacement, and horsepower.
## Am not sure if cylinders has a very strong relationship with mpg01. It appears to have a skewed plot, which might be the case as it is actually a qualitative feature although recognized by R as numeric. 

## Plot the qualitative features (cylinders and origin)
par(mfrow=c(1,2))
cCounts = table(Auto$cylinders,mpg01)
barplot(cCounts, xlab="mpg01", ylab="cylinders", beside=T, legend=rownames(cCounts))
oCounts = table(Auto$origin, mpg01)
barplot(oCounts, xlab="mpg01", ylab="origin", beside=T, legend=rownames(oCounts))
## cylinders seems to have some relationship with mpg01. It is much more common for a 4 cylinder vehicle to have higher mpg than lower mpg, whereas it is more common for 6 or 8 cylinder vehicles to have lower mpg than higher mpg. There are not many 3 or 5 cylinder vehicles. 
## origin also seems to have some relationship with mpg01. Category 1 origin, or American vehicles, seem to really consume gas, whereas Category 2 and 3 origins, European and Japanese vehicles, seem to have better mileage.



# (c) Split the data into a training set and a test set. 

## Halve data into train and test vectors and matrices
set.seed(1)
?sample
## Have sample draw a random number from 1 to nrow(Auto)
train = sample(x=1:dim(Auto)[1], size=0.5*dim(Auto)[1], rep=FALSE)
test = -train
Auto.train = Auto[train,]
Auto.test = Auto[test,]
dim(Auto.train)
dim(Auto.test)
summary(Auto.train)
summary(Auto.test)



# (d) Perform LDA on the training data in order to predict mpg01 using the variables that seemed most associated with mpg01 in (b). What is the test error of the model obtained? 

set.seed(1)
library(MASS)
?lda
lda.fit = lda(mpg01 ~ displacement+horsepower+weight+cylinders, subset=train)
lda.fit
plot(lda.fit)
lda.pred = predict(lda.fit, Auto.test)
names(lda.pred)
## Compute confusion matrix
table(lda.pred$class, Auto.test$mpg01)
mean(lda.pred$class != Auto.test$mpg01)
## Test error rate 10.2%



# (e) Perform QDA on the training data in order to predict mpg01 using the variables that seemed most associated with mpg01 in (b). What is the test error of the model obtained? 

set.seed(1)
?qda
qda.fit = qda(mpg01 ~ displacement+horsepower+weight+cylinders, subset=train)
qda.fit
qda.class = predict(qda.fit, Auto.test)$class
table(qda.class, Auto.test$mpg01)
mean(qda.class != Auto.test$mpg01)
## 12.24% error rate



# (f) Perform logistic regression on the training data in order to predict mpg01 using the variables that seemed most associated with mpg01 in (b). What is the test error of the model obtained? 

set.seed(1)
?glm
## Fit a logistic regression model using training data
glm.fit = glm(mpg01 ~ displacement+horsepower+weight+cylinders, family=binomial, data=Auto, subset=train)
summary(glm.fit)
par(mfrow=c(2,2))
plot(glm.fit)
## Obtain predicted probabilities of 0 or 1 mpg01 for each vehicle in our test set, given values of the predictors
glm.probs = predict(glm.fit, Auto.test, type="response")
## Classify them
glm.pred = ifelse(glm.probs > 0.5, "1", "0")
table(glm.pred, Auto.test$mpg01)
mean(glm.pred != Auto.test$mpg01)
## 10.2% test error rate



# (g) Perform KNN on the training data, with several values of K, in order to predict mpg01. Use only the variables that seemed most associated with mpg01 in (b). What test errors do you obtain? Which value of K seems to perform the best on this data set? 

library(class)
train.X = cbind(displacement, horsepower, weight, cylinders)[train,]
test.X = cbind(displacement, horsepower, weight, cylinders)[test,]
train.mpg01 = mpg01[train]

## k = 1
set.seed(1)
knn.pred = knn(train.X, test.X, train.mpg01, k=1)
table(knn.pred, Auto.test$mpg01)
mean(knn.pred != Auto.test$mpg01)
## 13.26% test error rate

## k = 3
set.seed(1)
knn.pred = knn(train.X, test.X, train.mpg01, k=3)
table(knn.pred, Auto.test$mpg01)
mean(knn.pred != Auto.test$mpg01)
## 11.73% test error rate

## k = 7
set.seed(1)
knn.pred = knn(train.X, test.X, train.mpg01, k=7)
table(knn.pred, Auto.test$mpg01)
mean(knn.pred != Auto.test$mpg01)
## 10.7% test error rate

## k = 7
set.seed(1)
knn.pred = knn(train.X, test.X, train.mpg01, k=50)
table(knn.pred, Auto.test$mpg01)
mean(knn.pred != Auto.test$mpg01)
## 11.73% test error rate

## k = 100
set.seed(1)
knn.pred = knn(train.X, test.X, train.mpg01, k=100)
table(knn.pred, Auto.test$mpg01)
mean(knn.pred != Auto.test$mpg01)
## 10.7% test error rate

## k = 185
set.seed(1)
knn.pred = knn(train.X, test.X, train.mpg01, k=185)
table(knn.pred, Auto.test$mpg01)
mean(knn.pred != Auto.test$mpg01)
## 13.26% test error rate

## k = 186
set.seed(1)
knn.pred = knn(train.X, test.X, train.mpg01, k=186)
table(knn.pred, Auto.test$mpg01)
mean(knn.pred != Auto.test$mpg01)
## 32.1% test error rate

## k = 190
set.seed(1)
knn.pred = knn(train.X, test.X, train.mpg01, k=190)
table(knn.pred, Auto.test$mpg01)
mean(knn.pred != Auto.test$mpg01)
## 52.6% test error rate

## values of k up to 100 perform the same, but then it drops off after k = 185 

 