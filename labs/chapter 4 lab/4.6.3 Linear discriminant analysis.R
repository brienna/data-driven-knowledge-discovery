
## Chapter 4 lab - Linear discriminant analysis

library(MASS)
library(ISLR)
options(width=90)
options(digits=4)
names(Smarket) # see headings
dim(Smarket) # see dimensions
summary(Smarket)
attach(Smarket)

## create a vector corresponding to the observations from 2001 through 2004 
train = (Year < 2005) 
summary(train)
Smarket.2005 = Smarket[!train,] # not train 
dim(Smarket.2005)
Direction.2005=Direction[!train]

## Fit the LDA model using only the observations before 2005 
lda.fit = lda(Direction ~ Lag1 + Lag2, subset=train)
## Notice that the syntax for the lda() function is identical to that of lm(), and to that of glm() except for the absence of the family option 
lda.fit
## The LDA output indicates that π1 = 0.492 and π2 = 0.508; in other words, 49.2% of the training observations correspond to days during which the market went down. 
## It also provides the group means; these are the average of each predictor within each class, and are used by LDA as estimates of μk. These suggest that there is a tendency for the previous 2 days’ returns to be negative on days when the market increases, and a tendency for the previous days’ returns to be positive on days when the market declines. 
## The coefficients of linear discriminants output provides the linear combination of Lag1 and Lag2 that are used to form the LDA decision rule. In other words, these are the multipliers of the elements of X = x in (4.19). If −0.642 × Lag1 − 0.514 × Lag2 is large, then the LDA classifier will predict a market increase, and if it is small, then the LDA classifier will predict a market decline. 
plot(lda.fit)
## The plot() function produces plots of the linear discriminants, obtained by computing −0.642 × Lag1 − 0.514 × Lag2 for each of the training observations. 

lda.pred = predict(lda.fit, Smarket.2005)
names(lda.pred)
## The predict() function returns a list with three elements. The first element, class, contains LDA’s predictions about the movement of the market. The second element, posterior, is a matrix whose kth column contains the posterior probability that the corresponding observation belongs to the kth class, computed from (4.10). Finally, x contains the linear discriminants, described earlier. 
lda.class = lda.pred$class
table(lda.class, Direction.2005)
mean(lda.class == Direction.2005)

## Applying a 50 % threshold to the posterior probabilities allows us to recreate the predictions contained in lda.pred$class. 
sum(lda.pred$posterior[,1] >= 0.5)
sum(lda.pred$posterior[,1] <= 0.5)
## The two above should output 70 and 182 which sums to 252, the amount of observations in Smarket.2005

## Notice that the posterior probability output by the model corresponds to the probability that the market will decrease: 
lda.pred$posterior[1:20,1]
lda.class[1:20]

## If we wanted to use a posterior probability threshold other than 50% in order to make predictions, then we could easily do so. For instance, suppose that we wish to predict a market decrease only if we are very certain that the market will indeed decrease on that day—say, if the posterior probability is at least 90 %. 
sum(lda.pred$posterior[,1] > 0.9)
## No days in 2005 meet that threshold! In fact, the greatest posterior prob- ability of decrease in all of 2005 was 52.02 %. 









