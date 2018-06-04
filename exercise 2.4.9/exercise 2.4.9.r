## Exercise 2.4.9
## Brienna Herold
## Summer 2018

# 9. This exercise involves the Auto data set studied in the lab. Make sure that the missing values have been removed from the data. 

## Obtain data set from http://www-bcf.usc.edu/~gareth/ISL/data.html
## For any info in file that's not available, put "?" then omit them
Auto <- read.csv("/Users/Brienna/Documents/Coursework/Master's Program/Summer 2018/ISTE 780/data-driven knowledge discovery/Practice/exercise 2.4.9/Auto.csv", header=T, na.strings="?")
Auto <- na.omit(Auto)
## Check num of rows & columns
nrow(Auto) 
ncol(Auto)
## Check column names
options(width=50)
colnames(Auto)
## Make it so we can access columns with column names
attach(Auto)
## Save cylinders column as a variable
cylinders <- as.factor(cylinders)

# (a)  Which of the predictors are quantitative, and which are qualitative? 

summary(Auto)
## Qualitative: origin, name, cylinders
## Quantitative: mpg, displacement, horsepower, weight, acceleration, year

# (b)  What is the range of each quantitative predictor? You can answer this using the range() function. 

range(mpg)
range(displacement)
range(horsepower)
range(weight)
range(acceleration)
range(year)

## a different way

sapply(Auto, range)## We converted clyinders to factors, so we need to get rid of it
sapply(Auto[,c(1,3:7)], range)

# (c) What is the mean and standard deviation of each quantitative predictor? 

sapply(Auto[,c(1,3:7)], mean)
sapply(Auto[,c(1,3:7)], sd)
## Formatted nicely
as.data.frame(t(sapply(Auto[,c(1,3:7)], function(bla) list(means=mean(bla), sds=sd(bla), ranges=range(bla)))))

# (d) Now remove the 10th through 85th observations. What is the range, mean, and standard deviation of each predictor in the subset of the data that remains?
 
## ranges are now narrower (if you take a subset, the range should always be either the same or narrower)
sapply(Auto[-10:-85,c(1,3:7)], range)
sapply(Auto[-10:-85,c(1,3:7)], mean)
sapply(Auto[-10:-85,c(1,3:7)], sd)
## Formatted nicely
as.data.frame(t(sapply(Auto[-10:-85,c(1,3:7)], function(bla) list(means=mean(bla), sds=sd(bla), ranges=range(bla)))))
## Note in [-10:-85, c(1,3:7)] did not need to separate -10:85 with a c() cuz the values are continuous, whereas with the columns, they are comma-separate

# (e)  Using the full data set, investigate the predictors graphically, using scatterplots or other tools of your choice. Create some plots highlighting the relationships among the predictors. Comment on your findings. 

## Scatter plot matrix (prof's fav data exploration to start w)
## Can see clearly that some are categorical (lines of dots)
pairs(Auto)
plot(displacement, horsepower)

## The next strongest linear relationship seems to be between displacement and weight, horsepower and weight. There is a strong categorical relationship between displacement and cylinders. There is a somewhat strong categorical relationship between displacement and origin. There are strong curvilinear relationships between mpg and displacement, mpg and horsepower, and mpg and weight. The other relationships are less clear. 

# (f)  Suppose that we wish to predict gas mileage (mpg) on the basis of the other variables. Do your plots suggest that any of the other variables might be useful in predicting mpg? Justify your answer.  
## Weight appears to be the best predictor of mpg. A scatter plot shows that only two 3500 lb+ cars get better than 20 mpg, while no 40 mpg+ car weighs more than 2500 lbs. Most cars show a relationship between mpg and weight on a smooth curve. Displacement and horsepower also appear to be good predictors, especially for larger values of displacement and horsepower.
