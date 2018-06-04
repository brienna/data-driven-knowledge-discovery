## Chapter 2 lab
## Brienna Herold
## Summer 2018


########## BASIC COMMANDS ##########

## Join together the numbers 1, 3, 2, 5 and save them as a vector named x

x <- c(1,3,2,5)
x

## We can also save things using = rather than <-

x = c(1,6,2)
x
y = c(1,4,3)

# Add two vectors together (they need to be of the same length)

length(x)
length(y)
x + y

## List all objects that we have saved so far

ls()

## Delete x and y

rm(x, y)
ls()

## Remove all objects at once

x = c(1,6,2)
y = c(1,4,3)
ls()
rm(list=ls())
ls()

## Learn more about a function & the arguments it takes

?matrix

## Create a simple matrix
## We could also omit typing the argument names cuz they are ordered
## But it is useful to specify the names of the arguments 

x = matrix(data=c(1,2,3,4), nrow=2, ncol=2)
x

## Create the same matrix, specifying that it be populated in order of the rows

matrix(data=c(1,2,3,4), nrow=2, ncol=2, byrow=TRUE)

## Return the square root of each element of the matrix
options(digits=3)
sqrt(x)

## Raise each element of the matrix to power of 2
x^2

## Generate a vector of random normal variables, with first argument n the sample size. Each time we call this function, we will get a different answer.

x <- rnorm(50)
x

## Get the type and class of the elements in x
typeof(x)
class(x)

## Compute the correlation between two sets of numbers

x = rnorm(50)
y = x + rnorm(50, mean=50, sd=.1) 
x
y
cor(x,y)

## This is a great relationship, very linear. The correlation coefficient of x and y is almost 1, where 1 is the most correlated. We can also see this relationship graphically.

plot(x, y)

## By default rnorm() creates standard normal random variables with a mean of 0 and a standard deviation of 1. We can alter the mean & sd, as shown with y above. Sometimes we also want our code to reproduce the exact same set of random numbers. We use set.seed() to do this. It takes an arbitrary integer argument.

set.seed(1303) # needs to be called each time 
rnorm(50)
set.seed(1303)
rnorm(50)
rnorm(50) # we didn't call set.seed, so this generates a different set of random numbers

## Compute the mean, variance, and sd of a vector of numbers

y = rnorm(100)
y
mean(y)
var(y)
sqrt(var(y)) # standard deviation
sd(y) # simpler way to get standard deviation


########## GRAPHICS ##########

plot() is primary way to plot data in R, produces scatterplot

x = rnorm(100)
y = rnorm(100)
plot(x,y) 

## Other things we can do with plot()

?plot
pdf("Figure.pdf") # creates a pdf and begins sending data to it
plot(x, y, xlab="this is the x-axis", ylab="this is the y-axis", main="Plot of X vs Y",  col="green")
dev.off() # stop sending data to pdf
## We can create a jpeg with jpeg()

## Create a sequence of numbers

x = seq(1,10)
x
x = 1:10 # another way
x 
x = seq(-pi, pi, length=50)
x

## Create more sophisticated plots
## The contour() function produces a contour plot to represent 3D data; it is like a topographical map. The 3rd argument is a matrix whose elements correspond to the z value (the third dimension) for each pair of (x,y) coordinates

y=x
f=outer(x, y, function(x,y)cos(y)/(1+x^2))
contour(x,y,f) 
contour(x,y,f,nlevels=45,add=T)
fa = (f-t(f)/2) # f minus the transpose of f divided by 2
contour(x,y,fa,nlevels=15)

## image() works the same way as contour(), except it produces a color-coded plot whose colors depend on the z value. Heatmap. 

image(x,y,fa)

## persp() can be used to produce a 3D plot. The arguments theta and phi control the angles at which the plot is viewed

persp(x,y,fa)
persp(x,y,fa,theta=30)
persp(x,y,fa,theta=30,phi=20)
persp(x,y,fa,theta=30,phi=70)
persp(x,y,fa,theta=30,phi=40)


########## INDEXING DATA ##########

A = matrix(1:16,4,4)
A

## Get element in 2nd row & 3rd column
A[2,3]

## Select multiple rows & columns at a time by providing vectors as the indices
A[c(1,3), c(2,4)]
A[1:3, 2:4] # don't need to use concatenate if we have continuous indices
A[,1:2] # only columns 1, 2
A[1:2,] # only rows 1, 2
A[-c(1,3),] # negative sign means keep all rows except those indicated
A[-c(1,3), -c(1,3,4)]

## Output num of rows and num of columns (dimensions)
dim(A)


########## LOADING DATA ##########

## Change working directory on OS X by going to Misc > Change Working Directory, or
getwd()
setwd("/Users/Brienna/Documents/Coursework/Master's Program/Summer 2018/ISTE 780/data-driven knowledge discovery/2.3 lab - practice in R")

## Import data set into R (can export with write.table())

Auto = read.table("Auto.data") # Auto is formatted as a data frame
fix(Auto) # View in a spreadsheet like window

## This particular dataset hasn't been loaded correctly, because R has assumed that the variable names are part of the data and so has included them in the first row. Also there are missing observations, indicated by "?" So include headers, and 

Auto = read.table("Auto.data", header=T, na.strings="?")
## header=T tells R that the first line of the file contains the variable names
## na.strings tells R that anytime it sees a particular character or set of characters (such as "?"), it should be treated as a missing element of the data matrix
fix(Auto)

## Import Excel format
Auto = read.csv("Auto.csv", header=T, na.strings="?")
fix(Auto)
dim(Auto) # 397 observations, or rows, and 9 variables, or columns
Auto[1:4,]

## Remove rows containing missing observations
Auto = na.omit(Auto)
dim(Auto)

## Once the data are loaded correctly, we can check variable names
names(Auto)


########## ADDITIONAL GRAPHICAL AND NUMERICAL SUMMARIES ##########

plot(cylinders, mpg) # produces an error, we need to save those variables, type it as Auto$cylinders, or use attach() to tell R to make the variables in the data frame available by name

plot(Auto$cylinders, Auto$mpg)
attach(Auto)
plot(cylinders, mpg)

## The cylinders variable is stored as a numeric vector, so R has treated it as quantitative. However, since there are only a small number of possible values for cylinders, you might prefer to treat it as a qualitative variable. Use as.factor() to convert quantitative variables into qualitative variables.

cylinders = as.factor(cylinders)

## If the variable plotted on the z-axis is categorical, then boxplots will automatically be produced by plot().

plot(cylinders, mpg)
plot(cylinders, mpg, col="red")
plot(cylinders, mpg, col="red", varwidth=T)
plot(cylinders, mpg, col="red", varwidth=T, horizontal=T)
plot(cylinders, mpg, col="red", varwidth=T, lab="cylinders", ylab="MPG")

## Plot a histogram. Note: col=2 is same as col="red"
hist(mpg)
hist(mpg, col=2)
hist(mpg, col=2, breaks=15)

## Create a scatterplot matrix (a scatterplot for every pair of variables for any given data set)
pairs(Auto)

## Produce scatterplots for just a subset of the variables
pairs(~ mpg + displacement + horsepower + weight + acceleration, Auto)

## Make it so we can identify the value for a particular variable for points on a plot. Pass 3 args to identify(): x-axis variable, y-axis variable, and the variable whose values we would like to see printed for each point.
plot(horsepower, mpg)
identify(horsepower,mpg,name)

## Show numerical summary of each variable
## For qualitative variables such as name, R will list the number of observations that fall in each category
summary(Auto)

## Show numerical summary of a single variable
summary(mpg)

