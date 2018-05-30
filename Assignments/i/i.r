## Exercise 2.4.8
## Brienna Herold
## Summer 2018

# 8.    This exercise relates to the College data set, which can be
#       found in the file College.csv. It contains a number of 
#       variables for 777 different universities and colleges in the 
#       US. The variables are
# • Private : Public/private indicator
# • Apps : Number of applications received
# • Accept : Number of applicants accepted
# • Enroll : Number of new students enrolled
# • Top10perc : New students from top 10 % of high school class 
# • Top25perc : New students from top 25 % of high school class 
# • F.Undergrad : Number of full-time undergraduates
# • P.Undergrad : Number of part-time undergraduates
# • Outstate : Out-of-state tuition
# • Room.Board : Room and board costs
# • Books : Estimated book costs
# • Personal : Estimated personal spending
# • PhD : Percent of faculty with Ph.D.’s
# • Terminal : Percent of faculty with terminal degree
# • S.F.Ratio : Student/faculty ratio
# • perc.alumni : Percent of alumni who donate
# • Expend : Instructional expenditure per student
# • Grad.Rate : Graduation rate

# Before reading the data into R, it can be viewed in Excel or a text editor.

# (a) 
# Use the read.csv() function to read the data into R. Call the loaded
# data college. Make sure that you have the directory set to the correct 
# location for the data.

setwd("/Users/Brienna/Documents/Coursework/Master's Program/Summer 2018/ISTE 780/Submissions/i")
college <- read.csv("College.csv") 

# (b)
# Look at the data using the fix() function. 

fix(college)

# You should notice that the 
# first column is just the name of each university. We don’t really want R 
# to treat this as data. However, it may be handy to have these names for 
# later. Try the following commands:
# > rownames(college)=college[,1] 
# > fix(college)

rownames(college) = college[,1] 
fix(college)

## In [,1] the trailing comma in 1st pos indicates we are working with only columns. If in 2nd pos, indicates we are working with only rows.

# You should see that there is now a row.names column with the name of each 
# university recorded. This means that R has given each row a name 
# corresponding to the appropriate university. R will not try to perform
# calculations on the row names. However, we still need to eliminate the first 
# column in the data where the names are stored. Try
# > college=college[,-1] 
# > fix(college)

college = college[,-1] 
fix(college)

# Now you should see that the first data column is Private. Note that another
# column labeled row.names now appears before the Private column. However,
# this is not a data column but rather the name that R is giving to each row.

# (c) 
# i. Use the summary() function to produce a numerical summary of the
# variables in the data set.

options(width=100)
summary(college)

# ii. Use the pairs() function to produce a scatterplot matrix of the first 
# ten columns or variables of the data. Recall that you can reference the
# first ten columns of a matrix A using A[,1:10].

pairs(college[,1:10])

# iii. Use the plot() function to produce side-by-side boxplots of Outstate
# versus Private.

boxplot(college[,"Outstate"]~college[,"Private"], main="Outstate versus Private", xlab="Private", ylab="Outstate", col=c("red", "blue"))

## https://www.rdocumentation.org/packages/graphics/versions/3.5.0/topics/boxplot

# iv. Create a new qualitative variable, called Elite, by binning the
# Top10perc variable. We are going to divide universities into two groups
# based on whether or not the proportion of students coming from the top 10%
# of their high school classes exceeds 50 %.
# > Elite=rep("No",nrow(college))
# > Elite[college$Top10perc >50]="Yes"
# > Elite=as.factor(Elite)
# > college=data.frame(college ,Elite)

Elite = rep("No", nrow(college))
Elite[college$Top10perc >50] = "Yes"
Elite = as.factor(Elite)
college = data.frame(college, Elite)

# Use the summary() function to see how many elite universities there are.

summary(college[,"Elite"]) 

## There are 78 elite universities.

# Now use the plot() function to produce side-by-side boxplots of Outstate
# versus Elite.

boxplot(college[,"Outstate"]~college[,"Elite"], main="Outstate versus Elite", xlab="Elite", ylab="Outstate", col=c("red", "blue"))

# v. Use the hist() function to produce some histograms with differing numbers
# of bins for a few of the quantitative variables. You may find the command
# par(mfrow=c(2,2)) useful: it will divide the print window into four regions
# so that four plots can be made simultaneously. Modifying the arguments to
# this function will divide the screen in other ways.

par(mfrow=c(2,3)) ## do not close while executing following commands
hist(college[,"Accept"], breaks=4, main="Histogram with 4 bins", ylab="Value", xlab="Accept", col="red")
hist(college[,"Enroll"], breaks=4, main="Histogram with 4 bins", ylab="Value", xlab="Enroll", col="red")
hist(college[,"Top10perc"], breaks=4, main="Histogram with 4 bins", ylab="Value", xlab="Top10perc", col="red")
hist(college[,"Accept"], breaks=10, main="Histogram with 10 bins", ylab="Value", xlab="Accept", col="blue")
hist(college[,"Enroll"], breaks=10, main="Histogram with 10 bins", ylab="Value", xlab="Enroll", col="blue")
hist(college[,"Top10perc"], breaks=10, main="Histogram with 10 bins", ylab="Value", xlab="Top10perc", col="blue")

## differing numbers of bins are done with breaks
## https://www.datamentor.io/r-programming/histogram
## https://www.datamentor.io/r-programming/subplot

# vi. Continue exploring the data, and provide a brief summary of what you
# discover.

## Get dimensions (num of rows & num of columns)
dim(college) 
nrow(college)
ncol(college)
## Get first few observations & last few observations
head(college, n=10)
tail(college, n=10)
## Get column headers
names(college)
## Get more info, e.g. types of data for each column
str(college)
## Show all categories or levels of a categorical variable
levels(college[,"Elite"])


