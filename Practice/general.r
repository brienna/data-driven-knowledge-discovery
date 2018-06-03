# To execute a line, go to the line and push command + return
# In the console, the numbers in the brackets [1] refer to the index number of the item right next to it. 

# Customizes size of console screen, re-execute other lines to see
options(width=60)

# Prints out a sequence of numbers 
1:50 
50:1
 
# Vector notation, using the concatenate function
c(0,1,1,2,3,5,8)
# R assumes that your vector is either a column vector or a row vector depending on what makes more sense. You can multiply a row vector and a column vector that are of the same length, but you cannot multiply a row vector by itself. However, R is more lenient on this than the rules of linear algebra are; R will assume that you meant to multiply a row vector and a column vector. 

# Perform operations on vectors (Again, R assumes that we are operating on a row and a column vector)
c(1,2,3,4) + c(10,20,30,40)
c(1,2,3,4) * c(10,20,30,40)
c(1,2,3,4) - c(1,1,1,1)
c(1,2,3,4) + 1
1/c(1,2,3,4,5) 

# This operation has too many decimals
1/c(1,2,3,4,5) 
# To reduce precision, set options then re-execute the above command
options(digits=3)

# Interesting property: we can add a short vector to a longer vector
# This is illegal in linear algebra.
# R extends the smaller vector to match the longer vector
# So here it adds 1 + 10, 2 + 100, then it reuses the vector 
# and adds 1 + 30, 4 + 100
c(1,2,3,4) + c(10,100)
# But this won't work 
c(1,2,3,4,5) + c(10,100)

# Character vector
"Hello, World."
c("Hello, World", "Hello, R interpreter")
c("Hello, World", "Hello, R interpreter", "Hello, World", "Hello, R interpreter")

# FUNCTIONS
# Everything seems to be a function in R
exp(1) # 2.72, or e
exp(0) # 1
e <- exp(1) # set e as exp(1), assignment operator can use either this or equal
e # prints out 2.72
2^10
cos(3.141593)
log2(1)
log(x=64, base=4)
3==3

# VARIABLES
x <- 1
y <- 2
z <- c(x,y)
z
y <- 4
z # stays the same even though y was changed, only remembers the value at time of assignment
b <- c(1,2,3,4,5,6,7,8,9,10,11,12)
b
b <- c(1:12)
b
b[7]
b[1:6]
b[b%%3==0]
b%%3==0
b[b>10] # where the current index position of b is greater than 10
b[c(1,6,11)] # choose individual indexes in any order

one <- 1
two <- 2
one = two
one
two
one==two

# Different way to assign
3 -> three
three

f <- function(x,y) {c(x+1, y+1)} #the object that'll be returned is going to be a vector x y with 1 added to each
f(1,2) # returns a vector containing 2 3
f

# R is an object oriented language but in a different way than Java


