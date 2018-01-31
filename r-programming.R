## Lesson 5: R programming
## ==

## Objectives:
## 1.  Writing your own functions
## 2.  Control structures (if, for, etc)

#######################
## Writing Functions ##
#######################

## So now you have a very quick introduction to R, and you know how to find and install
## packages. Sooner or later, you'll need to do something that isn't in an existing
## package. Maybe it will just be automating something that you do a lot with your own
## data, or it could be developing a new statistical technique. Either way, you'll want to
## use R's programming features.

## You've already used functions written by others. Writing your own function is really no
## harder. You define a function with the a function called 'function', and you assign it
## to an object so you can call it.

myfun <- function() {
  ## This function doesn't do anything!
}

myfun()

## A few things to notice. First, 'function' is followed by parentheses. These contain the
## function arguments. In this case, there are none, so the function can't take any input.
## After the parentheses are braces. Everything inside the braces is part of the function.
## In this case, the only thing inside the braces is a comment, which does nothing.

## When you call a function, even if it doesn't have any arguments, you need to include
## the parentheses. Otherwise, instead of calling the function, R will display its
## contents instead:

myfun

## A very common source of confusion for R beginners is to forget the parentheses after a
## function, resulting in a torrent of R code being spit out on the screen.

## In most cases, in order for a function to be useful, it needs to return a value. By
## default, the function will return the value of the last expression in the function
## body. In myfun, the last expression is a comment, which has no value, so myfun returns
## NULL. We can make it marginally more useful by adding a value:

myfun <- function(){
  ## This function always returns 42
  42
}

myfun()

## functions will return the value of the last statement in their definition. To be
## clearer, you should use the return() function to indicate what the output of the
## function will be:

myfun <- function(){
  ## This function always returns 42
  return(42)
}


## Slightly more useful now. But most functions need to have arguments:

myfun <- function(x){
  ## This function returns its argument
  return(x)
}

myfun(20)

myfun(30)

## Note that the argument you use in 'function' produces a local variable, accessible only
## from inside the function body. If an object with the same name occurs outside the
## function, it is 'masked' by the function argument:

x = 100
x
myfun(6)
x

## The x inside myfun is completely separate from the x outside the function. However, if
## you refer to an object in your function that isn't an argument, R will look for it in
## the global environment instead.

myfun <- function(x) {
  ## This funciton adds y to its argument
  return(x + y)
}

y = 10
myfun(5)
myfun(20)

y = 100
myfun(5)
myfun(20)

## Note that it is usually a bad idea to refer to variables in the global environment from
## inside your function body. This is an easy way to create hard-to-find bugs.

## As currently defined, myfun requires an argument:

myfun()

## We can assign x a default value with the equal sign:

myfun <- function(x = 42){
  ## Returns the argument, which defaults to 42
  return(x)
}

myfun(5)
myfun(-1000)
myfun("hello")
myfun()

## This is one case where '=' and '<-' are not the same. You should always use '=' in
## function definitions.

## Functions can include a combination of required and optional arguments:

myfun <- function(x, y = 17, message = "hi there"){
  print(message)
  return(x + y)
}

myfun(20)

## If you want to return multiple values, you need to combine them together into a list:

myfun <- function(x, y = 17, message = "hi there"){
  return(list(mysum = x + y, mymessage = message))
}

myfun(20)

###############
## Exercises ##
###############

## Write a function that returns the mean of two values

## Modify the function such that the second value defaults to 10

## What happens if you pass two vectors of equal length to this function, instead of two
## single numbers?

## Write a function that takes three arguments, calculates the quadratic formula, and
## returns the two solutions as a list (the quadratic equation is -b +/- sqrt(b^2 -4ac)/2a)


########################
## if else statements ##
########################

## Clearly, you can use functions to save a lot of typing when you're making complex
## calculations. You can also use them to filter and manipulate data. The 'if' statement
## is very useful for this:

if(1 + 1 == 2) {
  print("Everything's peachy") 
}

## The if statement starts with the word 'if', followed by a test in parentheses. If the
## test is TRUE, or a non-zero number, the expression in the braces following the test
## will be evaluated.

## You may also include an 'else' clause. The code inside the braces following the word
## 'else' gets evaluated only when the test is FALSE or zero:

if(1 + 1 == 3){
  print("test was true")
} else {
  print("test was false")
}

###############
## For loops ##
###############

## For loops in R work much like they do in bash, with slightly different syntax:

for(i in c("cat", "dog", "fish")) {
  print(i)
}

for(i in 1:10){
  print(i^2)
}

## You can nest loops too:

for(i in 1:10) {
  for(j in letters[1:5]){
    print(paste(i, j))
  }
}

## A common use of for loops is to 'walk' across a vector or matrix. There are often
## alternatives that produce faster code, but for is usually easier to figure out.

set.seed(1)                                 # so we all get the same matrix
mymat <- matrix(sample(1:100, 20), nrow = 5, ncol = 4)

mymat

for(i in 1:5) {
  for(j in 1:4){
    print(mymat[i, j])
  }
}

## This is an example for illustration only. There is an easier way to
## pull out each element in a matrix:

as.numeric(mymat)

## Compared to other languages, R has a reputation for being slow when
## it comes to running for loops. If you can avoid them by using
## vectorized operations, your code will be much faster. There are also
## a number of functions available that give you cleaner ways to loop
## over lists and matrices.

## Some examples
## column sums

## with a for loop:
res <- numeric(ncol(mymat))
for (i in 1:ncol(mymat)){
  res[i] <- sum(mymat[,i])
}
res

## with apply:
apply(mymat, MARGIN = 2, FUN = sum)

## with the builtin colSums:
colSums(mymat)

## normalizing data

## with a for loop:

norm.mat <- mymat
for (i in 1:ncol(mymat)){
  tmp.mean <- mean(mymat[, i])
  tmp.sd <- sd(mymat[, i])
  norm.mat[, i] <- (mymat[,i] - tmp.mean)/tmp.sd
}

rm(tmp.mean, tmp.sd)

norm.mat

## with apply:
apply(mymat, MARGIN = 2,
      FUN = function (x) {
        (x - mean(x)) / sd(x)
      }
      )

## with scale:
scale(mymat)

## Key point:
## 1. if there is a builtin to do what you need, use it!
##      scale, colSums, sweep, rowSums
## 2. if you can get apply to do what you need, use it!
## 3. if you have to do something too complicated for apply, then make a
##      for loop. 

      
###############
## Exercises ##
###############

## 1
## Write a function that takes two arguments, and returns a list containing the mean of
## the two arguments, the smaller number, and the larger number. Hint: use an if
## statement with < in the test.

## 2
## Write a function that takes two arguments - a vector of numbers and a minimum value.
## The function returns the vector with all the values that are less than the minimum
## value removed.

## 2b
## Modify 2 so that the minimum value has a default value of 10

## 3
## Write a function that calculates the geometric mean of a vector of numbers. Hint: you
## calculate the geometric mean by multiplying the N numbers together, then taking the Nth
## root of the product. Taking the Nth root is the same as raising to the exponent 1/N.
## The functions prod() and length() will be helpful.

## 4
## Write a function that takes a vector as an argument, and returns the mean of the lowest
## and highest values in the vector.

## 5
## Write a function that subtracts the lowest value in a matrix from every value in the
## matrix.

