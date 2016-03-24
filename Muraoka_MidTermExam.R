###
### Mid-Term Exam
### Taishi Muraoka
### March 24
###



##
## setup
##
library(devtools);library(roxygen2)

setwd("C:/Users/Taishi/Documents/MidTermExam")

current.code <- as.package("integrateIt")

load_all(current.code)

document(current.code)



##
## test integrateIt()
##
help(integrateIt)

# class "Trap"
myx <- seq(0, 20, by=0.2) # range from 0 to 20

myy <- sqrt(sin(myx)^3 + 1)

mya <- 0 # just want x between 0 to 7

myb <- 7

(trap_test <- integrateIt(myx, myy, mya, myb, Trap=TRUE)) # this works

integrateIt(myx, 1:9, mya, myb, Trap=TRUE) # error because the lengths of x and y differ

integrateIt(myx, myy, 1:3, myb, Trap=TRUE) # error because a is not a single number

integrateIt(myx, myy, "a", myb, Trap=TRUE) # error because a is not numeric

integrateIt(myx, myy, 4, 2, Trap=TRUE) # error because a is greater than b

integrateIt(myx, myy, mya, 200, Trap=TRUE) # error because b is greater than max(x)

print(trap_test) # print class "Trap"

plot(trap_test, "ANY") # plot class "Trap"

# class "Simpson"
myx <- seq(0, 20, by=0.25) # range from 0 to 20

myy <- 1/(myx^5+3)^(1/3)

mya <- 0 # just want x between 0 to 7

myb <- 7

(simpson_test <- integrateIt(myx, myy, mya, myb, Trap=FALSE)) # the works

print(integrateIt(myx, myy, mya, myb, Trap=FALSE)) # print class "Simpson"

plot(simpson_test, "ANY") # plot class "Simpson



##
## test tolTest()
##
help(tolTest)

myfun <- function(x){
  return(sqrt(sin(x)^3 + 1))
}

plot(0:50, myfun(0:50), type="l", # the function behaves like this
     xlab="x", ylab="y")

mytolerance <- 0.05

mya <- 0

myb <- 10

mystart <- 2

mycorrect <- as.numeric(integrate(myfun, 0, 10)$value)

# Trapezoidal rule
tolTest(myfun, mytolerance, mya, myb, Trap=TRUE, mystart, mycorrect)

# Simpson rule
tolTest(myfun, mytolerance, mya, myb, Trap=FALSE, mystart, mycorrect)
