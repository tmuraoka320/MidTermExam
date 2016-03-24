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

# class "Simpson"
myx <- seq(0, 20, by=0.25) # range from 0 to 20

myy <- 1/(myx^5+3)^(1/3)

mya <- 0 # just want x between 0 to 7

myb <- 7

(simpson_test <- integrateIt(myx, myy, mya, myb, Trap=FALSE)) # the works
