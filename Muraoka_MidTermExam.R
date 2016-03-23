###
### Mid-Term Exam
### Taishi Muraoka
### March 24
###



##
## setup
##
library(devtools);library(roxygen2)

setwd("C:/Users/Taishi/Documents/zzzzzzzzzzzzz")

current.code <- as.package("MTpack")

load_all(current.code)

document(current.code)
