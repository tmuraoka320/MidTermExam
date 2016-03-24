#' Tolerance Test with class "Tol"
#'
#' Conduct a tolerance test with S4 class "Tol"
#'
#'
#' @param fun A function
#' @param tolerance A level of tolerance
#' @param a A starting value
#' @param b An ending value
#' @param Trap A rule argument. If TRUE, the Trapezoidal rule is applied. If FALSE, the Simpson rule is applied. Defalt is TRUE.
#' @param start An argument for the number of intervals it should start with. Defalt is 2
#' @param correct The correct answer for the integral
#'
#'
#' @return An object of class "Tol" containing
#'  \item{fun}{A function}
#'  \item{tolerance}{A level of tolerance}
#'  \item{a}{A starting value}
#'  \item{b}{An ending value}
#'  \item{rule}{A rule argument, Whether the Trapezoidal rule or Simpson rule is used}
#'  \item{start}{An argument for the number of intervals it should start with}
#'  \item{correct}{The correct answer for the integral}
#'  \item{final_n}{The final number of intervals}
#'  \item{abs_error}{The absolute error of the estimate}
#'
#' @author Taishi Muraoka \email{tmuraoka@@wustl.edu}
#'
#' @examples
#' myfun <- function(x){
#'   return(sqrt(sin(x)^3 + 1))
#' }
#' mycorrect <- as.numeric(integrate(myfun, 0, 10)$value)
#' tolTest(myfun, 0.05, 0, 10, Trap=TRUE, 2, mycorrect)
#' tolTest(myfun, 0.05, 0, 10, Trap=FALSE, 2, mycorrect)
#'
#' @aliases tolTest,ANY-method
#'
#' @rdname tolTest
#'
#' @export
setGeneric(name="tolTest",
           def=function(fun, tolerance, a, b, Trap=TRUE, start, correct, ...){
             standardGeneric("tolTest")
           })

#' @export
setValidity("Tol", function(object){
  if(is.function(object@fun)==FALSE){ # check if fun is function
    print("fun should be a function!")
  }
  if(is.numeric(object@tolerance)==FALSE){ # check if tolerance is numeric
    print("tolerance should be numeric!")
  }
  if(is.numeric(object@a)==FALSE){ # check if a is numeric
    print("a should be numeric!")
  }
  if(is.numeric(object@b)==FALSE){ # check if b is numeric
    print("b should be numeric!")
  }
  if(object@rule != "Trapezoidal" & object@rule != "Simpson"){ # check if rule is either "Trapezoidal" or "Simpson"
    print("rule should be either 'Trapezoidal' or 'Simpson'!")
  }
  if(is.numeric(object@start)==FALSE){ # check if start is numeric
    print("start should be numeric!")
  }
  if(is.numeric(object@correct)==FALSE){ # check if correct is numeric
    print("correct should be numeric!")
  }
  if(is.numeric(object@final_n)==FALSE){ # check if final_n is numeric
    print("final_n should be numeric!")
  }
  if(object@abs_error >= object@tolerance | object@abs_error < 0){ # check if abs_error is greater than 0 and smaller than tolerance
    print("abs_error should be greater that 0 and smaller than the level of tolerance!")
  }

})

#' @export
setMethod("tolTest",
          definition=function(fun, tolerance, a, b, Trap=TRUE, start=2, correct, ...){
            # check if fun is function
            if(is.function(fun)==FALSE){
              stop("fun should be a function!")
            }
            # check if tolerance is numeric
            if(is.numeric(tolerance)==FALSE){
              stop("tolerance should be numeric!")
            }
            # check if a is numeric
            if(is.numeric(a)==FALSE){
              stop("a should be numeric!")
            }
            # check if b is numeric
            if(is.numeric(b)==FALSE){
              stop("b should be numeric!")
            }
            # check if correct is numeric
            if(is.numeric(correct)==FALSE){
              stop("correct should be numeric!")
            }
            # check if a is smaller than b
            if(a >= b){
              stop("a should be smaller than b!")
            }
            done <- FALSE
            final_n <- start # just change the name for convenience
            while(!done){ # keep looping until abs_error gets smaller than tolerance
              tempx <- seq(a, b, by=(b - a)/final_n) # based on a, b, and the number of intervals (start), create a vector x
              tempy <- sapply(tempx, fun) # use the function and create a vector of y
              if(Trap==FALSE & length(tempx)%%2 != 1){ # if the Simpson rule is required and the length of x is even, move to the next number of interval
                final_n <- final_n*2 # double the number of intervals for the next loop
              }
              else{ # if the Trapezoida rule is required, or if the Simpson rule is required and the length of x is odd, calculate abs_error
                temp_out <- integrateIt(tempx, tempy, a, b, Trap) # apply intergrateIt function
                abs_error <- abs(temp_out@result - correct) # get abs_error
                if(abs_error > tolerance){ # if abs_error is greater than tolerance
                  final_n <- final_n*2 # double the number of intervals for the next loop
                }
                else{ # if abs_error is successfully smaller than tolerance
                  done <- TRUE # end the loop
                }
              }
            }
            rule <- ifelse(Trap==TRUE, "Trapezoidal", "Simpson") # create "rule"
            object <- new("Tol", # create a new "Tol" object
                          fun=fun,
                          tolerance=tolerance,
                          a=a,
                          b=b,
                          rule=rule,
                          start=start,
                          correct=correct,
                          final_n=final_n,
                          abs_error=abs_error)
            return(object)
          })
