#' A Tol Object
#'
#' Objects of class \code{Tol} are created by the \code{tolTest} function
#'
#'
#' An object of the class 'Tol' has the following slots:
#' \itemize{
#'  \item{fun}{A function}
#'  \item{tolerance}{A level of tolerance}
#'  \item{a}{A starting value}
#'  \item{b}{An ending value}
#'  \item{rule}{A rule argument, whether the Trapezoidal rule or Simpson rule is used}
#'  \item{start}{An argument for the number of intervals it should start with}
#'  \item{correct}{The correct answer for the integral}
#'  \item{final_n}{The final number of intervals}
#'  \item{abs_error}{The absolute error of the estimate}
#'  }
#'
#' @author Taishi Muraoka: \email{tmuraoka@@wustl.edu}
#'
#' @aliases Tol-class, initialize,Tol-class,
#'
#' @rdname Tol-class
#'
#' @export
setClass(Class="Tol",
         slots=list(fun="function",
                    tolerance="numeric",
                    a="numeric",
                    b="numeric",
                    rule="character",
                    start="numeric",
                    correct="numeric",
                    final_n="numeric",
                    abs_error="numeric"))

#' @export
setMethod("initialize", "Tol",
          function(.Object, ...){
            value=callNextMethod()
            return(value)
          })
