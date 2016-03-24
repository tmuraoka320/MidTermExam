#' A Trap and Simpson Object
#'
#' Objects of class \code{Trap} and \code{Simpson} are created by the \code{integrateIt} function
#'
#'
#' An object of the class 'Trap' or 'Simpson' has the following slots:
#' \itemize{
#'  \item{x}{A vector of x}
#'  \item{y}{A vector of y}
#'  \item{result}{The result calculated applying the Trapezoidal or Simpson rule}
#'  }
#'
#' @author Taishi Muraoka: \email{tmuraoka@@wustl.edu}
#'
#' @aliases Trap-class, Simpson-class, initialize,Trap-Simpsonmethod, show,Trap-Simpson-method, print,Trap-Simpson-method
#'
#' @rdname Trap-Simpson-class
#'
#' @export
setClass(Class="Trap",
         slots=list(x="vector",
                    y="vector",
                    result="numeric"))

#' @export
setMethod("initialize", "Trap",
          function(.Object, ...){
            value=callNextMethod()
            return(value)
          })

#' @export
setClass(Class="Simpson",
         contain="Trap",
         slots=list(x="vector",
                    y="vector",
                    result="numeric"))

#' @export
setMethod("initialize", "Simpson",
          function(.Object, ...){
            value=callNextMethod()
            return(value)
          })

#' @export
setMethod("show", "Trap",
          function(object){
            cat(sprintf("An object of class '%s'", class(object)))
            cat("\nslot 'x':\n")
            print(object@x)
            cat("\nslot 'y':\n")
            print(object@y)
            cat("\nslot 'result':\n")
            print(object@result)
          })

#' @export
print.Trap <- function(Trap_or_Simpson_object){
  cat(sprintf("An object of class '%s'", class(Trap_or_Simpson_object)))
  cat("\nresult:\n")
  print(Trap_or_Simpson_object@result)
}
