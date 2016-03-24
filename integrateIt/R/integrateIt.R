#' Create an Object with class "Trap" or "Simpson"
#'
#' Create an instance with S4 class "Trap" or "Simpson"
#'
#'
#' @param x A numeric vector x
#' @param y A numeric vector y (= f(x))
#' @param a A starting value
#' @param b An ending value
#' @param Trap A rule argument. If TRUE, the Trapezoidal rule is applied. If FALSE, the Simpson rule is applied. Defalt is TRUE.
#'
#'
#' @return An object of class "Trap" or "Simpson" containing
#'  \item{x}{A vector of x}
#'  \item{y}{A vector of y}
#'  \item{result}{The result calculated by the Trapezoidal or Simpson rule}
#'
#' @author Taishi Muraoka \email{tmuraoka@@wustl.edu}
#'
#' @examples
#' myx <- 0:10
#' myy <- cos(3*myx) + sin(2*sin(myx))
#' mya <- 0
#' myb <- 10
#' integrateIt(myx, myy, mya, myb, Trap=TRUE)
#' integrateIt(myx, myy, mya, myb, Trap=FALSE)
#'
#' @aliases integrateIt,ANY-method
#'
#' @rdname integrateIt
#'
#' @export
setGeneric(name="integrateIt",
           def=function(x, y, a, b, Trap=TRUE, ...){
             standardGeneric("integrateIt")
           })

#' @export
setValidity("Trap", function(object){
  if(is.numeric(object@x)==FALSE){
    print("x should be a numeric vector!")
  }
  if(is.numeric(object@y)==FALSE){
    print("y should be a numeric vector!")
  }
  if(length(object@x) != length(object@y)){
    print("the length of x and y should be same!")
  }
})

#' @export
setMethod("integrateIt",
          definition=function(x, y, a, b, Trap=TRUE, ...){
            if(is.vector(x)==FALSE | is.vector(y)==FALSE){
              stop("x and y should be a vector!")
            }
            if(is.numeric(x)==FALSE | is.numeric(b)==FALSE |
               is.numeric(a)==FALSE | is.numeric(b)==FALSE){
              stop("x, y, a, and b should be numeric!")
            }
            if(length(x) != length(y)){
              stop("the length of x and y vectors should be same!")
            }
            if(length(a) != 1 | length(b) != 1){
              stop("a and b should be length 1")
            }
            subx <- x[which(x >= a & x <= b)]
            suby <- y[which(x >= a & x <= b)]
            if(Trap==TRUE){
              h2_part <- ((b-a)/(length(subx)-1))/2
              sum_part <- sum(mapply(function(p,q){p*q},
                                     p=suby, q=c(1, rep(2, length(suby)-2), 1)))
              object <- new("Trap",
                            x=subx,
                            y=suby,
                            result=h2_part*sum_part)
            }
            else{
              if(length(subx)%%2 != 1){
                stop("the length of x and y should be odd if you are to use the Simpson rule!")
              }
              h3_part <- ((b-a)/(length(subx)-1))/3
              sum_part <- sum(mapply(function(p,q){p*q},
                                     p=suby, q=c(1, rep(c(4,2), (length(suby)-3)/2),
                                                 4, 1)))
              object <- new("Simpson",
                            x=subx,
                            y=suby,
                            result=h3_part*sum_part)
            }
            return(object)
          })
