#' Create an Object with class "Trap" or "Simpson"
#'
#' Create an instance with S4 class "Trap" or "Simpson"
#'
#'
#' @param x A numeric vector x
#' @param y A numeric vector y (= f(x))
#' @param a A starting value (a should be smaller than b)
#' @param b An ending value (b should be greater than a)
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
#' @aliases integrateIt,ANY-method, plot,Trap-Simpson-method
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
  if(is.numeric(object@x)==FALSE){ # check if x is a numeric
    print("x should be a numeric vector!")
  }
  if(is.numeric(object@y)==FALSE){ # check if y is numeric
    print("y should be a numeric vector!")
  }
  if(length(object@x) != length(object@y)){ # check if x and y have the same length
    print("the length of x and y should be same!")
  }
})

#' @export
setMethod("integrateIt",
          definition=function(x, y, a, b, Trap=TRUE, ...){
            # check if x and y are vectors
            if(is.vector(x)==FALSE | is.vector(y)==FALSE){
              stop("x and y should be a vector!")
            }
            # check if x, y, a, and b are numeric
            if(is.numeric(x)==FALSE | is.numeric(y)==FALSE |
               is.numeric(a)==FALSE | is.numeric(b)==FALSE){
              stop("x, y, a, and b should be numeric!")
            }
            # check if x and y have the same length
            if(length(x) != length(y)){
              stop("the length of x and y vectors should be same!")
            }
            # check if a and b have the length 1
            if(length(a) != 1 | length(b) != 1){
              stop("a and b should be length 1")
            }
            # check if a is smaller than b
            if(a >= b){
              stop("a should be smaller than b!")
            }
            # check if a is larger than min(x)
            if(a < min(x)){
              stop("a should be greater than the minimum of x!")
            }
            if(b > max(x)){
              stop("b should be smaller than the maximum of x!")
            }
            subx <- x[which(x >= a & x <= b)] # limit the range of x to a-b
            suby <- y[which(x >= a & x <= b)] # limit the range of y based on a-b
            if(Trap==TRUE){ # if the Trapezoida rule is required
              h2_part <- ((b-a)/(length(subx)-1))/2 # get (b-a)/n/2
              sum_part <- sum(mapply(function(p,q){p*q},
                                     p=suby, q=c(1, rep(2, length(suby)-2), 1))) # get the sum part
              object <- new("Trap", # create a new "Trap" object
                            x=subx,
                            y=suby,
                            result=h2_part*sum_part)
            }
            else{ # if the Simpson rule is required
              if(length(subx)%%2 != 1){ # check if subsetted x and y have odd length. If not give an error
                stop("the length of x and y should be odd if you are to use the Simpson rule!")
              }
              h3_part <- ((b-a)/(length(subx)-1))/3 # get (b-a)/n/3
              sum_part <- sum(mapply(function(p,q){p*q},
                                     p=suby, q=c(1, rep(c(4,2), (length(suby)-3)/2),
                                                 4, 1))) # get the sum part
              object <- new("Simpson", # create a new "Simpson" object
                            x=subx,
                            y=suby,
                            result=h3_part*sum_part)
            }
            return(object)
          })

#' @export
setMethod("plot", "Trap",
          function(x, y){
            if(class(x)=="Trap"){ # if class is "Trap"
              plot(x@x, x@y, type="n",
                   main="The Result from the Trapezoidal Rule",
                   xlab="x", ylab="f(x)",
                   xlim=c(min(x@x), max(x@x)), ylim=c(min(c(min(x@y), 0)),
                                                      max(c(max(x@y), 0)))) # this ylim should automatically give a right range
              polygon(c((seq(min(x@x), max(x@x), length.out=length(x@x))),
                        rev(seq(min(x@x), max(x@x), length.out=length(x@x)))),
                      c(rep(0, times=length(x@y)), rev(x@y)),
                      border=NA, col=rgb(0,0.9,0.3,0.3)) # color the region of integration
              mapply(function(p,q){segments(p, 0, p, q, lty=2, col="green4")},
                     p=x@x, q=x@y) # draw vertical dotted lines of intervals
              sapply(1:(length(x@x)-1),
                     function(p){segments(x@x[p], x@y[p], x@x[p+1], x@y[p+1],
                                          lwd=2, col="green4")}) # draw straight lines connecting intervals
              abline(h=0, lty=3) # draw a horizontal line at y=0
            }
            else{ # if class is "Simpson"
              if(length(unique(sapply(1:(length(x@x)-1), # check if all intervals have the same length
                                      function(p){x@x[p+1] - x@x[p]}))) != 1){
                stop("the distance between two neighboring values should be same in x!") # if the lengths of intervals differ, return this error
              }
              plot(x@x, x@y, type="n",
                   main="The Result from the Simpson Rule",
                   xlab="x", ylab="f(x)",
                   xlim=c(min(x@x), max(x@x)), ylim=c(min(c(min(x@y), 0)),
                                                      max(c(max(x@y), 0))))
              sapply(1:(length(x@x)-2), function(p){
                u <- x@x[p] # get u
                fu <- x@y[p] # get f(u)
                v <- x@x[p+1] # get v
                fv <- x@y[p+1] # get f(v)
                w <- x@x[p+2] # get w
                fw <- x@y[p+2] # get f(w)
                seqx <- seq(u, w, by=0.01)
                sval <- fu*(((seqx-v)*(seqx-w))/((u-v)*(u-w))) + # calculate p(x)
                  fv*(((seqx-u)*(seqx-w))/((v-u)*(v-w))) +
                  fw*(((seqx-u)*(seqx-v))/((w-u)*(w-v)))
                polygon(c(seqx, rev(seqx)),
                        c(rep(0, times=length(sval)), rev(sval)),
                        border=NA, col="plum1") # color the region of integration
                lines(seqx, sval, lwd=2, col="darkviolet") # draw a curve line of each parabola
              })
              mapply(function(p,q){segments(p, 0, p, q, lty=2, col="darkviolet")},
                     p=x@x, q=x@y) # draw vertical dotted lines of intervals
              abline(h=0, lty=3) # a horizontal line at y=0
            }
          })
