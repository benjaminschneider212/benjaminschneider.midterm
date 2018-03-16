#' Evaluation of the expected a posteriori
#'
#' Finds the value of the expected a posteriori given an input object of class \code{Rasch} and a lower and upper bound.
#'
#' @param raschObj an object of class \code{Rasch}
#' @param lower a proposed lower bound of integration. The default is -6
#' @param upper a proposed upper bound of integration. The default is 6
#'
#' @return the expected a posteriori value
#'
#' @author Benjamin Schneider
#' @note This function provides a value that will be also output in the print function.
#' @examples
#' 
#' raschobject<-new("Rasch", name="Benjamin", a=c(1,2,3,4,5), y_i=c(0,1,0,1,0))
#' upper<-5
#' lower<-0
#' eap(raschobject, lower, upper)
#' 
#' @seealso \code{\link{Rasch}}, \code{\link{likelihood}}, \code{\link{prior}}, \code{\link{probability}}, \code{\link{print}}
#' @rdname eap
#' @aliases eap, ANY-method
#' @export
setGeneric(name="eap",
           def=function(raschObj, lower = -6, upper = 6)
           {standardGeneric("eap")}
)
#' @export
setMethod(f="eap",
          definition=function(raschObj, lower = -6, upper = 6){
            x<-lower
            y<-upper
            integrand1 <- function(theta) {likelihood(raschObj, theta)*prior(theta)}
            integrand2 <- function(theta) {theta*likelihood(raschObj, theta)*prior(theta)}
            denominator<-integrate(integrand1, x, y)
            numerator<-integrate(integrand2, x, y)
            output<-numerator$value/denominator$value
            return(output)
          })
