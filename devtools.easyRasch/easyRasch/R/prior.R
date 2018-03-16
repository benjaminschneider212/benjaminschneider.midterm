#' Prior of a given theta 
#'
#' Finds the height of a specified normal curve evaluated at theta
#'
#' @param theta a proposed value of the theta paramter of the probability equation
#'
#' @return The height of a specified normal curve evaluated at theta when mean=0 and standard deviation=3
#'
#' @author Benjamin Schneider
#' @note This is a very simple function
#' @examples
#' 
#' theta<-2
#' prior(theta)
#' 
#' @seealso Rasch, likelihood, probability 
#' @rdname prior
#' @aliases prior, ANY-method
#' @export
setGeneric(name="prior",
           def=function(theta)
           {standardGeneric("prior")}
)
#' @export
setMethod(f="prior",
          definition=function(theta){
            output<-dnorm(theta, mean=0, sd=3)
            return(output)
          })
