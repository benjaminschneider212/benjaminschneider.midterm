#' Rasch Object 
#' 
#' Object of class \code{Rasch} is an input in multiple functions in the \code{easyRasch} package
#'
#' 
#' An object of the class `Rasch' has the following slots:
#' \itemize{
#' \item \code{name} The name of the test taker
#' \item \code{a}  a vector of question item parameters
#' \item \code{y_i} a vector of answered for the respondent y_j}
#'
#' @author Benjamin R. Schneider: \email{benjamin.schneider@@wustl.edu}
#' @note \code{a} and \code{y_i} must be of the same length
#' @aliases Rasch-class initialize, Rasch-method 
#' @rdname Rasch
#' @export
setClass(Class="Rasch",
         representation = representation(
           name = "character",
           a = "numeric",
           y_i = "numeric"
         ),
         prototype = prototype(
           name = character(),
           a = numeric(),
           y_i = numeric()
         )
)
#' @export
setMethod("initialize", "Rasch", 
          function(.Object, ...){
            value=callNextMethod()
            return(value)
          }
) 
#' @export
Rasch.test <- function(raschObj) {
  if(length(raschObj@a) == length(raschObj@y_i)) TRUE 
  else paste("Unequal a and y_i lengths: ", length(raschObj@a), ", ",
             length(raschObj@y_i), sep="") 
}
#' @export
setValidity("Rasch", Rasch.test)