#' Print function for student name and EAP 
#'
#' Lists the name of the student taking the test and finds the value of the expected a posteriori given an input object of class \code{Rasch} and a lower and upper bound.
#'
#' @param raschObj an object of class \code{Rasch}
#' @param lower a proposed lower bound of integration. The default is -6
#' @param upper a proposed upper bound of integration. The default is 6
#'
#' @return the name of the student and the expected a posteriori value
#'
#' @author Benjamin Schneider
#' @note This function is a slight upgrade of the \code{eap} function in that it also provides the name of the student.
#' @examples
#' 
#' raschobject<-new("Rasch", name="Benjamin", a=c(1,2,3,4,5), y_i=c(0,1,0,1,0))
#' upper<-5
#' lower<-0
#' print(raschobject, lower, upper)
#' 
#' @seealso \code{\link{Rasch}}, \code{\link{likelihood}}, \code{\link{prior}}, \code{\link{eap}}, \code{\link{probability}}
#' @rdname print
#' @aliases print, ANY-method
#' @export
setGeneric(name="print",
           def=function(raschObj, lower = -6, upper = 6)
           {standardGeneric("print")}
)
#' @export
setMethod(f="print",
          definition=function(raschObj, lower = -6, upper = 6){
            eapoutput<-eap(raschObj, lower, upper)
            output<-list(raschObj@name, eapoutput)
            names(output)<-c("Name of test taker", "expected a posteriori value")
            return(output)
          })
