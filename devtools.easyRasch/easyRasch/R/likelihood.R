#' Likelihood of Rasch 
#'
#' Finds the likelihood function of a proposed value of theta
#'
#' @param raschObj A n object of class \code{Rasch}
#' @param theta a proposed value of the theta paramter of the probability equation
#'
#' @return The output of the likelihood function
#'
#' @author Benjamin Schneider
#' @note This is a very simple function
#' @examples
#' 
#' raschobject<-new("Rasch", name="Benjamin", a=c(1,2,3,4,5), y_i=c(0,1,0,1,0))
#' likelihood(raschobject, 2)
#' 
#' @seealso Rasch, 
#' @rdname likelihood
#' @aliases likelihood, ANY-method
#' @export
setGeneric(name="likelihood",
           def=function(raschObj, theta)
           {standardGeneric("likelihood")}
)
#' @export
setMethod(f="likelihood",
          definition=function(raschObj, theta){
            ourworkingoutputs<-probability(raschObj,theta)
            P_ij<-ourworkingoutputs$P_ij
            PQ_ij<-ourworkingoutputs$PQ_ij
            xi<-1
            for(i in 1:length(P_ij)){
              x<-P_ij[i]*PQ_ij[i]
              xi<-xi*x
            }
            return(xi)
          })            
            