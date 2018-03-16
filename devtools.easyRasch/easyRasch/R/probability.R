#' Probability of Rasch 
#'
#' Finds the probability of a student getting certain exam questions correctly given a parameter
#'
#' @param raschObj A n object of class \code{Rasch}
#' @param theta a proposed value of the theta paramter of the probability equation
#'
#' @return A vector of length n for P_ij for each equation and a specialized vector containing P_ij and Q_ij values depending on correct or incorrect answers
#'
#' @author Benjamin Schneider
#' @note This is a very simple function
#' @examples
#' 
#' raschobject<-new("Rasch", name="Benjamin", a=c(1,2,3,4,5), y_i=c(0,1,0,1,0))
#' probability(raschobject, 2)
#' 
#' @seealso Rasch, 
#' @rdname probability
#' @aliases probability, ANY-method
#' @export
setGeneric(name="probability",
           def=function(raschObj, theta)
           {standardGeneric("probability")}
)
#' @export
setMethod(f="probability",
          definition=function(raschObj, theta){
            a<-as.vector(raschObj@a)
            y_i<-as.vector(raschObj@y_i)
            n<-as.numeric(length(a))
            P_ij<-as.vector(NULL)
            PQ_ij<-as.vector(NULL)
            for (i in 1:n){
              x<-a[i]
              output<-(exp(theta-x)/(1+exp(theta-x)))
              P_ij<-as.vector(c(P_ij,output))}
            for (i in 1:n){
              x<-a[i]
              if (y_i[i]==1){
                output<-(exp(theta-x)/(1+exp(theta-x)))
                }
              if (y_i[i]==0){
                output<-1-(exp(theta-x)/(1+exp(theta-x)))}
                PQ_ij<-as.vector(c(PQ_ij,output))
            }
            output<-list(P_ij, PQ_ij)
            names(output)<-c("P_ij", "PQ_ij")
            return(output)
            }
)