library(devtools)
library(roxygen2)

#reload
setwd("/Users/benjaminschneider/Documents/GitHub/benjaminschneider.midterm/devtools.easyRasch")
current.code<-as.package("easyRasch")
load_all(current.code)
document(current.code)

#code for class structure:
setClass(Class="Rasch",
         representation = representation(
           name = "character",
           a = "numeric",
           y_i = "numeric"
         ),
         prototype = prototype(
           name = character(),
           a=numeric(),
           y_i = numeric()
         )
)

setMethod("initialize", "Rasch", 
          function(.Object, ...){
            value=callNextMethod()
            return(value)
          }
) 

#code for Probability function:
setGeneric(name="probability",
           def=function(raschObj, theta)
           {standardGeneric("probability")}
)

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

#code for Likelihood function
setGeneric(name="likelihood",
           def=function(raschObj, theta)
           {standardGeneric("likelihood")}
)

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

#code for prior function:

setGeneric(name="prior",
           def=function(theta)
           {standardGeneric("prior")}
)

setMethod(f="prior",
          definition=function(theta){
            output<-dnorm(theta, mean=0, sd=3)
            return(output)
          })

#code for eap
setGeneric(name="eap",
           def=function(raschObj, lower = -6, upper = 6)
           {standardGeneric("eap")}
)

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

#basic examples of functionality:

numerator$value/denominator$value


?probability
?likelihood
?prior
raschObj<-new("Rasch", name="Benjamin", a=c(1,2,3,4,5), y_i=c(0,1,0,1,0))
raschobject<-new("Rasch", name="Benjamin", a=c(1,2,3,4,5), y_i=c(0,1,0,1,0))
theta<-2
probability(raschobject, theta)
likelihood(raschobject, theta)
prior(theta)
eap(raschobject)
