library(devtools)
library(roxygen2)

#reload
setwd("/Users/benjaminschneider/Documents/GitHub/benjaminschneider.midterm/devtools.easyRasch")
current.code<-as.package("easyRasch")
load_all(current.code)
document(current.code)

###PRO TIP
package.skeleton()

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

blah<-new("Rasch", name="Benjamin", a=c(1,2,3,4,5), y_i=c(0,1,0,1,0))

#basic example
poisson.lik(4, c(4,4,4,4))

?poisson.lik


new("Poisson")