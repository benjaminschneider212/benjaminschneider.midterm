library(devtools)
library(roxygen2)

#reload
setwd("/Users/benjaminschneider/Desktop/poisson.lik/")
current.code<-as.package("PoissonLikelihood")
load_all(current.code)
document(current.code)

###PRO TIP
package.skeleton()

setClass(Class="AllSquares",
         representation = representation(
           addSquare = "numeric",
           subtractSquare = "numeric",
           x = "numeric",
           y = "numeric",
           square= "character"
         ),
         prototype = prototype(
           addSquare = numeric(),
           subtractSquare=numeric(),
           x = numeric(),
           y = numeric(),
           square=character()
         )
)

setMethod("initialize", "AllSquares", 
          function(.Object, ...){
            value=callNextMethod()
            return(value)
          }
) 

#basic example
poisson.lik(4, c(4,4,4,4))

?poisson.lik


new("Poisson")