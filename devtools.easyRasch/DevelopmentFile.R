#This development file contains all of the code and documentation for said code for the package. You will NOT find documentation in the individual R files so that this can be a one stop shop for grading.
#That being said, the R files contain the necessary Roxygen code in order to generate functioning (hopefully with the correct functionality) functions and helpful help files.

library(devtools)
library(roxygen2)

#reload
setwd("/Users/benjaminschneider/Documents/GitHub/benjaminschneider.midterm/devtools.easyRasch")
current.code<-as.package("easyRasch")
load_all(current.code)
document(current.code)

#code for class structure:
setClass(Class="Rasch", #this is us setting the class. I use upper case R to indicate that this is a class
         representation = representation(
           name = "character", #this is a character because of course our names will not be numeric
           a = "numeric", #this will be a vector of n length
           y_i = "numeric" #this will be a vector of n length
         ),
         prototype = prototype(
           name = character(), #Just doing the same above, I know this does something, but I am not sure what, but I know that is has to be included
           a=numeric(),
           y_i = numeric()
         )
)

setMethod("initialize", "Rasch", #this sets up our initialize method so that we are able to create objects of the class Rasch
          function(.Object, ...){
            value=callNextMethod()
            return(value)
          }
) 

#Validity test, So the biggest possible error I think that would be run into is if the two vectors are not of the same length, this fixes that problem
Rasch.test <- function(raschObj) {
  if(length(raschObj@a) == length(raschObj@y_i)) TRUE #this just checks if both are of the same length
  else paste("Unequal a and y_i lengths: ", length(raschObj@a), ", ",
             length(raschObj@y_i), sep="") #if not than it prints this and shows the length of the two vectors as they currently are, I got this from off line and modified it.
}

setValidity("Rasch", Rasch.test) #this sets us up so we know we are creating valid Rasch objects each time. We know that the character name won't get messed up so we are good!

#code for Probability function:
setGeneric(name="probability", #setting up our generic for the probability function
           def=function(raschObj, theta) #we have two arguments that can be taken in in this case.
           {standardGeneric("probability")}
)

setMethod(f="probability",
          definition=function(raschObj, theta){ #necessary code to set up the function
            a<-as.vector(raschObj@a) #this subsets the rasch object and extracts a vector for a
            y_i<-as.vector(raschObj@y_i) #this subsets the rasch object and extracts a vector for y_i
            n<-as.numeric(length(a)) #this length is used for the for loop below
            P_ij<-as.vector(NULL) #creation of empty vectors for our eventual outputs
            PQ_ij<-as.vector(NULL)
            for (i in 1:n){ #a for loop for every single value of a
              x<-a[i] #this extracts the first value each time and is replaces with each new cycle of the loop
              output<-(exp(theta-x)/(1+exp(theta-x))) #this takes in our extracted a value and theta value to run the equation
              P_ij<-as.vector(c(P_ij,output))} #this adds the new value each time so we form a vactor with all of the P_ij values
            for (i in 1:n){ #a for loop for every single value of a and y_i, this is a little different
              x<-a[i] #this extracts the first value each time and is replaces with each new cycle of the loop
              if (y_i[i]==1){ #we set this condition fo the purpose of getting out a P-ij value when the answer is correct
                output<-(exp(theta-x)/(1+exp(theta-x))) #this is the same as above and sends the value below to be added into the vector
              }
              if (y_i[i]==0){ #now we are creating a Q_ij value for our wrong answers
                output<-1-(exp(theta-x)/(1+exp(theta-x)))} #this functionality is the exact same but we just subtract P_ij from ! in order to get our Q_ij
              PQ_ij<-as.vector(c(PQ_ij,output)) #regardless of whether there is a P_ij and Q_ij value, they will be added here and we can compare this vector to the vector above to know we are right
            }
            output<-list(P_ij, PQ_ij) #making a list out our outputs so they can both be viewed (and later manipulated)
            names(output)<-c("P_ij", "PQ_ij") #Naming the values for the ease of the user
            return(output) #creating out output and ta-da we have our probability vectors!
          }
)

#code for Likelihood function
setGeneric(name="likelihood", #doing this again
           def=function(raschObj, theta) #we have the same inputs above which we will pass into the function we made above
           {standardGeneric("likelihood")}
)

setMethod(f="likelihood",
          definition=function(raschObj, theta){ #mirroring the above for the purpose of the creation of our function
            ourworkingoutputs<-probability(raschObj,theta) #so here i call in the probability function and save the output as a new object so I can subset it
            PQ_ij<-ourworkingoutputs$PQ_ij #I extract the PQ_ij vector here
            xi<-1 #I create this value so that I can start the fo loop below with something I multiply onto and this will not effect the value (similar to creating the null vector above)
            for(i in 1:length(PQ_ij)){#so now I call the for loop and I do it for the length of the two vectors
              x<-PQ_ij[i] # I extract the element I am working with in the loop
              xi<-xi*x #i multiply it to the previous in order to create the output
            }
            return(xi) #return the output of the likelihood function
})

#code for prior function:

setGeneric(name="prior", #same thing as always
           def=function(theta) #only have one argument
           {standardGeneric("prior")}
)

setMethod(f="prior",
          definition=function(theta){ #taking in the argument again
            output<-dnorm(theta, mean=0, sd=3) #this is super easy and all it involved was calling in the theta and properly doing the dnorm function with the requested values.
            return(output) #easy as that!
          })

#code for EAP function
setGeneric(name="eap", #creating the generic
           def=function(raschObj, lower = -6, upper = 6) #doing our arguments with the defaults
           {standardGeneric("eap")}
)

setMethod(f="eap",
          definition=function(raschObj, lower = -6, upper = 6){ #repeating the arguments and defaults again!
            integrand.denominator <- function(theta) {likelihood(raschObj, theta)*prior(theta)} #creating the integrand of the denominator, in terms of theta
            integrand.numerator <- function(theta) {theta*likelihood(raschObj, theta)*prior(theta)} #creating the integrand for the numerator, in terms of theta
            denominator<-integrate(integrand.denominator, lower, upper) #doing the actual integral with created function
            numerator<-integrate(integrand.numerator, lower, upper) #doing the actual integral with created function
            output<-numerator$value/denominator$value #have to subset the value because the output is as a list
            return(output) #the output of the eap function. If the lower and upper bounds are lower=-n and upper=n then the output will be 0
          })

#code for the print function
setGeneric(name="print", #last function
           def=function(raschObj, lower = -6, upper = 6) #this is the same as the eap
           {standardGeneric("print")}
)

setMethod(f="print",
          definition=function(raschObj, lower = -6, upper = 6){ #same way of doing the defaults
            eapoutput<-eap(raschObj, lower, upper) #here we call the eap function in
            output<-list(raschObj@name, eapoutput) #we subset the name out of our rasch object to create a list
            names(output)<-c("Name of test taker", "expected a posteriori value") #names for ease of interpretation
            return(output) #our output is good to go!
          })

#basic examples of functionality:

?probability
?likelihood
?prior
?eap
?print

raschObj<-new("Rasch", name="Benjamin", a=c(1,2,3,4,5,6), y_i=c(0,1,0,1,0))
raschobject<-new("Rasch", name="Benjamin", a=c(1,2,3,4,5), y_i=c(0,1,0,1,0))
theta<-2
probability(raschobject, theta)
likelihood(raschobject, theta)
prior(theta)
eap(raschobject)
eap(raschobject, 0,5)
print(raschobject, 0,5)
