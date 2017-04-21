

##' Generic function to simulate tracks
##'
##' @param object Object to simulate from
##' @param n Number of replications
##' @param ... other parameters
##' @author Christoffer Moesgaard Albertsen
##' @seealso \code{\link{simTrack,Animal-method}}, \code{\link{simTrack,Measurement-method}}, \code{\link{simTrack,Movement-method}}
#' @export
setGeneric("simTrack",
  function(object,n, ...)
    standardGeneric("simTrack")
  )


##' Simulate from a movement model
##'
##' @param object Movement reference class object implementing the model to simulate from
##' @param n Number of replications
##' @param x0 Initial values
##' @param ... not used
##' @return A 2 x number of time steps in object x n array of simulated values
##' @author Christoffer Moesgaard Albertsen
##' @seealso \code{\link{simTrack}}, \code{\link{simTrack,Animal-method}}, \code{\link{simTrack,Measurement-method}}
setMethod("simTrack", "Movement",
          function(object,
                   n = 1,
                   x0 = object$mu[,1],
                   ...){
              X <- replicate(n,object$simulate(x0 = x0))
              return(X)
          }
          )

##' Simulate from a measurement model with observation information
##'
##' @param object Measurement reference class object to simulate from
##' @param n  Number of replications
##' @param observation Observation reference class object with time points and Argos location class information to use in the simulation
##' @param ... Not used
##' @return A 2 x number of time steps in observation x n array of simulated values
##' @author Christoffer Moesgaard Albertsen
##' @seealso \code{\link{simTrack}}, \code{\link{simTrack,Animal-method}}, \code{\link{simTrack,Movement-method}}
setMethod("simTrack", c("Measurement"),
          function(object,
                   n = 1,
                   observation,
                   ...){
              if(!class(observation) == "Observation")
                  stop("Observation must be an Observation object.")
              X <- replicate(n,object$simulate(observation))
              return(X)
          }
          )

##' Simulate from an Animal state-space model
##'
##' @param object Animal reference class describing the state-space model to simulate from
##' @param n Number of replications
##' @param newObject Should a new Animal object be added to the returned matrix?
##' @param ... Not used
##' @return A (2 + newObject) x n matrix where the first row (X) contains lists with a matrix of simulated movement tracks, the second row (Y) contains lists with a matrix of simulated observations, and the third row (Animal - if present) contains lists with a new Animal object based on the simulated values.
##' @author Christoffer Moesgaard Albertsen
##' @seealso \code{\link{simTrack}}, \code{\link{simTrack,Measurement-method}}, \code{\link{simTrack,Movement-method}}
setMethod("simTrack", "Animal",
          function(object,
                   n = 1,
                   newObject = TRUE,
                   ...){
              X <- replicate(n,object$simulate(newObject))
              return(X)
          }
          )
