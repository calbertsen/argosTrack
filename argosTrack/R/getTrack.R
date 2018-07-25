
##' Generic function to extract movement track.
##'
##' Generic function to extract movement track.
##' @param object The object from which to extract the track
##' @param ... Additional arguments passed to the appropriate method
##' @return A data frame of positions
##' @seealso \code{\link{getTrack,Animal-method}} \code{\link{getTrack,Movement-method}} \code{\link{getTrack,Observation-method}}
##' @export
setGeneric("getTrack",
  function(object, ...)
    standardGeneric("getTrack")
  )

##' Extracts track from animal data
##'
##' Extracts the estimated and observed track from an Animal object
##' @param object \link{Animal} reference class object
##' @param ... Additional arguments passed to the appropriate method
##' @return A data frame of positions
##' @author Christoffer Moesgaard Albertsen
##' @method getTrack Animal
setMethod("getTrack", "Animal",
          function(object,
                   ...){
              mtr <- getTrack(object$movement)
              otr <- getTrack(object$observation)
              r <- merge(otr,mtr,by="dates",all=TRUE)
              r$id <- object$name
              return(r[c(1,ncol(r),2:(ncol(r)-1))])
          })




##' Extracts track from animal data
##'
##' Extracts the estimated track from a Movement object
##' @param object \link{Movement} reference class object
##' @param ... Additional arguments passed to the appropriate method
##' @return A data frame of positions
##' @author Christoffer Moesgaard Albertsen
##' @method getTrack Movement
setMethod("getTrack", "Movement",
          function(object,
                   ...){
              r <- data.frame(dates=object$dates,
                              est.lat=object$mu[1,],
                              est.lon=object$mu[2,],
                              sd.lat=object$sdmu[1,],
                              sd.lon=object$sdmu[2,])
              return(r)
          })





##' Extracts track from animal data
##'
##' Extracts the observed track from an Observation object
##' @param object \link{Observation} reference class object
##' @param ... Additional arguments passed to the appropriate method
##' @return A data frame of positions
##' @author Christoffer Moesgaard Albertsen
##' @method getTrack Observation
setMethod("getTrack", "Observation",
          function(object,
                   ...){
              r <- data.frame(dates=object$dates,
                              obs.lat=object$lat,
                              obs.lon=object$lon,
                              obs.lc=object$getLocationClassInput())
              return(r)
          })


