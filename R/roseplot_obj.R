

##' Generic roseplot
##'
##' @param object Object to plot
##' @param ... other parameters
##' @author Christoffer Moesgaard Albertsen
#' @export
setGeneric("roseplot",
  function(object, ...)
    standardGeneric("roseplot")
  )

##' Roseplot for Movement reference class object
##'
##' @param object Movement reference class object
##' @param nclass Number of histogram classes
##' @param prob Scale the frequencies to probabilities?
##' @param type 'step' for plotting step lengths, 'angle' for plotting angles and 'both' for plotting both.
##' @param bearings Plot bearings (TRUE) or turning angles (FALSE)
##' @param show Should a plot be produced?
##' @param ... not used
##' @return Invisibly returns a list of bearings, step lengths and step lengths in km/h.
##' @author Christoffer Moesgaard Albertsen
##'
##' @examples
##' d <- subadult_ringed_seal
##' obs <- Observation(lon = d$lon,
##'                    lat = d$lat,
##'                    dates = as.POSIXct(d$date),
##'                    locationclass = d$lc
##'                    )
##' meas <- Measurement(model="n")
##' mov <- RW(unique(obs$dates))
##' roseplot(mov)
##' anim <- Animal(obs,mov,meas,"Subadult")
##' plot(anim)
##' 
setMethod("roseplot", "Movement",
          function(object,
                   nclass = 35,
                   prob=TRUE,
                   type=c("both","step","angle"),
                   bearings=FALSE,
                   show = TRUE,
                   ...){

                       type <- match.arg(type)
                       
                       bear <- bearing(head(object$mu[2,],-1),
                                  head(object$mu[1,],-1),
                                  tail(object$mu[2,],-1),
                                  tail(object$mu[1,],-1),
                                  object$nauticalStates)
              turningangles <- diff(bear)
              steplengths <- stepLength(head(object$mu[2,],-1),
                                         head(object$mu[1,],-1),
                                         tail(object$mu[2,],-1),
                                         tail(object$mu[1,],-1),
                                         object$nauticalStates)
              steplengths_per_hour <- steplengths / as.numeric(difftime(tail(object$dates,-1),
                                                                        head(object$dates,-1),
                                                                        units="hour"))

              if(show){
                  if(type=="both"){
                      layout(matrix(1:2,1,2))
                  }
                  if(type %in% c("both","step"))
                      hist(steplengths_per_hour * 1.852,
                           xlab = "Step length (km/h)",
                           main = NULL,
                           nclass = nclass,
                           prob = prob,
                           ...)
                  if(type %in% c("both","angle"))
                      if(bearings){
                          .roseplot(bear,
                                   breaks = nclass,
                                   prob=prob,
                                   xlab="Directional bearings (radians)",...)
                      }else{
                          .roseplot(turningangles,
                                   breaks = nclass,
                                   prob=prob,
                                   xlab="Turning angles between states (radians)",...)
                      }
              }
              invisible(list(bearings=bear,
                             steplengths=steplengths,
                             steplengths_per_hour=steplengths_per_hour))
          }
          )

##' Roseplot for Animal reference class object
##'
##' Calls roseplot on object$movement
##' @param object Animal reference class object
##' @param ... parameters passed to the method for the Movement reference class
##' @return As \code{\link{roseplot,Movement-method}}
##' @seealso \code{\link{roseplot,Movement-method}}
##' @author Christoffer Moesgaard Albertsen
setMethod("roseplot", "Animal",
          function(object,...)
              roseplot(object$movement,...)
          )
