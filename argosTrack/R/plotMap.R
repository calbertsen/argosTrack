
##' Generic map plot
##'
##' @param object Object to plot
##' @param ... other parameters to be passed through to plotting functions.
##' @author Christoffer Moesgaard Albertsen
##' @seealso \code{\link{plotMap,Animal-method}}, \code{\link{plotMap,Movement-method}}, \code{\link{plotMap,Observation-method}}
##' @return Invisibly returns the reference class object
#' @export
#' @importFrom graphics plot
setGeneric("plotMap",
  function(object, ...)
    standardGeneric("plotMap")
  )


##' Map plot for an Animal reference class object
##'
##' @param object Animal reference class object
##' @param plotArgs Arguments to setup background plot
##' @param args Arguments for plotting movement data.
##' @param add If FALSE a new plot window is created.
##' @param obsArgs Arguments for plotting observation data.
##' @param sdArgs Arguments for plotting standard errors.
##' @param ... additional arguments
##' @seealso \code{\link{plotMap}}, \code{\link{plotMap,Movement-method}}, \code{\link{plotMap,Observation-method}}
##' @return Invisibly returns the reference class object
##' @author Christoffer Moesgaard Albertsen
setMethod("plotMap", "Animal",
          function(object, plotArgs = list(), args = list(lwd=3,col="red"),add=FALSE, obsArgs = list(pch=16), sdArgs = list(col = "grey", border=NA),   ...){
              
              rng <- object$getRange(FALSE)
              yrng <- rng[1,]
              yrng <- yrng + 0.1 * c(-1,1) * diff(yrng)
              xrng <- rng[2,]
              xrng <- xrng + 0.1 * c(-1,1) * diff(xrng)

              if(!add){
                  if(!("x" %in% names(plotArgs)))
                      plotArgs$x <- 0
                  if(!("y" %in% names(plotArgs)))
                      plotArgs$y <- 0
                  if(!("type" %in% names(plotArgs)))
                      plotArgs$type <- "n"
                  if(!("ylim" %in% names(plotArgs)))
                      plotArgs$ylim <- yrng
                  if(!("xlim" %in% names(plotArgs)))
                      plotArgs$xlim <- xrng
                  if(!("xlab" %in% names(plotArgs)))
                      plotArgs$xlab <- expression(paste("Longitude (",degree,")",sep=""))
                  if(!("ylab" %in% names(plotArgs)))
                      plotArgs$ylab <- expression(paste("Latitude (",degree,")",sep=""))
                  if(!("asp" %in% names(plotArgs)))
                     plotArgs$asp <- 1/cos((mean(yrng) * pi) / 180)
                  do.call("plot",plotArgs)
              }
              object$addToMapPlot(obsArgs,args)
              invisible(object)  
          }
          )

##' Map plot for a Movement reference class object
##'
##' @param object Movement reference class object
##' @param plotArgs Arguments to setup background plot
##' @param args Arguments for plotting movement data.
##' @param add If FALSE a new plot window is created.
##' @param sdArgs Arguments for plotting standard errors.
##' @param ... additional arguments
##' @return Invisibly returns the reference class object
##' @seealso \code{\link{plotMap}}, \code{\link{plotMap,Animal-method}}, \code{\link{plotMap,Observation-method}}
##' @author Christoffer Moesgaard Albertsen
setMethod("plotMap", "Movement",
          function(object, plotArgs = list(), args = list(pch=16,lwd=3,col="red"),add=FALSE, sdArgs = list(col = "grey", border=NA), ...){
              
               rng <- object$getRange(FALSE)
              yrng <- rng[1,]
              yrng <- yrng + 0.1 * c(-1,1) * diff(yrng)
              xrng <- rng[2,]
              xrng <- xrng + 0.1 * c(-1,1) * diff(xrng)

              if(!add){
                  if(!("x" %in% names(plotArgs)))
                      plotArgs$x <- 0
                  if(!("y" %in% names(plotArgs)))
                      plotArgs$y <- 0
                  if(!("type" %in% names(plotArgs)))
                      plotArgs$type <- "n"
                  if(!("ylim" %in% names(plotArgs)))
                      plotArgs$ylim <- yrng
                  if(!("xlim" %in% names(plotArgs)))
                      plotArgs$xlim <- xrng
                  if(!("xlab" %in% names(plotArgs)))
                      plotArgs$xlab <- expression(paste("Longitude (",degree,")",sep=""))
                  if(!("ylab" %in% names(plotArgs)))
                      plotArgs$ylab <- expression(paste("Latitude (",degree,")",sep=""))
                  if(!("asp" %in% names(plotArgs)))
                     plotArgs$asp <- 1/cos((mean(yrng) * pi) / 180)
                  do.call("plot",plotArgs)
              }
              object$addToMapPlot(args)
              invisible(object)
          }
          )


##' Map plot for an Observation reference class object
##'
##' @param object Observation reference class object
##' @param plotArgs Arguments to setup background plot
##' @param args Arguments for plotting latitude observation data.
##' @param add If FALSE a new plot window is created.
##' @param sdArgs Arguments for plotting standard errors.
##' @param ... additional arguments
##' @return Invisibly returns the reference class object
##' @seealso \code{\link{plotMap}}, \code{\link{plotMap,Animal-method}}, \code{\link{plotMap,Movement-method}}
##' @author Christoffer Moesgaard Albertsen
setMethod("plotMap", "Observation",
          function(object, plotArgs = list(), args = list(pch=16,lwd=3,col="red"),add=FALSE, sdArgs = list(col = "grey", border=NA), ...){
              
              rng <- object$getRange()
              yrng <- rng[1,]
              yrng <- yrng + 0.1 * c(-1,1) * diff(yrng)
              xrng <- rng[2,]
              xrng <- xrng + 0.1 * c(-1,1) * diff(xrng)

              if(!add){
                  if(!("x" %in% names(plotArgs)))
                      plotArgs$x <- 0
                  if(!("y" %in% names(plotArgs)))
                      plotArgs$y <- 0
                  if(!("type" %in% names(plotArgs)))
                      plotArgs$type <- "n"
                  if(!("ylim" %in% names(plotArgs)))
                      plotArgs$ylim <- yrng
                  if(!("xlim" %in% names(plotArgs)))
                      plotArgs$xlim <- xrng
                  if(!("xlab" %in% names(plotArgs)))
                      plotArgs$xlab <- expression(paste("Longitude (",degree,")",sep=""))
                  if(!("ylab" %in% names(plotArgs)))
                      plotArgs$ylab <- expression(paste("Latitude (",degree,")",sep=""))
                  if(!("asp" %in% names(plotArgs)))
                     plotArgs$asp <- 1/cos((mean(yrng) * pi) / 180)
                  do.call("plot",plotArgs)
              }
              object$addToMapPlot(args)
              invisible(object)
          }
          )

