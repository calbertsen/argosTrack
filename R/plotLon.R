
##' Generic longitude plot
##'
##' @param object Object to plot
##' @param ... other parameters to be passed through to plotting functions.
##' @author Christoffer Moesgaard Albertsen
##' @seealso \code{\link{plotLon,Animal-method}}, \code{\link{plotLon,Movement-method}}, \code{\link{plotLon,Observation-method}}
##' @export
setGeneric("plotLon",
  function(object, ...)
    standardGeneric("plotLon")
  )


##' Longitude plot for an Animal reference class object
##'
##' @param object Animal reference class object
##' @param plotArgs Arguments to setup background plot
##' @param args Arguments for plotting longitude movement data.
##' @param add If FALSE a new plot window is created.
##' @param obsArgs Arguments for plotting longitude observation data.
##' @param sdArgs Arguments for plotting standard errors.
##' @param sd Should standard errors be plotted?
##' @param ... additional arguments
##' @seealso \code{\link{plotLon}}, \code{\link{plotLon,Movement-method}}, \code{\link{plotLon,Observation-method}}
##' @author Christoffer Moesgaard Albertsen
setMethod("plotLon", "Animal",
          function(object, plotArgs = list(), args = list(lwd=3,col="red"),add=FALSE, obsArgs = list(pch=16), sdArgs = list(col = "grey", border=NA), sd=FALSE,   ...){
              
              yrng <- object$getRange(sd)[2,]
              yrng <- yrng + 0.1 * c(-1,1) * diff(yrng)

              dat <- unique(c(object$observation$dates,
                              object$movement$dates))
              
              if(!add){
                  if(!("x" %in% names(plotArgs)))
                      plotArgs$x <- dat
                  if(!("y" %in% names(plotArgs)))
                      plotArgs$y <- rep(0,length(dat))
                  if(!("type" %in% names(plotArgs)))
                      plotArgs$type <- "n"
                  if(!("ylim" %in% names(plotArgs)))
                      plotArgs$ylim <- yrng
                  if(!("xlim" %in% names(plotArgs)))
                      plotArgs$xlim <- range(dat)
                  if(!("xlab" %in% names(plotArgs)))
                      plotArgs$xlab <- "Date"
                  if(!("ylab" %in% names(plotArgs)))
                      plotArgs$ylab <- expression(paste("Longitude (",degree,")",sep=""))
                  do.call("plot",plotArgs)
              }
              object$addToLonPlot(obsArgs,args,sdArgs,sd)
                            
          }
          )


##' Longitude plot for a Movement reference class object
##'
##' @param object Movement reference class object
##' @param plotArgs Arguments to setup background plot
##' @param args Arguments for plotting longitude movement data.
##' @param add If FALSE a new plot window is created.
##' @param sdArgs Arguments for plotting standard errors.
##' @param sd Should standard errors be plotted?
##' @param ... additional arguments
##' @seealso \code{\link{plotLon}}, \code{\link{plotLon,Animal-method}}, \code{\link{plotLon,Observation-method}}
##' @author Christoffer Moesgaard Albertsen
setMethod("plotLon", "Movement",
          function(object, plotArgs = list(), args = list(),add=FALSE, sdArgs = list(col = "grey", border=NA), sd=TRUE,   ...){
              
              yrng <- object$getRange(sd)[2,]
              yrng <- yrng + 0.1 * c(-1,1) * diff(yrng)
              
              if(!add){
                  if(!("x" %in% names(plotArgs)))
                      plotArgs$x <- object$dates
                  if(!("y" %in% names(plotArgs)))
                      plotArgs$y <- rep(0,length(object$dates))
                  if(!("type" %in% names(plotArgs)))
                      plotArgs$type <- "n"
                  if(!("ylim" %in% names(plotArgs)))
                      plotArgs$ylim <- yrng
                  if(!("xlim" %in% names(plotArgs)))
                      plotArgs$xlim <- range(object$dates)
                  if(!("xlab" %in% names(plotArgs)))
                      plotArgs$xlab <- "Date"
                  if(!("ylab" %in% names(plotArgs)))
                      plotArgs$ylab <- expression(paste("Longitude (",degree,")",sep=""))
                  do.call("plot",plotArgs)
              }
              if(sd)
                  object$addToLonPlotSd(sdArgs)
              object$addToLonPlot(args)
                            
          }
          )

##' Longitude plot for an Observation reference class object
##'
##' @param object Observation reference class object
##' @param plotArgs Arguments to setup background plot
##' @param args Arguments for plotting Longitude observation data.
##' @param add If FALSE a new plot window is created.
##' @param ... additional arguments
##' @author Christoffer Moesgaard Albertsen
##'  @seealso \code{\link{plotLon}}, \code{\link{plotLon,Animal-method}}, \code{\link{plotLon,Movement-method}}
setMethod("plotLon", "Observation",
          function(object, plotArgs = list(), args = list(),add=FALSE, ...){
              
              yrng <- object$getRange()[2,]
              yrng <- yrng + 0.1 * c(-1,1) * diff(yrng)
              
              if(!add){
                  if(!("x" %in% names(plotArgs)))
                      plotArgs$x <- object$dates
                  if(!("y" %in% names(plotArgs)))
                      plotArgs$y <- rep(0,length(object$dates))
                  if(!("type" %in% names(plotArgs)))
                      plotArgs$type <- "n"
                  if(!("ylim" %in% names(plotArgs)))
                      plotArgs$ylim <- yrng
                  if(!("xlim" %in% names(plotArgs)))
                      plotArgs$xlim <- range(object$dates)
                  if(!("xlab" %in% names(plotArgs)))
                      plotArgs$xlab <- "Date"
                  if(!("ylab" %in% names(plotArgs)))
                      plotArgs$ylab <- expression(paste("Longitude (",degree,")",sep=""))
                  do.call("plot",plotArgs)
              }
              object$addToLonPlot(args)
                            
          }
          )
