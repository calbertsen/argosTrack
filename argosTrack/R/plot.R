##' Plotting an Animal object
##'
##' Plots time \eqn{\times} latitude, time \eqn{\times} longitude and longitude \eqn{\times} latitude for an Animal reference class object.
##' @param x Animal reference class object
##' @param ... other parameters to be passed through to plotting functions.
##' @seealso \code{\link{plotMap}}, \code{\link{plotLat}}, and \code{\link{plotLon}}
##' @return Invisibly returns the reference class object
##' @author Christoffer Moesgaard Albertsen
##' @importFrom graphics layout
##' @export
plot.Animal <- function(x,...){
    oldMar <- par("mar")
    args <- as.list(match.call(expand.dots=TRUE))[-1]
    graphics::layout(matrix(c(rep(1,4*3),2,2,3,3,2,2,3,3),nrow=4))
    names(args)[1] <- "object"
    par(mar = c(4.1,4.1,4.1,0.1))
    do.call("plotMap",args)
    par(mar = c(0.1,0.1,4.1,4.1))
    if(is.null(args$plotArgs))
        args$plotArgs <- list()
    args$plotArgs$axes = FALSE
    args$plotArgs$xlab = NA
    args$plotArgs$ylab = NA
    cexbase <- eval(if(match("cex",names(args$plotArgs),nomatch=FALSE)) args$plotArgs$cex else par("cex"))
    cexlab <- eval(if((match("cex.lab",names(args$plotArgs),nomatch=FALSE))) args$plotArgs$cex.lab else par("cex.lab"))
    do.call("plotLat",args)
    allDates <- sort(unique(c(x$observation$dates,x$movement$dates)))
    axis.POSIXct(side=3, x = allDates);axis(side=4);box()
    mtext(expression(paste("Latitude (",degree,")",sep="")),side = 4,line = 3, cex = cexlab * cexbase)
    mtext("Date",side = 3,line = 3, cex = cexlab * cexbase)
    par(mar = c(4.1,0.1,0.1,4.1))
    do.call("plotLon",args)
    axis.POSIXct(side=1, x = allDates);axis(side=4);box()
    mtext(expression(paste("Longitude (",degree,")",sep="")),side = 4,line = 3,
          cex = cexlab * cexbase)
    mtext("Date",side = 1,line = 3, cex = cexlab * cexbase)
    par(mar = oldMar)
    invisible(x)
}

##' Plotting a Movement object
##'
##' Plots time \eqn{\times} latitude, time \eqn{\times} longitude and longitude \eqn{\times} latitude for a Movement reference class object.
##' @param x Movement reference class object
##' @param ... other parameters to be passed through to plotting functions.
##' @seealso \code{\link{plotMap}}, \code{\link{plotLat}}, and \code{\link{plotLon}}
##' @return Invisibly returns the reference class object
##' @author Christoffer Moesgaard Albertsen
##' @importFrom graphics layout
##' @export
plot.Movement <- function(x,...){
    oldMar <- par("mar")
    args <- as.list(match.call(expand.dots=TRUE))[-1]
    graphics::layout(matrix(c(rep(1,4*3),2,2,3,3,2,2,3,3),nrow=4))
    names(args)[1] <- "object"
    par(mar = c(4.1,4.1,4.1,0.1))
    do.call("plotMap",args)
    par(mar = c(0.1,0.1,4.1,4.1))
    if(is.null(args$plotArgs))
        args$plotArgs <- list()
    args$plotArgs$axes = FALSE
    args$plotArgs$xlab = NA
    args$plotArgs$ylab = NA
    cexbase <- eval(if(match("cex",names(args$plotArgs),nomatch=FALSE)) args$plotArgs$cex else par("cex"))
    cexlab <- eval(if((match("cex.lab",names(args$plotArgs),nomatch=FALSE))) args$plotArgs$cex.lab else par("cex.lab"))
    do.call("plotLat",args)
    axis.POSIXct(side=3, x = x$dates);axis(side=4);box()
    mtext(expression(paste("Latitude (",degree,")",sep="")),side = 4,line = 3, cex = cexlab * cexbase)
    mtext("Date",side = 3,line = 3, cex = cexlab * cexbase)
    par(mar = c(4.1,0.1,0.1,4.1))
    do.call("plotLon",args)
    axis.POSIXct(side=1, x = x$dates);axis(side=4);box()
    mtext(expression(paste("Longitude (",degree,")",sep="")),side = 4,line = 3,
          cex = cexlab * cexbase)
    mtext("Date",side = 1,line = 3, cex = cexlab * cexbase)
    par(mar = oldMar)
    invisible(x)
}


##' Plotting an Observation object
##'
##' Plots time \eqn{\times} latitude, time \eqn{\times} longitude and longitude \eqn{\times} latitude for an Observation reference class object.
##' @param x Observation reference class object
##' @param ... other parameters to be passed through to plotting functions.
##' @seealso \code{\link{plotMap}}, \code{\link{plotLat}}, and \code{\link{plotLon}}
##' @return Invisibly returns the reference class object
##' @author Christoffer Moesgaard Albertsen
##' @importFrom graphics layout
##' @export
plot.Observation <- function(x,...){
    oldMar <- par("mar")
    args <- as.list(match.call(expand.dots=TRUE))[-1]
    graphics::layout(matrix(c(rep(1,4*3),2,2,3,3,2,2,3,3),nrow=4))
    names(args)[1] <- "object"
    par(mar = c(4.1,4.1,4.1,0.1))
    do.call("plotMap",args)
    par(mar = c(0.1,0.1,4.1,4.1))
    if(is.null(args$plotArgs))
        args$plotArgs <- list()
    args$plotArgs$axes = FALSE
    args$plotArgs$xlab = NA
    args$plotArgs$ylab = NA
    cexbase <- eval(if(match("cex",names(args$plotArgs),nomatch=FALSE)) args$plotArgs$cex else par("cex"))
    cexlab <- eval(if((match("cex.lab",names(args$plotArgs),nomatch=FALSE))) args$plotArgs$cex.lab else par("cex.lab"))
    do.call("plotLat",args)
    axis.POSIXct(side=3, x = x$dates);axis(side=4);box()
    mtext(expression(paste("Latitude (",degree,")",sep="")),side = 4,line = 3, cex = cexlab * cexbase)
    mtext("Date",side = 3,line = 3, cex = cexlab * cexbase)
    par(mar = c(4.1,0.1,0.1,4.1))
    do.call("plotLon",args)
    axis.POSIXct(side=1, x = x$dates);axis(side=4);box()
    mtext(expression(paste("Longitude (",degree,")",sep="")),side = 4,line = 3,
          cex = cexlab * cexbase)
    mtext("Date",side = 1,line = 3, cex = cexlab * cexbase)
    par(mar = oldMar)
    invisible(x)
}
