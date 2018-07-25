##' Plotting an Animal object
##'
##' Plots time \eqn{\times} latitude, time \eqn{\times} longitude and longitude \eqn{\times} latitude for an Animal reference class object.
##' @param x Animal reference class object
##' @param ... other parameters to be passed through to plotting functions.
##' @seealso \code{\link{plotMap}}, \code{\link{plotLat}}, and \code{\link{plotLon}}
##' @author Christoffer Moesgaard Albertsen
##' @importFrom graphics layout
##' @export
plot.Animal <- function(x,...){
              args <- as.list(match.call(expand.dots=TRUE))[-1]
              graphics::layout(matrix(c(rep(1,4*3),2,2,3,3,2,2,3,3),nrow=4))
              names(args)[1] <- "object"
              do.call("plotMap",args)
              do.call("plotLat",args)
              do.call("plotLon",args)
}

##' Plotting a Movement object
##'
##' Plots time \eqn{\times} latitude, time \eqn{\times} longitude and longitude \eqn{\times} latitude for a Movement reference class object.
##' @param x Movement reference class object
##' @param ... other parameters to be passed through to plotting functions.
##' @seealso \code{\link{plotMap}}, \code{\link{plotLat}}, and \code{\link{plotLon}}
##' @author Christoffer Moesgaard Albertsen
##' @importFrom graphics layout
##' @export
plot.Movement <- function(x,...){
              args <- as.list(match.call(expand.dots=TRUE))[-1]
              graphics::layout(matrix(c(rep(1,4*3),2,2,3,3,2,2,3,3),nrow=4))
              names(args)[1] <- "object"
              do.call("plotMap",args)
              do.call("plotLat",args)
              do.call("plotLon",args)
}


##' Plotting an Observation object
##'
##' Plots time \eqn{\times} latitude, time \eqn{\times} longitude and longitude \eqn{\times} latitude for an Observation reference class object.
##' @param x Observation reference class object
##' @param ... other parameters to be passed through to plotting functions.
##' @seealso \code{\link{plotMap}}, \code{\link{plotLat}}, and \code{\link{plotLon}}
##' @author Christoffer Moesgaard Albertsen
##' @importFrom graphics layout
##' @export
plot.Observation <- function(x,...){
              args <- as.list(match.call(expand.dots=TRUE))[-1]
              graphics::layout(matrix(c(rep(1,4*3),2,2,3,3,2,2,3,3),nrow=4))
              names(args)[1] <- "object"
              do.call("plotMap",args)
              do.call("plotLat",args)
              do.call("plotLon",args)
}
