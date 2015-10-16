##' Simulate a new track from Argos data
##'
##' The function returns a list of lists of simulated values containing:
##' \itemize{
##' \item{velocity}{Matrix of simulated coordinatewise velocities}
##' \item{position}{Matrix of true location coordinates}
##' \item{observation}{Matrix of observed location coordinates}
##' \item{dates}{Dates of observations}
##' \item{locationclass}{Location class of observations}
##' }
##' The simulations are based on a fitted argosTrack object.
##' @title Simulate a new track from Argos data
##' @param object An argostrack object
##' @param nsim Number of simulations. Ignored.
##' @param seed Seed for simulations. Ignored.
##' @param locationclass Use other location classes than the object
##' @param fixstates Use other location classes than the object
##' @param newpar List of new parameter values to use
##' @param ... Not used
##' @return a list of simulated values. See details.
##' @author Christoffer Moesgaard Albertsen
##' @export
##' @importFrom stats simulate
##' 
simulate.argostrack <- function(object,
                                nsim = 1,
                                seed = NULL,
                                locationclass=NULL,
                                fixstates=FALSE,
                                newpar=NULL,
                                ...){
    sample <- residuals(object)
    qualOrig <- object$locationclass
    if(fixstates){
        sts <- object$positions
    }else{
        sts <- simStates(object,newpar)
    }
    numSam <- 1:dim(sample)[2]
    if(is.null(locationclass)){
        qual <- object$locationclass
    }else{
        qual <- locationclass
    }
    res <- array(dim=c(2,length(qual)))
    for(i in 1:dim(res)[2]){
        samnum <- sample(numSam[qualOrig==qual[i]],1)
        err <- sample(c(-1,1),2,replace=TRUE,prob=c(0.5,0.5))*sample[,samnum]
        res[,i] <- sts$mu[,i]+err
    }
    rownames(res) <- c("latitude","longitude")
    rownames(sts$vel) <- c("latitude","longitude")
    rownames(sts$mu) <- c("latitude","longitude")
        
    outp <- list("velocity" = sts$vel,
                 "position" = sts$mu,
                 "observation" = res,
                 "dates" = object$dates,
                 "locationclass" = factor(qual)
                 )
    class(outp) <- "argostrack_simulation"

    return(outp)
}
