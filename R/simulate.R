
#' Simulate a new track from Argos data
#' @param object An argostrack object

#'@export

simulate.argostrack <- function(object){
    sample <- residuals(object)
    sts <- simStates(object)
    numSam <- 1:dim(sample)[2]
    qual <- object$locationclass
    res <- array(dim=c(2,length(qual)))
    for(i in 1:dim(res)[2]){
        samnum <- sample(numSam[qual==qual[i]],1)
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
                 "locationclass" = object$locationclass
                 )

    return(outp)
}
