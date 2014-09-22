
#' Simulate a new track from Argos data
#' @param object An argostrack object
#' @param locationclass Use other location classes than the object
#' @param fixstates Use the states from object

#'@export

simulate.argostrack <- function(object,locationclass=NULL,fixstates=FALSE,newpar=NULL){
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

    return(outp)
}
