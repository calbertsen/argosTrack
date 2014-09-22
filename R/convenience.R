

#' Expand estimated states to all time points
#' @param eststat 2xn Matrix of estimated states
#' @param dt vector of length m>=n of time difference to previous observation

expandMu <- function(eststat,dt){
res<-array(dim=c(2,length(dt)))
    st <- 1
    for(i in 1:length(dt)){
        if(dt[i]>0 && i>1){
            st <- st+1
        }
        res[1,i] <- eststat[1,st]
        res[2,i] <- eststat[2,st]
    }
    return(res)
}

#' Simulate states given an argostrack object
#' @param object Argostrack object
#' @param locationclass The location classes of the track

simStates <- function(object,newpar=NULL){
    if(!class(object)=="argostrack"){
        stop("Object must be of class argostrack")
    }
    if(is.null(newpar)){
        parUse <- object$optimization$par
    }else{
        if(length(newpar)!=length(object$optimization$par)){
            stop("Parameter vector must be the same length as the parameter vector from the TMB object.")
        }
        parUse <- newpar
    }
    parl <- object$tmb_object$env$parList(parUse)
    beta <- exp(parl$logbeta)
    logSdState <- parl$logSdState
    gamma <- parl$gamma
    dt <- object$tmb_object$env$data$dt
    mu0 <- parl$mu[,1]
    vel0 <- parl$vel[,1]
    
    varState <- exp(2.0*logSdState)
    mu <- array(dim=c(2,length(dt)))
    vel <- array(dim=c(2,length(dt)))

    mu[,1] <- mu0
    vel[,1] <- vel0

    for(i in 2:length(dt)){
        if(dt[i]>0){
            mm <- c(mu[1,i-1]+vel[1,i-1]*(1.0-exp(-beta[1]*dt[i]))/beta[1],
                    gamma[1]+exp(-beta[1]*dt[i])*(vel[1,i-1]-gamma[1]),
                    mu[2,i-1]+vel[2,i-1]*(1.0-exp(-beta[2]*dt[i]))/beta[2],
                    gamma[2]+exp(-beta[2]*dt[i])*(vel[2,i-1]-gamma[2])
                    )
            cc <- matrix(0,ncol=4,nrow=4)
            cc[1,1] <- varState[1]/beta[1]^2*(dt[i]-2*(1-exp(-beta[1]*dt[i]))/beta[1]+(1-exp(-2*beta[1]*dt[i]))/(2*beta[1]))
            cc[3,3] <- varState[2]/beta[2]^2*(dt[i]-2*(1-exp(-beta[2]*dt[i]))/beta[2]+(1-exp(-2*beta[2]*dt[i]))/(2*beta[2]))
            cc[2,2] <- varState[1]*(1-exp(-2*beta[1]*dt[i]))/(2*beta[1])
            cc[4,4] <- varState[2]*(1-exp(-2*beta[2]*dt[i]))/(2*beta[2])
            cc[1,2] <- varState[1]*(1-2*exp(-beta[1]*dt[i])+exp(-2*beta[1]*dt[i]))/(2*beta[1]^2)
            cc[3,4] <- varState[2]*(1-2*exp(-beta[2]*dt[i])+exp(-2*beta[2]*dt[i]))/(2*beta[2]^2)
            cc[2,1] <- cc[1,2]
            cc[4,3] <- cc[3,4]
            rr <- as.vector(mvtnorm::rmvnorm(1,mm,cc))
            mu[,i] <- rr[c(1,3)]
            vel[,i] <- rr[c(2,4)]                    
          
        }else{
            mu[,i] <- mu[,i-1]
            vel[,i] <- vel[,i-1] 
        }
    }
    return(list(mu=mu,vel=vel))
}
