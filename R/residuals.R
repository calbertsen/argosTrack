
#' Extract model residuals
#'
#' @export

#residuals <- function(object, ...) UseMethod("residuals",object)


# Implementation for argostrack object

residuals.argostrack <- function(object,type="smooth", ...){
    if(type=="smooth"){
        res <- object$observations - object$positions
        colnames(res) <- object$locationclass
        return(res)
    }else if(type=="onestep"){
        sr <- fitobj$sdreport_summary
        oldmu <- matrix(sr[rownames(sr)=="mu",1],nrow=2)
        oldvel <- matrix(sr[rownames(sr)=="vel",1],nrow=2)
        newdat <- object$tmb_object$env$data
        newpar <- object$tmb_object$env$parList(object$optimization$par)
        newpar$mu <- oldmu
        newpar$vel <- oldvel
        rand <- unique(names(object$tmb_object$env$par[object$tmb_object$env$random]))
        newmap <- object$tmb_object$env$map
        newmap$numdata <- NULL

        obj <- TMB::MakeADFun(newdat,newpar,map=newmap,random=rand,DLL="argosTrack")
        obj$env$inner.control$trace <- FALSE
        obj$env$tracemgc <- FALSE
        
        par0 <- obj$par
        getPars <- function(i){
            pp <- par0
            pp["numdata"] <- i
            pp
        }

        loopvals <- (2:length(newdat$lon))[newdat$dt>0]
        predMu <- array(dim=c(2,length(loopvals)))
        sdevMu <- array(dim=c(2,length(loopvals)))
        for(i in (length(loopvals)-1):1){
            j <- loopvals[i]
            pars <- getPars(j)
            try({
                outp <- capture.output(obj$fn(pars))
                H <- diag(length(par0))*1e100    ## Do not account for fixed effect uncertainty (!)
                outp <- capture.output(sdr <- sdreport(obj, par.fixed=pars, hessian.fixed=H))
                predMu[,i] <- matrix(summary(sdr,"random")[,1],nrow=2)[,i+1]
                sdevMu[,i] <- matrix(summary(sdr,"random")[,2],nrow=2)[,i+1]
            },silent=TRUE)
            if(round(length(loopvals)-1-i,-2)==length(loopvals)-1-i)
                cat(paste("Calculating residual",length(loopvals)-1-i+1,"of",length(loopvals),"...\n"))
        }
        
        return(object$observations -expandMu(predMu,newdat$dt))

    }else{
        error("Unknown residual type. Must be 'smooth' or 'onestep'") 
    }
}
