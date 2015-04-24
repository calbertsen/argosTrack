# Sample random effects
rmvnorm <- function(n,obj,par=obj$env$par,update=TRUE){
    # From TMB
    updateCholesky <- function(L,H,t=0){
        .Call("destructive_CHM_update",L,H,as.double(t),PACKAGE="Matrix")
    }

    h <- obj$env$spHess(par,random=TRUE)
    L <- obj$env$L.created.by.newton
    if(update)
        updateCholesky(L,h) ## P %*% h %*% Pt = L %*% Lt

    u <- matrix(rnorm(ncol(L)*n),ncol(L),n)
    u <- Matrix::solve(L,u,system="Lt") ## Solve Lt^-1 %*% u
    u <- Matrix::solve(L,u,system="Pt") ## Multiply Pt %*% u
    as.matrix(u)
}






#' Extract model residuals
#'
#' @export

#residuals <- function(object, ...) UseMethod("residuals",object)


# Implementation for argostrack object
# Note: smoothpred, simulated and onestep are not actual residuals
# and only give predictions at the states (may be fewer than observations)

residuals.argostrack <- function(object,type="smooth",seed=1, ...){
    if(type=="smooth"){
        res <- object$observations - object$positions
        colnames(res) <- object$locationclass
        return(res)
    }else if(type=="smoothpred"){
         res <- object$positions[,object$tmb_object$env$data$dt > 0]
         return(res)
    }else if(type=="simulated"){
         set.seed(seed)
         dimPos <- dim(object$positions)
         reNames <- unique(names(object$tmb_object$env$par[object$tmb_object$env$random]))
         estX <- object$sdreport_summary[rownames(object$sdreport_summary)==reNames,1]
         
         Xrn <- rmvnorm(1,object$tmb_object,object$tmb_object$env$last.par.best,FALSE)+estX
         res <- matrix(Xrn[names(estX)=="mu"],nrow=2)
         return(res)
    }else if(type=="onestep"){
        sr <- object$sdreport_summary
        oldmu <- matrix(sr[rownames(sr)=="mu",1],nrow=2)
        oldvel <- matrix(sr[rownames(sr)=="vel",1],nrow=2)
        newdat <- object$tmb_object$env$data
        newpar <- object$tmb_object$env$parList(object$optimization$par)
        if(all(dim(oldmu)==dim(newpar$mu)))
            newpar$mu <- oldmu
        if(all(dim(oldvel)==dim(newpar$vel)))
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

        loopvals <- (1:length(newdat$lon))#[newdat$dt>0]
        predMu <- array(dim=c(2,length(loopvals)))
        sdevMu <- array(dim=c(2,length(loopvals)))
        for(i in (length(loopvals)-1):1){
            j <- loopvals[i]
            pars <- getPars(j)
            try({
                outp <- capture.output(obj$fn(pars))
                ## Do not account for fixed effect uncertainty (!)
                H <- diag(length(par0))*1e100    
                outp <- capture.output(sdr <- TMB::summary.sdreport(TMB::sdreport(obj, par.fixed=pars, hessian.fixed=H)))
                predMu[,i] <- sdr[rownames(sdr)=="test",1] #matrix(summary(sdr,"random")[,1],nrow=2)[,i+1]
                sdevMu[,i] <- sdr[rownames(sdr)=="test",2] #matrix(summary(sdr,"random")[,2],nrow=2)[,i+1]
            },silent=TRUE)
            if(round(length(loopvals)-1-i,-2)==length(loopvals)-1-i)
                cat(paste("Calculating residual",length(loopvals)-1-i+1,"of",length(loopvals),"...\n"))
        }

        res <- list(predMu,sdevMu) #(object$observations-expandMu(predMu,newdat$dt))/expandMu(sdevMu,newdat$dt)
        return(res)

    }else{
        stop("Unknown residual type. Must be 'smooth' or 'onestep'") 
    }
}
