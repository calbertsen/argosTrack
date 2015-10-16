
##' Residuals for argosTrack object
##'
##' The function calculated residuals for an argosTrack object.
##' There are currently two types of residuals:
##' \itemize{
##' \item{smooth}{Gives smoothed residuals, i.e., the observations minus the smoothed estimated states}
##' \item{oneste}{Gives one step ahead prediction residuals calculated by the \pkg{TMB} function \code{oneStepPredict}}
##' }
##' @section Note:
##' The one step ahead residuals are calculated and standardize coordinatewise. Hence, they need not be independent between coordinates.
##' 
##' @title Residuals for argosTrack object
##' @param object argosTrack object
##' @param type Type of residual (see details)
##' @param seed Seed for simulations.
##' @param parallel Should one step predictions be calculated in parallel?
##' @param osamethod Method for calculating one step predictions. Default to \code{oneStepGaussianOffMode} for Gaussian models and \code{oneStepGeneric} for t models.
##' @param conditional Subset of observations to condition on.
##' @param first First observation to calculate residuals for.
##' @param ... Passed to nowhere
##' @return Matrix of latitude and longitude residuals.
##' @author Christoffer Moesgaard Albertsen
##' @export
residuals.argostrack <- function(object,
                                 type=c("smooth", "onestep"),
                                 seed=1,
                                 parallel=FALSE,
                                 osamethod=NULL,
                                 conditional = 1,
                                 first = 2,
                                 ...){
    requireNamespace("TMB")
    type <- match.arg(type)
    if(type == "smooth"){
        res <- object$observations - object$positions
        colnames(res) <- object$locationclass
        return(res)
    ## }else if(type == "smoothpred"){
    ##      res <- object$positions[,object$tmb_object$env$data$dt > 0]
    ##      return(res)
    ## }else if(type == "simulated"){
    ##      set.seed(seed)
    ##      dimPos <- dim(object$positions)
    ##      reNames <- unique(names(object$tmb_object$env$par[object$tmb_object$env$random]))
    ##      estX <- object$sdreport_summary[rownames(object$sdreport_summary) %in% reNames,1]
         
    ##      Xrn <- rmvnorm(1,object$tmb_object,object$tmb_object$env$last.par.best,FALSE) + estX
    ##      res <- matrix(Xrn[names(estX) == "mu"],nrow=2)
    ##      return(res)
    }else if(type == "onestep"){

        if(is.null(osamethod)){
            method <- ifelse(object$errordistribution == "n",
                             "oneStepGaussianOffMode","oneStepGeneric")
        }else{
            method <- osamethod
        }

        osa_lat <- TMB::oneStepPredict(object$tmb_object,"lat","klat",
                                   discrete = FALSE,
                                   conditional = conditional,
                                   seed = seed,
                                   subset = (first:dim(object$observations)[2]),
                                   method=method, parallel=parallel)
        osa_lon <- TMB::oneStepPredict(object$tmb_object,"lon","klon",
                                   discrete = FALSE,
                                   conditional = conditional,
                                   seed = seed,
                                   subset = (first:dim(object$observations)[2]),
                                   method=method, parallel=parallel)

        nas <- matrix(NA,ncol=first-1,nrow=2)
        res <- cbind(nas,rbind(osa_lat$residual,osa_lon$residual))
 
        rownames(res) <- c("latitude","longitude")
        colnames(res) <- object$locationclass
        class(res) <- "argostrack_residual"
        return(res)
        
    }else{
        stop("Unknown residual type. Must be 'smooth' or 'onestep'") 
    }
}
