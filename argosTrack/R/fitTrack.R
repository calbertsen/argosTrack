
##' Generic function to fit movement models.
##'
##' Generic function to fit movement models.
##' @param object The object to be fitted
##' @param ... Additional arguments passed to the appropriate method
##' @seealso \code{\link{fitTrack,Animal-method}}
#' @export
setGeneric("fitTrack",
  function(object, ...)
    standardGeneric("fitTrack")
  )


##' Fits a state-space movement model to animal data
##'
##' Fits the state-space movement determined by object using maximum likelihood estimation with the Laplace approximation.
##' @param object \link{Animal} reference class object
##' @param method Estimation method. Either laplace for maximum likelihood estimation with the Laplace approximation or mcmc for Markov Chain Monte Carlo (not yet implemented).
##' @param silent Disable all tracing information?
##' @param fixcorrection Should the ratio between measurement errors be fixed?
##' @param nlminb.control List of control arguments passed to 'nlminb'
##' @param inner.control List of control arguments passed to 'TMB::newton'
##' @param ... Additional arguments passed to object$getTMBmap
##' @return The result returned from 'nlminb' is returned invisibly.
##' @seealso \link[TMB]{MakeADFun} \link[TMB]{newton} \link[stats]{nlminb}
##' @note The function changes 'object'.
##' @author Christoffer Moesgaard Albertsen
##'  @seealso \code{\link{fitTrack}}
##' @importFrom stats nlminb optimHess
setMethod("fitTrack", "Animal",
          function(object,
                   method = c("laplace","mcmc"),
                   silent = FALSE,
                   fixcorrection = FALSE,
                   nlminb.control = list(),
                   inner.control = list(maxit = 1000),
                   ...){
              requireNamespace("TMB",quietly=TRUE)
              method <- match.arg(method)

###############
## Do checks ##
###############
              if(method == "mcmc") stop("Not yet implemented")
              
              dat <- object$getTMBdata()
              par <- object$getTMBparameters()
              map <- object$getTMBmap(fixcorrection = fixcorrection, ...)
              obj <- TMB::MakeADFun(data = dat,
                                    parameters = par,
                                    map = map,
                                    random = c("mu","vel"),
                                    DLL = "argosTrack",
                                    inner.control = inner.control,
                                    checkParameterOrder=FALSE,
                                    silent = silent)

              esttime <- system.time(opt <- stats::nlminb(obj$par,obj$fn,obj$gr, control = nlminb.control))
              opt$estimation_time <- esttime
              opt$gr <- obj$gr(opt$par)
              opt$hessian <- stats::optimHess(opt$par,obj$fn,obj$gr)
              rep <- obj$report(par = obj$env$last.par.best)
              parList <- obj$env$parList(par = obj$env$last.par.best)
              sdr <- TMB::sdreport(obj,
                                   par.fixed = opt$par,
                                   hessian.fixed = opt$hessian)
              sdParList <- obj$env$parList(par = summary(sdr,c("fixed","random"))[,2])
              ssdr <- summary(sdr)
              ## Update Movement
              object$movement$updateFromFit(parList,opt$hessian,sdParList)
              ## Update Measurement
              object$measurement$updateFromFit(parList,opt$hessian,ssdr[rownames(ssdr)=="splineSd",])
              ## Update Animal
              object$updateFromFit(opt)
              class(opt) <- "fittedTrack"
              invisible(opt)
          }
)
