#' A Reference Class for specifying a measurement model.
#'
#' @field model Either "n" for multivariate Gaussian measurements, "t" for multivariate t-distributed measurements (Lange et al. 1989), or "sh" for multivariate symmetric hyperbolic distributed measurements of Argos data (Experimental; see Details).
#' @field logSdObs  Vector of scale parameters \eqn{\tau} (See Details)
#' @field corObs Correlation parameter between coordiantes (not implemented yet)
#' @field logCorrection Matrix of scale matrix ratios \eqn{R} (See Details) 
#' @field vcov Covariance matrix of parameter estimates
#' @field splineXlogSd Not used for Argos data
#' @field knotPars Not used for Argos data
#' @field df Degrees of freedom for model == "t"
#' @field minDf Minimum degrees of freedom to allow in the model
#' @field nauticalObs Should the observations be modelled in nautical miles?
#' @field reportSpline Not used for Argos data
#'
#' @references Albertsen, C. M., Whoriskey, K., Yurkowski, D., Nielsen, A., and Flemming, J. M. (2015) Fast fitting of non-Gaussian state-space models to animal movement data via Template Model Builder. Ecology, 96(10), 2598-2604. doi: 10.1890/14-2101.1
#' \cr\cr Barndorff-Nielsen, O. (1977) Exponentially decreasing distributions for the logarithm of particle size. Proc. R. Soc. Lond. A. 353, 401-419.
#' \cr\cr Jonsen, I., J. Mills Flemming, and R. Myers. (2005) Robust state-space modeling of animal movement data. Ecology 86, 2874-2880.
#' \cr\cr Lange, K., Roderick J. A. Little, & Jeremy M. G. Taylor. (1989). Robust Statistical Modeling Using the t Distribution. Journal of the American Statistical Association, 84(408), 881-896.
#' 
#' @details Following Albertsen et al. (2015), the reference class describes the measurement part of the state-space model for the movement data:
#' \deqn{Y_t - X_t \sim \epsilon_t.}
#' Here ,\eqn{Y_t} is the observation, \eqn{X_t} is the state process at the corresponding time and \eqn{epsilon_t} is random noise (either Gaussian, t or symmetric hyperbolic) with scale matrix \eqn{diag(\tau_2 R_{a(t),2},\tau_2 R_{a(t),2})}, where \eqn{a(t)} is the argos location class of the observation.
#' If the time steps of the state process does not correspond to the observation time stamps (e.g. for a discrete time movement model), a linear interpolation between the last state before and the first state after the observation is used as \eqn{X_t} (as Jonsen et al. 2005).
#'
#' The symmetric hyperbolic distribution is a special case of the distribution introduced by Barndorff-Nielsen (1977). The un-normalized density is
#' \deqn{ f(x;\theta,\mu,\Sigma) = exp( - \sqrt{\theta + \theta (x-\mu)\Sigma^{-1} (x-\mu)^T})}
#' where \eqn{x} is a vector of observations, \eqn{\theta} is a scalar parameter, \eqn{\mu} is a vector of location parameters, and \eqn{\Sigma} is a positive definite scale matrix.
#' 
#' @seealso \code{\link{Measurement}} \code{\link{Movement-class}}, \code{\link{Observation-class}}, \code{\link{Animal-class}}
#'
#'
#' @note The reference class methods are not intended to be used by the user. Please use appropriate functions such as \code{\link{simTrack}}.
#' @author Christoffer Moesgaard Albertsen
#' 
#' @importFrom methods setRefClass new
#' @importFrom stats splinefun
#' @exportClass Measurement
setRefClass("Measurement",
            fields = c(model = "character",
                       logSdObs = "numeric",
                       logSdObsExtra = "numeric",
                       corObs = "numeric",
                       logCorrection = "matrix",
                       vcov = "matrix",
                       splineXlogSd = "numeric",
                       knotPars = "numeric",
                       df = "numeric",
                       minDf = "numeric",
                       nauticalObs = "logical",
                       reportSpline = "matrix"
                       ),
            methods = list(

                initialize = function(model = c("n","t","sh"),
                                      logSdObs = c(0,0),
                                      corObs = 0,
                                      logCorrection = matrix(
                                          c(0.6507,0.8231,
                                            2.3432,2.0532,
                                            3.8514,2.9602,
                                            3.8280,3.0463,
                                            4.4417,3.6951,
                                            5.5714,5.5149,
                                            8,8),2,7),
                                      splineXlogSd = 0,
                                      knotPars = numeric(5),
                                      df = numeric(8),
                                      minDf = 3.0,
                                      nauticalObs = FALSE,
                                      ...
                                      ){
                    "Method to initialize the class. Input corresponds to the reference class fields."

###############
                    ## Do checks ##
###############
                    modelIn <- match.arg(model)

                    if(modelIn == "sh")
                        warning("The symmetric hyperbolic is experimental")

                    if(!missing(logSdObs))
                        if(!(is.numvec(logSdObs) && length(logSdObs)==2))
                            stop("logSdObs must be a numeric vector of length 2.")
                    if(!missing(corObs))
                        if(!(is.numsca(corObs)))
                            stop("corObs must be a scalar.")

                    if(!missing(logCorrection))
                        if(!(is.nummat(logCorrection) && dim(logCorrection)[1] == 2 && dim(logCorrection)[2] == 7))
                            stop("logCorrection must be a 2x7 numeric matrix.")
                    if(!missing(splineXlogSd))
                        if(!(is.numsca(splineXlogSd)))
                            stop("splineXlogSd must be a scalar.")

                    if(!missing(knotPars))
                        if(!is.numvec(knotPars))
                            stop("knotPars must be a numeric vector.")
                    if(!missing(df))
                        if(!is.numvec(df) || length(df) != 8)
                            stop("df must be a numeric vector of length 8.")
                    if(!missing(minDf))
                        if(!(is.numsca(minDf) && minDf >= 0))
                            stop("minDf must be a non-negative scalar.")
                    if(!missing(nauticalObs))
                        if(!(length(nauticalObs)==1 && is.logical(nauticalObs)))
                            stop("nauticalObs must be logical.")
                    

                    if(missing(minDf) & modelIn == "sh"){
                        minDfIn <- 0.0
                    }else{
                        minDfIn <- minDf
                    }
################
                    ## initFields ##
################
                    
                    initFields(model = modelIn,
                               logSdObs = logSdObs,
                               logSdObsExtra = 0,
                               corObs = corObs,
                               logCorrection = logCorrection,
                               vcov = diag(Inf,0),
                               splineXlogSd = splineXlogSd,
                               knotPars = knotPars,
                               df = df,
                               minDf = minDfIn,
                               nauticalObs = nauticalObs,
                               reportSpline = matrix(NA,nrow=366,ncol=2)
                               )


                },
                getTMBdata = function(dayOfYearSpline = c()){
                    "Function to return a data list for TMB::MakeADFun. Intended to be called from an Animal reference class object."
                    nKnots <- length(.self$knotPars)
                    dat <- list(minDf = .self$minDf,
                                errorModelCode = as.numeric(factor(.self$model,levels = c('t','n','sh')))-1,
                                nauticalObs = as.numeric(.self$nauticalObs),
                                splineKnots = c(0,
                                                quantile(dayOfYearSpline,
                                                         seq(0,1,len=nKnots))[-c(1,nKnots)],
                                                366)
                                )
                    return(dat)
                },
                getTMBparameters = function(){
                    "Function to return a parameter list for TMB::MakeADFun. Intended to be called from an Animal reference class object."
                    pars <- list(logSdObs = .self$logSdObs,
                                 logSdObsExtra = .self$logSdObsExtra,
                                 logCorrection = .self$logCorrection,
                                 splineXlogSd = .self$splineXlogSd,
                                 knotPars = .self$knotPars,
                                 df = .self$df
                                 )
                    return(pars)
                },
                getTMBmap = function(correlated = FALSE,
                                     useSpline = FALSE,
                                     fixcorrection = FALSE,
                                     usedLevels = c("GPS","3", "2", "1", "0", "A", "B","Z"),
                                     numPerLevel = c(),
                                     needExtraVarPar = FALSE){
                    "Function to return a map list for TMB::MakeADFun. Intended to be called from an Animal reference class object."
                    argosClasses <- c("GPS","3", "2", "1", "0", "A", "B","Z")
                    map <- list()
                    if(.self$model == "n"){
                        map$df <- factor(rep(NA,length(.self$df)))
                    }else{
                        if(length(numPerLevel) > 1){
                            dfMap <- numeric(length(usedLevels))
                            dfMap[1] <- 1
                            for(qq in 2:length(dfMap)){
                                dfMap[qq] <- qq
                                if(numPerLevel[qq] < 20)
                                    dfMap[qq] <- dfMap[qq-1]
                            }
                            if(numPerLevel[1] < 20)
                                dfMap[1] <- dfMap[2]

                            dfMapUse <- rep(NA,length(.self$df))
                            dfMapUse[which(argosClasses %in% usedLevels)] <- dfMap
                            
                            map$df <- factor(dfMapUse)

                        }

                    }
                    if(!needExtraVarPar)
                        map$logSdObsExtra <- factor(NA)
                    
                    if(!useSpline){
                        map$splineXlogSd <- factor(NA)
                        map$knotPars <- factor(rep(NA,length(.self$knotPars)))
                    }else{
                        map$knotPars <- factor(c(1,
                                                 2:(length(.self$knotPars)-1),
                                                 1))
                    }

                    if(fixcorrection || length(numPerLevel[numPerLevel > 0]) == 1){
                        map$logCorrection <- factor(rep(NA,length(.self$logCorrection)))
                    }else{
                        lcm <- matrix(1:14,2,7)
                        colnames(lcm) <- argosClasses[-2]
                        lcm[,which(!(argosClasses[-2] %in% usedLevels))] <- NA
                        
                        lcm[,!is.na(lcm[1,])][,which(numPerLevel[usedLevels %in% argosClasses[-2]] == 0)] <- NA
                        map$logCorrection <- factor(as.vector(lcm))
                    } 
                    
                    return(map)
                },
                updateFromFit = function(parList,
                                         hessian,
                                         reportSpline){
                    "Function to save the result from nlminb."
                    logSdObs <<- parList$logSdObs
                    ## corObs <<- parList$corObs
                    logCorrection <<- parList$logCorrection
                    splineXlogSd <<- parList$splineXlogSd
                    knotPars <<- parList$knotPars
                    df <<- parList$df

                    vcovTmp <- solve(hessian)
                    indx <- which(rownames(vcovTmp) %in% c("logSdObs","logCorrection","splineXlogSd","knotPars","df"))
                    vcov <<- vcovTmp[indx,indx]

                    reportSpline <<- reportSpline

                },
                simulate = function(observation){
                    "Function to simulate measurement data. 'observation' is an Observation reference class object, containing the time points and Argos location classes for the simulation. A matrix containing the measurement data is returned."
                    ## Check input
                    if(class(observation) != "Observation")
                        stop("observation must be an Observation object")
                    ## Matrix to hold simulations
                    Y <- matrix(NA,2,length(observation$dates))
                    ## Set up covariance matrices
                    varObs <- matrix(NA,
                                     nrow = dim(.self$logCorrection)[1],
                                     ncol = dim(.self$logCorrection)[2] + 1)
                    dfObs <- .self$minDf + exp(.self$df)
                    
                    for(i in 1:dim(varObs)[1]){
                        varObs[i,1] <- exp(2 * .self$logSdObs[i])
                        for(j in 2:dim(varObs)[2])
                            varObs[i,j] = exp(2 * (.self$logSdObs[i] + .self$logCorrection[i,j-1]))
                    }
                    covKnown <- diag(0.00001,2)

                    ## Model code:
                    ## 0: argos incl GPS
                    ## 1: Known
                    ## 2: spline
                    indx <- which(observation$varModelCode == 0)
                    for(i in 1:nlevels(observation$qual)){
                        indx2 <- which(as.numeric(observation$qual[indx])==i)
                        if(.self$model == "t"){
                            if(length(indx2) > 0)
                                Y[,indx][,indx2] <- t(rmvt(length(indx2),
                                                           mu = c(0,0),
                                                           sigma = diag(varObs[,i]),
                                                           df = dfObs[i]
                                                           ))

                        }else if(.self$model == "sh"){
                            if(length(indx2) > 0)
                                Y[,indx][,indx2] <- t(rmvsh(length(indx2),
                                                            mu = c(0,0),
                                                            sigma = diag(varObs[,i]),
                                                            delta = dfObs[i]
                                                            ))
                        }else{
                            if(length(indx2) > 0)
                                Y[,indx][,indx2] <- t(rmvnorm(length(indx2),
                                                              mu = c(0,0),
                                                              sigma = diag(varObs[,i])))
                        }
                        ## Implement t and sh
                        
                    }
                    indx <- which(observation$varModelCode == 1)
                    Y[,indx] <- t(rmvnorm(length(indx),
                                          mu = c(0,0),
                                          sigma = covKnown))
                    indx <- which(observation$varModelCode == 2)
                    splineUse <- .self$reportSpline[,1]
                    if(all(is.na(splineUse))){
                        nKnots <- length(.self$knotPars)
                        splineKnots <- c(0,
                                         quantile(observation$dayOfYear,
                                                  seq(0,1,len=nKnots))[-c(1,nKnots)],
                                         366)
                        splineUse <- exp(stats::splinefun(splineKnots,
                                                          .self$knotPars)(0:365+1))

                    }
                    for(i in indx){                                   
                        Y[,i] <- t(rmvnorm(1,
                                           mu = c(0,0),
                                           sigma = diag(c(exp(2*.self$splineXlogSd),
                                                          splineUse[observation$dayOfYear[i]]^2))))
                    }
                    return(Y)
                },
                show = function(){
                    cat("\n\nMeasurement model\n")
                    cat("-----------------\n\n")
                    cat("Measurement distribution:",.self$model,"\n")
                    cat("Use nautical observations:",.self$nauticalObs,"\n")

                    varObs <- matrix(NA,
                                     nrow = dim(.self$logCorrection)[1],
                                     ncol = dim(.self$logCorrection)[2] + 1)
                    rownames(varObs) <- c("Latitude","Longitude")
                    colnames(varObs) <- c("GPS","3", "2", "1", "0", "A", "B","Z")
                    dfObs <- .self$minDf + exp(.self$df)
                    names(dfObs) <- c("GPS","3", "2", "1", "0", "A", "B","Z")
                    
                    for(i in 1:dim(varObs)[1]){
                        varObs[i,1] <- exp(2 * .self$logSdObs[i])
                        for(j in 2:dim(varObs)[2])
                            varObs[i,j] = exp(2 * (.self$logSdObs[i] + .self$logCorrection[i,j-1]))
                    }

                    cat("Variance parameters:\n")
                    print(varObs)
                    if(.self$model == "t"){
                        cat("Degrees of freedom parameters:\n")
                        print(dfObs)
                    }else if(.self$model == "sh"){
                        cat("Delta parameters:\n")
                        print(dfObs)
                    }
                    
                }
                
            )
            )

##' Construct a Measurement object
##'
##' @param model Measurement model distribution
##' @param logSdObs Variance parameters on log-scale
##' @param corObs Correlation parameter
##' @param logCorrection Correction parameters between location classes
##' @param splineXlogSd Longitudinal variance parameter for spline model
##' @param knotPars Latitudinal variance parameters for spline model
##' @param df Degrees of freedom parameters for t distribution
##' @param minDf Minimal degrees of freedom
##' @param nauticalObs Transform observations from longitude/latitude to nautical miles?
##' @param ... Not currently used
##' @return a Measurement object
##' @seealso \code{\link{Measurement-class}} \code{\link{Movement}} \code{\link{Observation}} \code{\link{Animal}}
##' @examples
#' d <- subadult_ringed_seal
#' obs <- Observation(lon = d$lon,
#'                    lat = d$lat,
#'                    dates = as.POSIXct(d$date,tz="GMT"),
#'                    locationclass = d$lc
#'                    )
#' meas <- Measurement(model="t")
#' mov <- RW(unique(obs$dates))
#' anim <- Animal(obs,mov,meas,"Subadult")
#' plot(anim)
##' @author Christoffer Moesgaard Albertsen
##' @export
Measurement <- function(model = c("n","t","sh"),
                        logSdObs = c(0,0),
                        corObs = 0,
                        logCorrection = matrix(
                            c(0.6507,0.8231,
                              2.3432,2.0532,
                              3.8514,2.9602,
                              3.8280,3.0463,
                              4.4417,3.6951,
                              5.5714,5.5149,
                              8,8),2,7),
                        splineXlogSd = 0,
                        knotPars = numeric(5),
                        df = numeric(8),
                        minDf = 3.0,
                        nauticalObs = FALSE,
                        ...){
    methods::new("Measurement",
                 model = model,
                 logSdObs = logSdObs,
                 corObs = corObs,
                 logCorrection = logCorrection,
                 splineXlogSd = splineXlogSd,
                 knotPars = knotPars,
                 df = df,
                 minDf = minDf,
                 nauticalObs = nauticalObs,
                 ...)
}
