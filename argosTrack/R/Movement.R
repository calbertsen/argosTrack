#' An abstract Reference Class for fitting a Movement model.
#' 
#' Reference class implementing common functions for movement models.
#' @note This reference class should not be used as it does not implement a model. The class is only intended to be a superclass for actual implementations of movement models. A movement model implementation must include an initialization method, a simulation method and (preferably) a getTMBmap method.
#' @seealso Used in \code{\link{Animal}}. Currently the following movement models are implemented: \code{\link{CTCRW}}, \code{\link{DCRW}}, \code{\link{DSBHN}}, \code{\link{DSBW}}, \code{\link{RW}}.
#' 
#' @family "Movement models"
#' 
#' @export Movement
#' @importFrom methods setRefClass new 
#' @exportClass Movement
Movement <- setRefClass("Movement",
                        sealed = TRUE,
                        fields = list(model = "character",
                                      dates = "POSIXct",
                                      parameters = "numeric",
                                      varianceParameters = "numeric",
                                      mu = "matrix",
                                      vel = "matrix",
                                      sdmu = "matrix",
                                      sdvel = "matrix",
                                      nauticalStates = "logical",
                                      vcov = "matrix",
                                      timeunit = "character",
                                      data = "list",
                                      options = "list"),
                        methods = list(
                            copy = function (shallow = FALSE) 
                            {
                                def <- .refClassDef
                                value <- new(def,
                                             dates = .self$dates)
                                ## The rest is from methods::
                                vEnv <- as.environment(value)
                                selfEnv <- as.environment(.self)
                                for (field in names(def@fieldClasses)) {
                                    if (shallow) 
                                        assign(field, get(field, envir = selfEnv), envir = vEnv)
                                    else {
                                        current <- get(field, envir = selfEnv)
                                        ## current is never a ref class
                                        ## if (is(current, "envRefClass")) 
                                        ##     current <- current$copy(FALSE)
                                        assign(field, current, envir = vEnv)
                                    }
                                }
                                value
                            },
                            initialize = function(...){
                                stop("The abstract movement class should not be used")
                            },
                            getTMBdata = function(){
                                "Function to return a data list for TMB::MakeADFun"
                                dat <- list(dtStates =  c(0,
                                                          as.numeric(difftime(tail(.self$dates,-1),
                                                                              head(.self$dates,-1),
                                                                              units=.self$timeunit)
                                                                     )
                                                          ),
                                            moveModelCode = .self$options$moveModelCode,
                                            nauticalStates = as.numeric(.self$nauticalStates)
                                            )
                                return(dat)
                            },
                            getTMBparameters = function(){
                                "Function to return a parameter list for TMB::MakeADFun"
                                pars <- list(movePars = .self$parameters,
                                             logSdState = .self$varianceParameters,
                                             mu = .self$mu,
                                             vel = .self$vel
                                             )
                                return(pars)
                            },
                            getTMBmap = function(...){
                                "Function to return a map list for TMB::MakeADFun."
                                map <- list()
                                return(map)
                            },
                            updateFromFit = function(parList,hessian,sdParList = NULL){
                                "Function to save the result from nlminb."
                                parameters <<- parList$movePars
                                varianceParameters <<- parList$logSdState
                                mu <<- parList$mu
                                vel <<- parList$vel

                                if(!is.null(sdParList)){
                                    sdmu <<- sdParList$mu
                                    sdvel <<- sdParList$vel
                                }else{
                                    sdmu <<- NA * parList$mu
                                    sdvel <<- NA * parList$vel
                                }
                                vcovTmp <- solve(hessian)
                                indx <- which(rownames(vcovTmp) %in% c("movePars",
                                                                       "logSdState")
                                              )
                                vcov <<- vcovTmp[indx,indx]
                            },
                            show = function(){
                                cat("\n\nMovement model:\n")
                                cat("-------------\n\n")
                                cat("Model:",.self$model,"\n")
                                cat("Movement parameters:\n")
                                mm <- .self$parameters
                                names(mm) <- .self$options$parnames
                                print(mm)
                                cat("Movement variance parameters:\n")
                                vv <- .self$varianceParameters
                                names(vv) <- .self$options$varparnames
                                print(vv)
                                cat("Number of latent variables:",dim(.self$mu)[2]+dim(.self$mu)[2],"\n")
                                cat("Using nautical states:",.self$nauticalStates,"\n")
                                cat("Using time unit:",.self$timeunit,"\n")
                            },
                            addToLatPlot = function(arglist = list()){
                                "Function to add estimated movement latitudes to a plot. arglist is a list of base graphics arguments to be used for movement estimates."
                                if(.self$nauticalStates){
                                    ll <- apply(.self$mu,2,function(x).Call("n2ll",x=x[2],y=x[1],PACKAGE="argosTrack"))[2:1,]
                                }else{
                                    ll <- .self$mu
                                }
                                arglist$x = .self$dates
                                arglist$y = ll[1,]
                                do.call("lines",arglist)

                            },
                            addToLonPlot = function(arglist = list()){
                                "Function to add estimated movement longitudes to a plot. arglist is a list of base graphics arguments to be used for movement estimates."
                                if(.self$nauticalStates){
                                    ll <- apply(.self$mu,2,function(x).Call("n2ll",x=x[2],y=x[1],PACKAGE="argosTrack"))[2:1,]
                                }else{
                                    ll <- .self$mu
                                }
                                arglist$x = .self$dates
                                arglist$y = ll[2,]
                                do.call("lines",arglist)

                            },addToLatPlotSd = function(arglist = list(col = "grey",
                                                                       border = NA,
                                                                       lwd = 3,
                                                                       lty = 2)){
                                "Function to add standard errors of estimated movement latitudes to a plot. arglist is a list of base graphics arguments to be used."

                                if(.self$nauticalStates){
                                    lllow <- apply(.self$mu - 2 * .self$sdmu,2,function(x).Call("n2ll",x=x[2],y=x[1],PACKAGE="argosTrack"))[2:1,]
                                    llhigh <- apply(.self$mu + 2 * .self$sdmu,2,function(x).Call("n2ll",x=x[2],y=x[1],PACKAGE="argosTrack"))[2:1,]
                                }else{
                                    lllow <- .self$mu - 2 * .self$sdmu
                                    llhigh <- .self$mu + 2 * .self$sdmu
                                }
                                arglist$x = c(.self$dates,rev(.self$dates))
                                arglist$y = c(lllow[1,],rev(llhigh[1,]))
                                do.call("polygon",arglist)


                            },
                            addToLonPlotSd = function(arglist = list(col = "grey",
                                                                     border = NA,
                                                                     lwd = 3,
                                                                     lty = 2)){
                                "Function to add standard errors of estimated movement longitudes to a plot. arglist is a list of base graphics arguments to be used."
                                if(.self$nauticalStates){
                                    lllow <- apply(.self$mu - 2 * .self$sdmu,2,function(x).Call("n2ll",x=x[2],y=x[1],PACKAGE="argosTrack"))[2:1,]
                                    llhigh <- apply(.self$mu + 2 * .self$sdmu,2,function(x).Call("n2ll",x=x[2],y=x[1],PACKAGE="argosTrack"))[2:1,]
                                }else{
                                    lllow <- .self$mu - 2 * .self$sdmu
                                    llhigh <- .self$mu + 2 * .self$sdmu
                                }
                                arglist$x = c(.self$dates,rev(.self$dates))
                                arglist$y = c(lllow[2,],rev(llhigh[2,]))
                                do.call("polygon",arglist)

                            },
                            addToMapPlot = function(arglist = list()){
                                 "Function to add estimated movement coordinates to a plot. arglist is a list of base graphics arguments to be used."
                                if(.self$nauticalStates){
                                    ll <- apply(.self$mu,2,function(x).Call("n2ll",x=x[2],y=x[1],PACKAGE="argosTrack"))[2:1,]
                                }else{
                                    ll <- .self$mu
                                }
                                arglist$x = ll[2,]
                                arglist$y = ll[1,]
                                do.call("lines",arglist)

                            },
                            getRange = function(sd=FALSE){
                                "Returns the range of estimated coordinates. If sd = TRUE, the range includes standard errors of the movement estimates."
                                vlow <- vhigh <- .self$mu
                                if(any(is.finite(.self$sdmu))){
                                    lllow <- vlow - sd * 2 * .self$sdmu
                                    llhigh <- vhigh + sd * 2 * .self$sdmu
                                }else{
                                    lllow <- vlow
                                    llhigh <- vhigh
                                }
                                
                                if(.self$nauticalStates){
                                    
                                    lllow <- apply(lllow,2,function(x).Call("n2ll",x=x[2],y=x[1],PACKAGE="argosTrack"))[2:1,]
                                    llhigh <- apply(llhigh,2,function(x).Call("n2ll",x=x[2],y=x[1],PACKAGE="argosTrack"))[2:1,]
                                }
                                
                                return(t(apply(cbind(lllow,llhigh),1,range,na.rm=TRUE)))

                            }
                        )
                        )
