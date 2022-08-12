#' A Reference Class for fitting an Ornstein-Uhlenbeck with foraging.
#'
#' The reference class implements an Ornstein-Uhlenbeck with foraging (Flemming et al. 2014). 
#'
#' @seealso \link{Movement-class} \link{OUF}
#'
#' @family "Movement models"
#'
#' @references
#' Flemming et al.
#' 
#' @importFrom methods setRefClass new 
#' @exportClass OUF
setRefClass("OUF",
                  contains = "Movement",
                  methods = list(
                      copy = function (shallow = FALSE) 
                      {
                          value <- callSuper(shallow = shallow)
                          value
                      },
                      initialize = function(dates,
                                            pars = numeric(6),
                                            varPars = numeric(2),
                                            nauticalStates = FALSE,
                                            timeunit = "hours"
                                            ){
                          "Method to initialize the class. 'dates' is a vector of distinct and increasing POSIXct dates; 'pars' is vector of the movement parameters: \\eqn{log(\\beta_{H,lat})}, \\eqn{log(\\beta_{H,lon})}, \\eqn{log(\\beta_{F,lat})}, \\eqn{log(\\beta_{F,lon})}, \\eqn{\\gamma_{lat}}, \\eqn{\\gamma_{lon}}; 'varPars' is a vector of movement variance parameters: \\eqn{log(\\sigma_{lat})}, \\eqn{log(\\sigma_{lon})}; 'nauticalStates' is a logical value indicating whether the states should be modelled in nautical miles, and 'timeunit' is the time unit to use for calculating time steps."
###############
## Do checks ##
###############
                          if(!is.POSIXct(dates))
                              stop("dates must be a POSIXct class.")
                          if(any(diff(dates) <= 0))
                              stop("dates must be sorted and different")
                          if(!(length(pars)==6 && is.numvec(pars)))
                              stop("pars must be a numeric vector of length 6.")
                          if(!(length(varPars)==2 && is.numvec(varPars)))
                              stop("varPars must be a numeric vector of length 2.")
                          if(!(length(nauticalStates)==1 && is.logical(nauticalStates)))
                              stop("nauticalStates must be logical.")
                          if(!(length(timeunit == 1) &&
                                      timeunit %in% c("auto", "secs", "mins", 
                                                      "hours", "days", "weeks")))
                              stop("timeunit must be one of: 'auto', 'secs', 'mins', 'hours', 'days', 'weeks'.")

################
## initFields ##
################
                          
                          initFields(model = "Ornstein-Uhlenbeck with Foraging (OUF)",
                                     dates = dates,
                                     parameters = pars,
                                     varianceParameters = varPars,
                                     mu = matrix(0,2,length(dates)),
                                     vel = matrix(0,2,length(dates)),
                                     sdmu = matrix(NA,2,length(dates)),
                                     sdvel = matrix(NA,2,length(dates)),
                                     nauticalStates = nauticalStates,
                                     vcov = diag(Inf,0),
                                     timeunit = timeunit,
                                     data = list(),
                                     options = list(moveModelCode = 11,
                                                    parnames = c("log(beta_H,lat)","log(beta_H,lon)","log(beta_F,lat)","log(beta_F,lon)","gamma_lat","gamma_lon"),
                                                    varparnames = c("log(sigma_lat)","log(sigma_lon)"))
                                     )


                      },
                      simulate = function(x0 = c(0,0,0,0)){
                          "Function to simulate from the movement model. The initial states (latitudianl velocity, longitudinal velocity, latitudinal/y-coordinate location and longitudinal/x-coordinate location) must be given. If nauticalStates==TRUE, the result is returned in nautical miles."

                          ## TODO: Handle nautical states better
                          beta <- exp(.self$parameters[1:4])
                          gamma <- .self$parameters[5:6]
                          varState <- exp(2.0 * .self$varianceParameters)
                          dt <- c(1,as.numeric(difftime(tail(.self$dates,-1),
                                                        head(.self$dates,-1),
                                                        units = .self$timeunit)))
                          
                                      
                          X <- matrix(NA,4,length(.self$dates))
                          X[,1] <- x0[c(1,3,2,4)]
                          for(i in 2:length(.self$dates)){

                              ## Create covariance matrix
                              cov <- matrix(0,4,4)
                              vtmp <- varState[1:2]*(1.0-exp(-2.0*beta[3:4]*dt[i]))/(2.0*beta[3:4])
                              cov[1,1] <- 0.001 * vtmp[1]
                              cov[2,2] <- 0.001 * vtmp[2]
                              cov[3,3] <- 0.999 * vtmp[1]
                              cov[4,4] <- 0.999 * vtmp[2]


                              ## Create mean vector
                              state <- rep(NA,4)
                              state[1:2] <- X[1:2,i-1] + ( - beta[1:2] * (X[1:2,i-1] - gamma[1:2]) + X[3:4,i-1]) * dt[i]
                              state[3:4] <- X[3:4,i-1] * exp(-beta[3:4] * dt[i])

                              
                              X[,i] <- rmvnorm(1,
                                               mu = state,
                                               sigma = cov)
                          }
                          ## Return positions
                          return(X)
                      },
                      getTMBmap = function(...){
                          "Function to return a map list for TMB::MakeADFun. If equaldecay=TRUE, \\eqn{\\beta_1} and \\eqn{\\beta_2} are estimated as equal. If equaldrift=TRUE, \\eqn{\\gamma_1} and \\eqn{\\gamma_2} are estimated as equal. If fixdrift=TRUE, \\eqn{\\gamma_1} and \\eqn{\\gamma_2} are fixed at the current value."
                          map <- callSuper(...)
                          args <- list(...)
                          mpar <- 1:4
                          doit <- FALSE

                          if("equaldecay" %in% names(args))                       
                              if(args$equaldecay){
                                  mpar[1:2] <- 1;
                                  mpar[3:4] <- 3;
                                  doit <- TRUE
                              }
                          if("equaldrift" %in% names(args))
                              if(args$equaldrift){
                                  mpar[4:5] <- 4;
                                  doit <- TRUE
                              }
                          if("fixdrift" %in% names(args))
                              if(args$fixdrift){
                                  mpar[4:5] <- NA;
                                  doit <- TRUE
                              }

                          if("equalvar" %in% names(args)) ## var
                              if(args$equalvar){
                                  map$logSdState <- factor(c(1,1))
                              }
                          

                          if(doit)
                              map$movePars <- factor(mpar)

                          return(map)
                          
                      }

                  
                  )
                  )


##' Create an OUF movement model object
##'
##' @param dates Vector of distinct and increasing POSIXct dates
##' @param pars Vector of movement parameters: \\eqn{log(\\beta_{lat})}, \\eqn{log(\\beta_{lon})}, \\eqn{\\gamma_{lat}}, \\eqn{\\gamma_{lon}}
##' @param varPars Vector of movement variance parameters: \\eqn{log(\\sigma_{lat})}, \\eqn{log(\\sigma_{lon})}
##' @param nauticalStates Should latent states be transformed from longitude/latitude to nautical miles?
##' @param timeunit timeunit used for calculating time steps.
##' @return An OUF object
##' @seealso \code{\link{OUF-class}}
#' @examples
#' d <- subadult_ringed_seal
#' mov <- OUF(unique(as.POSIXct(d$date,tz="GMT")))
##' @author Christoffer Moesgaard Albertsen
##' @export
OUF <- function(dates,
                  pars = numeric(6),
                  varPars = numeric(2),
                  nauticalStates = FALSE,
                  timeunit = "hours"){
    new("OUF",
        dates = dates,
        pars = pars,
        varPars = varPars,
        nauticalStates = nauticalStates,
        timeunit = timeunit)
}
