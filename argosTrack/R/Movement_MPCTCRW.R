#' A Reference Class for fitting a Multi-Persistence Continuous Time Correlated Random Walk model.
#'
#' This movement model extends \code{\link{CTCRW}} by having two velocity processes; one with short memory and one with longer memory.
#' 
#' @seealso \link{Movement-class}
#'
#' @family "Movement models"
#' 
#' @examples
#' d <- subadult_ringed_seal
#' #mov <- argosTrack:::MPCTCRW(unique(as.POSIXct(d$date,tz="GMT")))
#'
#' 
#' @importFrom methods setRefClass new
#' @keywords internal
# nocov start
setRefClass("MPCTCRW",
                  contains = "Movement",
                  methods = list(
                      initialize = function(dates,
                                            pars = numeric(8),
                                            varPars = numeric(4),
                                            nauticalStates = FALSE,
                                            timeunit = "hours"
                                            ){
                          "Method to initialize the class. 'dates' is a vector of distinct and increasing POSIXct dates; 'pars' is vector of the movement parameters: \\eqn{log(\\beta_{1,lat})}, \\eqn{log(\\beta_{1,lon})}, \\eqn{log(\\beta_{2,lat})}, \\eqn{log(\\beta_{2,lon})}, \\eqn{\\gamma_{1,lat}}, \\eqn{\\gamma_{1,lon}}, \\eqn{\\gamma_{2,lat}}, \\eqn{\\gamma_{2,lon}}; 'varPars' is a vector of movement variance parameters: \\eqn{log(\\sigma_{1,lat})}, \\eqn{log(\\sigma_{1,lon})}, \\eqn{log(\\sigma_{2,lat})}, \\eqn{log(\\sigma_{2,lon})}; 'nauticalStates' is a logical value indicating whether the states should be modelled in nautical miles, and 'timeunit' is the time unit to use for calculating time steps."
###############
## Do checks ##
###############
                          if(!is.POSIXct(dates))
                              stop("dates must be a POSIXct class.")
                          if(any(diff(dates) <= 0))
                              stop("dates must be sorted and different")
                          if(!(length(pars)==8 && is.numvec(pars)))
                              stop("pars must be a numeric vector of length 8.")
                          if(!(length(varPars)==4 && is.numvec(varPars)))
                              stop("varPars must be a numeric vector of length 4.")
                          if(!(length(nauticalStates)==1 && is.logical(nauticalStates)))
                              stop("nauticalStates must be logical.")
                          if(!(length(timeunit == 1) &&
                                      timeunit %in% c("auto", "secs", "mins", 
                                                      "hours", "days", "weeks")))
                              stop("timeunit must be one of: 'auto', 'secs', 'mins', 'hours', 'days', 'weeks'.")

################
## initFields ##
################
                          
                          initFields(model = "Multi-Persistence Continuous Time Correlated Random Walk (CTCRW)",
                                     dates = dates,
                                     parameters = pars,
                                     varianceParameters = varPars,
                                     mu = matrix(0,2,length(dates)),
                                     vel = matrix(0,4,length(dates)),
                                     sdmu = matrix(NA,2,length(dates)),
                                     sdvel = matrix(NA,4,length(dates)),
                                     nauticalStates = nauticalStates,
                                     vcov = diag(Inf,0),
                                     timeunit = timeunit,
                                     data = list(),
                                     options = list(moveModelCode = 2,
                                                    parnames = c("log(beta_{1,lat})","log(beta_{1,lon})","log(beta_{2,lat}-beta_{1,lat})","log(beta_{2,lon}-beta_{1,lon})","gamma_{1,lat}","gamma_{1,lon}","gamma_{2,lat}","gamma_{2,lon}"),
                                                    varparnames = c("log(sigma_{1,lat})","log(sigma_{1,lon})","log(sigma_{2,lat})","log(sigma_{2,lon})"))
                                     )


                      },
                      simulate = function(x0 = c(0,0,0,0,0,0)){
                          "Function to simulate from the movement model. The initial states (latitudianl velocity 1, longitudinal velocity 1, latitudianl velocity 2, longitudinal velocity 2, latitudinal/y-coordinate location and longitudinal/x-coordinate location) must be given. The function only returns the locations. If nauticalStates==TRUE, the result is returned in nautical miles."
                          beta <- exp(.self$parameters[1:4])
                          beta[3:4] <- beta[3:4] + beta[1:2]
                          gamma <- .self$parameters[5:8]
                          varState <- exp(2.0 * .self$varianceParameters)
                          dt <- c(1,as.numeric(difftime(tail(.self$dates,-1),
                                                        head(.self$dates,-1),
                                                        units = .self$timeunit)))
                          
                                      
                          X <- matrix(NA,6,length(.self$dates))
                          X[,1] <- x0
                          X[,2] <- x0
                          for(i in 3:length(.self$dates)){

                              ## Create covariance matrix
                              cov <- matrix(0,6,6)
                              cov[1,1] <- 0.001 #varState[1]/beta[1]^2.0*(dt[i]-2.0*(1.0-exp(-beta[1]*dt[i]))/beta[1]+(1.0-exp(-2.0*beta[1]*dt[i]))/(2.0*beta[1])) + varState[3]/beta[3]^2.0*(dt[i]-2.0*(1.0-exp(-beta[3]*dt[i]))/beta[3]+(1.0-exp(-2.0*beta[3]*dt[i]))/(2.0*beta[3]))
                              cov[2,2] <- varState[1]*(1.0-exp(-2.0*beta[1]*dt[i]))/(2*beta[1])
                              cov[3,3] <- varState[3]*(1.0-exp(-2.0*beta[3]*dt[i]))/(2*beta[3])
                              cov[2,1] <- 0 #varState[1]*(1.0-2.0*exp(-beta[1]*dt[i])+exp(-2.0*beta[1]*dt[i]))/(2.0*beta[1]^2.0)
                              cov[1,2] <- cov[2,1]
                              cov[3,1] <- 0 #varState[3]*(1.0-2.0*exp(-beta[3]*dt[i])+exp(-2.0*beta[3]*dt[i]))/(2.0*beta[3]^2.0)
                              cov[1,3] <- cov[3,1]
                              cov[3,2] <- 0.0
                              cov[2,3] <- cov[3,2]
                              
                              cov[4,4] <- 0.001 #varState[2]/beta[2]^2.0*(dt[i]-2.0*(1.0-exp(-beta[2]*dt[i]))/beta[2]+(1.0-exp(-2.0*beta[2]*dt[i]))/(2.0*beta[2])) + varState[4]/beta[4]^2.0*(dt[i]-2.0*(1.0-exp(-beta[4]*dt[i]))/beta[4]+(1.0-exp(-2.0*beta[4]*dt[i]))/(2.0*beta[4]))
                              cov[5,5] <- varState[2]*(1.0-exp(-2.0*beta[2]*dt[i]))/(2.0*beta[2])
                              cov[6,6] <- varState[4]*(1.0-exp(-2.0*beta[4]*dt[i]))/(2.0*beta[4])
                              cov[4,5] <- 0 #varState[2]*(1.0-2.0*exp(-beta[2]*dt[i])+exp(-2.0*beta[2]*dt[i]))/(2.0*beta[2]^2.0)
                              cov[5,4] <- cov[4,5]
                              cov[4,6] <- 0 #varState[4]*(1.0-2.0*exp(-beta[4]*dt[i])+exp(-2.0*beta[4]*dt[i]))/(2.0*beta[4]^2.0)
                              cov[6,4] <- cov[4,6]
                              cov[5,6] <- 0.0
                              cov[6,5] <- cov[5,6]


                              ## Create mean vector
                              state <- rep(NA,4)
                              mutm <- X[c(1,4),i-1]
                              mutmm <- X[c(1,4),i-2]
                              veltm <- X[c(2,5,3,6),i-1]
                              veltmm <- X[c(2,5,3,6),i-2]
                              
                              state[1] <- mutm[1]+veltm[1]*(1.0-exp(-beta[1]*dt[i]))/beta[1]+veltm[3]*(1.0-exp(-beta[3]*dt[i]))/beta[3];
                              state[2] <- gamma[1]+exp(-beta[1]*dt[i])*(veltm[1]-gamma[1]);
                              state[3] <- gamma[3]+exp(-beta[3]*dt[i])*(veltm[3]-gamma[3]);

                              state[4] <- mutm[2]+veltm[2]*(1.0-exp(-beta[2]*dt[i]))/beta[2]+veltm[3]*(1.0-exp(-beta[4]*dt[i]))/beta[4];
                              state[5] <- gamma[2]+exp(-beta[2]*dt[i])*(veltm[2]-gamma[2]);
                              state[6] <- gamma[4]+exp(-beta[4]*dt[i])*(veltm[4]-gamma[4]);
                              
                              X[,i] <- rmvnorm(1,
                                               mu = state,
                                               sigma = cov)
                          }
                          ## Return positions
                          return(X[c(1,4),])
                      },
                      getTMBmap = function(...){
                          "Function to return a map list for TMB::MakeADFun. If equaldecay=TRUE, \\eqn{\\beta_1} and \\eqn{\\beta_2} are estimated as equal in each velocity process. If equaldrift=TRUE, \\eqn{\\gamma_1} and \\eqn{\\gamma_2} are estimated as equal in each velocity process. If fixdrift=TRUE, \\eqn{\\gamma_1} and \\eqn{\\gamma_2} are fixed at the current value in each velocity process."
                          map <- callSuper(...)
                          args <- list(...)
                          mpar <- 1:8
                          doit <- FALSE

                          if("equaldecay" %in% names(args))                       
                              if(args$equaldecay){
                                  mpar[1:2] <- 1;
                                  mpar[3:4] <- 2;
                                  doit <- TRUE
                              }
                          if("equaldrift" %in% names(args))
                              if(args$equaldrift){
                                  mpar[5:6] <- 3;
                                  doit <- TRUE
                              }
                          if("fixdrift" %in% names(args))
                              if(args$fixdrift){
                                  mpar[5:6] <- NA;
                                  doit <- TRUE
                              }
                          mpar[7:8] <- NA;

                          map$movePars <- factor(mpar)

                          return(map)
                          
                      }

                  
                  )
                  )
# nocov end
