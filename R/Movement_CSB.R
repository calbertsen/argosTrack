#' A Reference Class for fitting a Continuous Time Step lengths / Bearings model.
#'
#' The reference class implements a Continuous Time Step lengths / Bearings model (Parton et al. 2016). The bearings are modelled by a Brownian Motion,
#' \deqn{B_{t} = B_{t-1}+\eta_{t},}
#' and the step lengths are modelled by an Ornstein-Uhlenbeck model,
#' \deqn{S_{t} = \mu + e^{-\beta_c\Delta_t}(S_{t-\Delta_t} - \mu) + \zeta_t.}
#' Here, \eqn{\Delta_t} is the length of the time steps and \eqn{\eta_{t}}, \eqn{\zeta_{t}} are random variables.
#' \eqn{\eta_{c,t}} follows a Gaussian distribution with mean zero and variance \eqn{V\left(\eta_{c,t}\right) = \sigma_c^2(1-e^{-2\beta_c\Delta_t})/(2\beta_c)}. The \eqn{\eta}'s are independent in time and coordiantes.
#' \eqn{\zeta_{t}} follows a Gaussian distribution with mean zero and variance
#' \deqn{V\left(\zeta_{t}\right)=\frac{\sigma_S^2}{2\beta}\left(1 - e^{2\beta\Delta_t}\right).}
#' \eqn{\eta_t} follows a Gaussian distribution with mean zero and variance
#' \deqn{V\left(\eta_t\right) = \Delta_t \sigma_B^2}
#'
#' @seealso \link{Movement}
#' 
#' @examples
#' d <- subadult_ringed_seal
#' mov <- argosTrack:::CSB(unique(as.POSIXct(d$date,tz="GMT")))
#'
#' @family "Movement models"
#'
#' @references
#' Parton, A., P. G. Blackwell, and A. Skarin. (2016) Bayesian Inference for Continuous Time Animal Movement Based on Steps and Turns. arXiv:1608.05583. 
#' 
#' 
#' @importFrom methods setRefClass new
#' @importFrom stats rnorm
#' @keywords internal
CSB <- setRefClass("CSB",
                  contains = "Movement",
                  methods = list(
                      copy = function (shallow = FALSE) 
                      {
                          value <- callSuper(shallow = shallow)
                          value
                      },
                      initialize = function(dates,
                                            pars = numeric(2),
                                            varPars = numeric(2),
                                            nauticalStates = FALSE,
                                            timeunit = "hours"
                                            ){
                          "Method to initialize the class. 'dates' is a vector of distinct and increasing POSIXct dates; 'pars' is vector of the movement parameters: \\eqn{log(\\beta)}, \\eqn{\\gamma}; 'varPars' is a vector of movement variance parameters: \\eqn{log(\\sigma_{S})}, \\eqn{log(\\sigma_{B})}; 'nauticalStates' is a logical value indicating whether the states should be modelled in nautical miles, and 'timeunit' is the time unit to use for calculating time steps."
###############
## Do checks ##
###############
                          if(!is.POSIXct(dates))
                              stop("dates must be a POSIXct class.")
                          dt0 <- as.numeric(difftime(tail(dates,-1),
                                                     head(dates,-1),
                                                     units = timeunit))
                          if(any(diff(dates) <= 0))
                              stop("dates must be sorted and different")
                          if(!(length(pars)==2 && is.numvec(pars)))
                              stop("pars must be a numeric vector of length 2.")
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
                          
                          initFields(model = "Continuous Time Step lengths / Bearings (CSB)",
                                     dates = dates,
                                     parameters = pars,
                                     varianceParameters = varPars,
                                     mu = matrix(0,2,length(dates)),
                                     vel = matrix(0,0,0),
                                     sdmu = matrix(NA,2,length(dates)),
                                     sdvel = matrix(NA,0,0),
                                     nauticalStates = nauticalStates,
                                     vcov = diag(Inf,0),
                                     timeunit = timeunit,
                                     data = list(),
                                     options = list(moveModelCode = 9,
                                                    parnames = c("log(beta)","gamma"),
                                                    varparnames = c("log(sigma_S)","log(sigma_B)"))
                                     )


                      },
                      simulate = function(x0 = c(0,0)){
                          "Function to simulate from the movement model. The initial states (latitudinal/y-coordinate location and longitudinal/x-coordinate location) must be given. The function only returns the locations. If nauticalStates==TRUE, the result is returned in nautical miles."

                          ## TODO: Handle nautical states better
                          beta <- exp(.self$parameters[1])
                          gamma <- .self$parameters[2]
                          varState <- exp(2.0 * .self$varianceParameters)
                          dt <- c(1,as.numeric(difftime(tail(.self$dates,-1),
                                                        head(.self$dates,-1),
                                                        units = .self$timeunit)))
                          Xtmp <- complex(length(.self$dates))
                          steplen <- rep(NA,length(.self$dates))
                          steplen[1] <- 0
                          bearing <- rep(NA,length(.self$dates))
                          bearing[1] <- 0

                          x0n <- .Call("ll2n",lon=x0[2],lat=x0[1], PACKAGE="argosTrack")
                          Xtmp[1] <- complex(real=x0n[1],
                                             imaginary=x0n[2])
                          for(i in 2:length(.self$dates)){
                              steplen[i] <- stats::rnorm(1,gamma+exp(-beta*dt[i])*(steplen[i-1]-gamma),sqrt(0.5*varState[1]/beta*(1-exp(-2*beta*dt[i]))))
                              bearing[i] <- stats::rnorm(1,bearing[i-1],sqrt(dt*varState[2]))
                              Xtmp[i] <- complex(argument = bearing[i],
                                                 modulus = steplen[i])
                          }
                          ## Return positions
                          Xcs <- cumsum(Xtmp)
                          X <- sapply(Xcs,function(x).Call("n2ll",x=Re(x),y=Im(x),PACKAGE="argosTrack"))[2:1,]
                          return(X)
                      },
                      getTMBparameters = function(){
                          "Function to create parameter list for TMB::MakeADFun."
                          pars <- callSuper()
                          if(all(diff(pars$mu[1,])==0) && all(diff(pars$mu[2,])==0))
                              ## starting values should not be random
                              pars$mu <- pars$mu + rnorm(length(pars$mu),0,0.1)
                          return(pars)
                      }   
                                       
                  )
                  )
