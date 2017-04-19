#' A Reference Class for fitting a Discrete time Step-length/Bearings model with Half Normal distributed steps.
#'
#' The reference class implements a Discrete time Step-length/Bearings model with Half Normal distributed steps. \cr
#' Let \eqn{X_t} be the (bivariate) location (in nautical miles) at time \eqn{t}. The step lengths are
#' \deqn{S_t = \sqrt{\sum\left(X_t-X_{t-1}\right)^2}}
#' and the bearings are
#' \deqn{B_t = atan2(X_{y,t}-X_{y,t-1},X_{x,t}-X_{y,t-1})} 
#' where \eqn{x} denotes the east/west coordinate and \eqn{y} denotes the north/south coordinate.
#' The step lengths are modelled by a half normal distribution
#' \deqn{S_t \sim HN(\sigma^2)}
#' and the bearings are modelled by a wrapped Cauchy distribution with location parameter \eqn{B_{t-1}} and concentration parameter \eqn{\rho}.
#' 
#' @seealso \code{\link{Movement}}, \code{link{DSBW}}.
#'
#' @family "Movement models"
#'
#' @note Step-length/Bearings models can be difficult for the Laplace approximation. Estimation problems can often be solved by decreasing the number of states relative to the number of observations.
#' 
#' @examples
#' d <- subadult_ringed_seal
#' dates <- unique(as.POSIXct(d$date,tz="GMT"))
#' dseq <- seq(min(dates),max(dates), "day")
#' mov <- DSBHN(dseq)
#' 
#' @export DSBHN
#' @importFrom methods setRefClass new 
#' @exportClass DSBHN
DSBHN <- setRefClass("DSBHN",
                  contains = "Movement",
                  methods = list(
                      copy = function (shallow = FALSE) 
                      {
                          value <- callSuper(shallow = shallow)
                          value
                      },
                      initialize = function(dates,
                                            pars = numeric(1),
                                            varPars = numeric(1),
                                            nauticalStates = FALSE,
                                            timeunit = "hours"
                                            ){
                          "Method to initialize the class. 'dates' is a vector of distinct and equidistant POSIXct dates; 'pars' is vector of the movement parameters: \\eqn{log(\\rho)}; 'varPars' is a vector of movement variance parameters: \\eqn{log(\\sigma)}; 'nauticalStates' is a logical value indicating whether the states should be modelled in nautical miles, and 'timeunit' is the time unit to use for calculating time steps."

###############
## Do checks ##
###############

                          if(!missing(timeunit))
                              if(!(length(timeunit == 1) &&
                                   timeunit %in% c("auto", "secs", "mins", 
                                                   "hours", "days", "weeks")))
                                  stop("timeunit must be one of: 'auto', 'secs', 'mins', 'hours', 'days', 'weeks'.")
                          if(!is.POSIXct(dates))
                              stop("dates must be a POSIXct class.")
                          dt0 <- as.numeric(difftime(tail(dates,-1),
                                                     head(dates,-1),
                                                     units = timeunit))
                          if(!(all(round(dt0,5) == round(dt0[1],5))))
                              stop("Dates must be equidistant.")
                          if(!(length(pars)==1 && is.numvec(pars)))
                              stop("pars must be a numeric vector of length 1.")
                          if(!(length(varPars)==1 && is.numvec(varPars)))
                              stop("varPars must be a numeric vector of length 1.")
                          if(!missing(nauticalStates))
                          if(!(length(nauticalStates)==1 && is.logical(nauticalStates)))
                              stop("nauticalStates must be logical.")


################
## initFields ##
################
                          
                          initFields(model = "Discrete time Step-length/Bearings model with Half Normal distributed steps (DSBHN)",
                                     dates = dates,
                                     parameters = pars,
                                     varianceParameters = varPars,
                                     mu = matrix(0,2,length(dates)),
                                     vel = matrix(0,0,0),
                                     sdmu = matrix(NA,2,length(dates)),
                                     sdvel = matrix(0,0,0),
                                     nauticalStates = nauticalStates,
                                     vcov = diag(Inf,0),
                                     timeunit = timeunit,
                                     data = list(),
                                     options = list(moveModelCode = 5,
                                                    parnames = c("log(rho)"),
                                                    varparnames = c("log(sigma)"))
                                     )


                      },
                      simulate = function(x0 = c(0,0)){
                          "Function to simulate from the movement model. The initial states (latitudinal/y-coordinate location and longitudinal/x-coordinate location) must be given. If nauticalStates==TRUE, the result is returned in nautical miles."
                          dt <- c(1,as.numeric(difftime(tail(.self$dates,-1),head(.self$dates,-1), units = .self$timeunit)))
                          Xtmp <- complex(length(.self$dates))
                          steplen <- rep(NA,length(.self$dates))
                          steplen[1] <- 0
                          bearing <- rep(NA,length(.self$dates))
                          bearing[1] <- 0

                          ## Transform x0 to nautical
                          x0n <- .Call("ll2n",lon=x0[2],lat=x0[1], PACKAGE="argosTrack")
                          Xtmp[1] <- complex(real=x0n[1],
                                             imaginary=x0n[2])
                                                    
                          for(i in 2:length(.self$dates)){
                              steplen[i] <- rhalfnorm(1,exp(2*.self$varianceParameters[1]))
                              bearing[i] <- rwcauchy(1,bearing[i-1],exp(.self$parameters[1]))
                              Xtmp[i] <- complex(argument = bearing[i],
                                                 modulus = steplen[i])
                              ## Add to state                              
                          }

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
