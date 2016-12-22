#' A Reference Class for fitting an Ornstein-Uhlenbeck Velocity model.
#'
#' The reference class implements an Ornstein-Uhlenbeck Velocity. The velocity is modelled by the (bivariate) stochastic differential equation
#' \deqn{dV_t = -\Theta (V_t-\mu) d_t + S dB_t,}
#' Since the locations are know from the velocity by \eqn{X_{t} = \sum_{i=0}^{t} V_i}, the increments are known as
#' \deqn{X_{t_n} - X_{t_{n-1}} = V_{t_n}.}
#' Hence,
#' \deqn{X_{t_n} = X_{t_{n-1}} + \mu + \Theta (X_{t_{n-1}} - X_{t_{n-2}} - \mu)}
#'
#' 
#' @seealso \code{\link{Movement}}. A generalization of \code{\link{DCRW}}, \code{\link{RW}}, and \code{\link{CTCRW}} (except only the location is modelled here and it is assumed to be known given the velocities)
#'
#' @family "Movement models"
#' 
#' @references
#' Albertsen, C.M. (2016) Personal communication.
#'
#' @examples
#' d <- subadult_ringed_seal
#' mov <- argosTrack:::OUV(unique(as.POSIXct(d$date)))
#' 
#' @keywords internal
OUV <- setRefClass("OUV",
                  contains = "Movement",
                  methods = list(
                      initialize = function(dates,
                                            pars = c(1,0,0,1,0,0,0),
                                            varPars = numeric(2),
                                            nauticalStates = FALSE,
                                            timeunit = "hours"
                                            ){
                          "Method to initialize the class. 'dates' is a vector of distinct and increasing POSIXct dates; 'pars' is vector of the movement parameters: \\eqn{\\Theta_{0,0}}, \\eqn{\\Theta_{1,0}},\\eqn{\\Theta_{0,1}},\\eqn{\\Theta_{1,1}}, \\eqn{logit_{(-1,1)}(\\rho)}, \\eqn{\\mu_{lat}}, \\eqn{\\mu_{lon}}; 'varPars' is a vector of movement variance parameters: \\eqn{log(\\sigma_{lat})}, \\eqn{log(\\sigma_{lat})}; 'nauticalStates' is a logical value indicating whether the states should be modelled in nautical miles, and 'timeunit' is the time unit to use for calculating time steps."
###############
## Do checks ##
###############
                          if(!is.POSIXct(dates))
                              stop("dates must be a POSIXct object.")
                          if(any(diff(dates) <= 0))
                              stop("dates must be sorted and different")
                          if(!(length(pars)==7 && is.numvec(pars)))
                              stop("pars must be a numeric vector of length 7.")
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
                          
                          initFields(model = "Ornstein-Uhlenbeck Velocity (OUV)",
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
                                     options = list(moveModelCode = 8, parnames = c("Theta_{11}","Theta_{21}","Theta_{12}","Theta_{22}","logit[-1,1](rho)","mu_1","mu_2"))
                                     )


                      },
                      simulate = function(x0 = c(0,0)){
                          "Function to simulate from the movement model. The initial states (latitudinal/y-coordinate location and longitudinal/x-coordinate location) must be given. If nauticalStates==TRUE, the result is returned in nautical miles."
                          dt <- c(1,as.numeric(difftime(tail(.self$dates,-1),
                                                        head(.self$dates,-1),
                                                        units = .self$timeunit)))
                          gamma <- 1/(1+exp(-.self$parameters[1:2]))
                          rho <- 2.0/(1+exp(-.self$parameters[4])) - 1
                          phi <- .self$parameters[3]
                          mupar <- .self$parameters[5:6]
                          cov <- matrix(c(exp(2*.self$varianceParameters[1]),
                                         rho * exp(sum(.self$varianceParameters)),
                                         rho * exp(sum(.self$varianceParameters)),
                                         exp(2*.self$varianceParameters[2])),2,2)

                          Gth <- matrix(c(-log(gamma),-phi,phi,-log(gamma)),2,2)
                         
                          var <- function(dt){
                              ## var <- matrix(NA,2,2)
                              ## var[1,1] <- abs((1/2)*(cov[1,1]*cos(phi*dt)*sin(phi*dt)+cov[2,2]*cos(phi*dt)*sin(phi*dt)+cov[1,1]*phi*dt-cov[2,2]*phi*dt)/phi)
                              ## var[1,2] <- 0.0 ##-(1/2)*(cov[1,1]*cos(phi*dt)^2+cos(phi*dt)^2*cov[2,2]-2*cov[1,2]*phi*dt-cov[1,1]-cov[2,2])/phi
                              ## var[2,1] <- var[1,2]
                              ## var[2,2] <- abs((1/2)*(cov[1,1]*cos(phi*dt)*sin(phi*dt)+cov[2,2]*cos(phi*dt)*sin(phi*dt)-cov[1,1]*phi*dt+cov[2,2]*phi*dt)/phi)
                              ## var
                              dt*dt*.Call("idtcrwVarMat",dt=dt,
                                          gamma=gamma,
                                          phi=phi,
                                          rho=rho,
                                          varState=diag(cov),
                                          PACKAGE = "argosTrack")
                          }
                          
                          state <- function(Xm,Xmm,dt,dtm){
                              meGth <- Matrix::expm(-Gth*dt)
                              ##Xm + gamma * as.vector(meGth %*% (Xm-Xmm))
                              Xm + dt*(mupar + as.vector(meGth %*% ((Xm-Xmm)/dtm - mupar)))
                          }
                          
                          X <- matrix(NA,2,length(.self$dates))
                          X[,1] <- x0
                          mu0 <- X[,1] + mupar
                          X[,2] <- rmvnorm(1,
                                           mu = mu0,
                                           sigma = cov)
                          for(i in 3:length(.self$dates)){
                              mu0 <- state(X[,i-1],X[,i-2],dt[i],dt[i-1])
                              X[,i] <- rmvnorm(1,
                                               mu = mu0,
                                               sigma = var(dt[i]))
                          }
                          return(X)
                      },
                      getTMBmap = function(...){
                          "Function to create map list for TMB::MakeADFun. If equaldecay=TRUE, \\eqn{\\gamma_1} and \\eqn{\\gamma_2} are estimated as equal. If equaldecay=TRUE, \\eqn{\\mu_1} and \\eqn{\\mu_2} are estimated as equal. If fixdrift=TRUE, \\eqn{\\mu_1} and \\eqn{\\mu_2} are fixed at the current value."
                          
                          map <- callSuper(...)
                          args <- list(...)
                          mpar <- 1:7
                          doit <- FALSE

                          if("symdecay" %in% names(args))
                              if(args$symdecay){
                                  mpar[2:3] <- 2;
                                  doit <- TRUE
                              }
                          if("diagdecay" %in% names(args))
                              if(args$diagdecay){
                                  mpar[2:3] <- NA;
                                  doit <- TRUE
                              }
                          if("equaldecay" %in% names(args))  ## gamma             
                              if(args$equaldecay){
                                  mpar[c(1,4)] <- 1;
                                  doit <- TRUE
                              }
                          if("equaldrift" %in% names(args)) ## mu
                              if(args$equaldrift){
                                  mpar[6:7] <- 6;
                                  doit <- TRUE
                              }
                          if("fixdrift" %in% names(args)) ## mu
                              if(args$fixdrift){
                                  mpar[6:7] <- NA;
                                  doit <- TRUE
                              }


                          ## Always equal decay
                          #doit <- TRUE

                          if(doit)
                              map$movePars <- factor(mpar)

                          return(map)
                          
                      }

                  
                  )
                  )
