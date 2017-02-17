#' A Reference Class for fitting a Continuous Time Correlated Random Walk model.
#'
#' The reference class implements a Continuous Time Correlated Random Walk (Johnson et al. 2008). The velocity is modelled by an Ornstein Uhlenbeck process in each coordinate (\eqn{c}),
#' \deqn{\nu_{c,t}-\gamma_c = e^{-\beta_c\Delta_{t}}[\nu_{c,t-\Delta_{t}}-\gamma_c]+\eta_{c,t},}
#' and the locations are modelled by the integrated velocity,
#' \deqn{X_{c,t} = X_{c,t-\Delta_{t}}+\nu_{c,t-\Delta_{t}}\left(1-e^{-\beta_c\Delta_t}\right)/\beta_c+\zeta_{c,t}.}
#' Here, \eqn{\Delta_t} is the length of the time steps and \eqn{\eta_{c,t}}, \eqn{\zeta_{c,t}} are random variables.
#' \eqn{\eta_{c,t}} follows a Gaussian distribution with mean zero and variance \eqn{V\left(\eta_{c,t}\right) = \sigma_c^2(1-e^{-2\beta_c\Delta_t})/(2\beta_c)}. The \eqn{\eta}'s are independent in time and coordiantes.
#' \eqn{\zeta_{c,t}} follows a Gaussian distribution with mean zero and variance
#' \deqn{V\left(\zeta_{c,t}\right)=\frac{\sigma_c^2}{\beta_c^2}\left(\Delta_t-2\left(1-e^{-\beta_c\Delta_t}\right)/\beta_c+\left(1-e^{-2\beta_c\Delta_t}\right)/(2\beta_c)\right).}
#' The \eqn{\zeta}'s are independent in time and coordiantes.
#' The model correlates \eqn{\eta_{c,t}} and \eqn{\zeta_{c',t'}} by
#' \deqn{Cov\left(\eta_{c,t},\zeta_{c,t}\right)= \frac{\sigma_c^2}{2\beta_c^2}\left(1-2e^{-\beta_c\Delta_t}+e^{-2\beta_c\Delta_i}\right).}
#' For \eqn{c\neq c'} and \eqn{t\neq t'}, \eqn{Cov\left(\eta_{c,t},\zeta_{c',t'}\right)=0}
#'
#' @seealso \link{Movement}
#' 
#' @examples
#' d <- subadult_ringed_seal
#' mov <- CTCRW(unique(as.POSIXct(d$date,tz="GMT")))
#'
#' @family "Movement models"
#'
#' @references
#' Johnson, D. S., J. M. London, M. A. Lea, and J. W. Durban. (2008) Continuous-time correlated random walk model for animal telemetry data. Ecology 89, 1208-1215.
#' \cr\cr Albertsen, C. M., Whoriskey, K., Yurkowski, D., Nielsen, A., and Flemming, J. M. (2015) Fast fitting of non-Gaussian state-space models to animal movement data via Template Model Builder. Ecology, 96(10), 2598-2604. doi: 10.1890/14-2101.1
#' 
#' @export CTCRW
#' @importFrom methods setRefClass new 
#' @exportClass CTCRW
CTCRW <- setRefClass("CTCRW",
                  contains = "Movement",
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
                                  if (is(current, "envRefClass")) 
                                      current <- current$copy(FALSE)
                                  assign(field, current, envir = vEnv)
                              }
                          }
                          value
                      },
                      initialize = function(dates,
                                            pars = numeric(4),
                                            varPars = numeric(2),
                                            nauticalStates = FALSE,
                                            timeunit = "hours"
                                            ){
                          "Method to initialize the class. 'dates' is a vector of distinct and increasing POSIXct dates; 'pars' is vector of the movement parameters: \\eqn{log(\\beta_{lat})}, \\eqn{log(\\beta_{lon})}, \\eqn{\\gamma_{lat}}, \\eqn{\\gamma_{lon}}; 'varPars' is a vector of movement variance parameters: \\eqn{log(\\sigma_{lat})}, \\eqn{log(\\sigma_{lon})}; 'nauticalStates' is a logical value indicating whether the states should be modelled in nautical miles, and 'timeunit' is the time unit to use for calculating time steps."
###############
## Do checks ##
###############
                          if(!is.POSIXct(dates))
                              stop("dates must be a POSIXct class.")
                          if(any(diff(dates) <= 0))
                              stop("dates must be sorted and different")
                          if(!(length(pars)==4 && is.numvec(pars)))
                              stop("pars must be a numeric vector of length 4.")
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
                          
                          initFields(model = "Continuous Time Correlated Random Walk (CTCRW)",
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
                                     options = list(moveModelCode = 1, parnames = c("log(beta_lat)","log(beta_lon)","gamma_lat","gamma_lon"))
                                     )


                      },
                      simulate = function(x0 = c(0,0,0,0)){
                          "Function to simulate from the movement model. The initial states (latitudianl velocity, longitudinal velocity, latitudinal/y-coordinate location and longitudinal/x-coordinate location) must be given. The function only returns the locations. If nauticalStates==TRUE, the result is returned in nautical miles."

                          ## TODO: Handle nautical states better
                          beta <- exp(.self$parameters[1:2])
                          gamma <- .self$parameters[3:4]
                          varState <- exp(2.0 * .self$varianceParameters)
                          dt <- c(1,as.numeric(difftime(tail(.self$dates,-1),
                                                        head(.self$dates,-1),
                                                        units = .self$timeunit)))
                          
                                      
                          X <- matrix(NA,4,length(.self$dates))
                          X[,1] <- x0
                          for(i in 2:length(.self$dates)){

                              ## Create covariance matrix
                              cov <- matrix(NA,4,4)
                              cov[1,1] <- varState[1]/beta[1]^2 * (dt[i]-2.0*(1.0-exp(-beta[1]*dt[i]))/beta[1]+(1.0-exp(-2.0*beta[1]*dt[i]))/(2.0*beta[1]))
                              cov[2,2] <- varState[1]*(1.0-exp(-2.0*beta[1]*dt[i]))/(2*beta[1])
                              cov[2,1] <- cov[1,2] <- varState[1]*(1.0-2.0*exp(-beta[1]*dt[i])+exp(-2.0*beta[1]*dt[2]))/(2.0*beta[1] ^ 2.0)

                              cov[3,3] <- varState[2]/beta[2]^2 * (dt[i]-2.0*(1.0-exp(-beta[2]*dt[i]))/beta[2]+(1.0-exp(-2.0*beta[2]*dt[i]))/(2.0*beta[2]))
                              cov[4,4] <- varState[2]*(1.0-exp(-2.0*beta[2]*dt[i]))/(2*beta[2])
                              cov[4,3] <- cov[3,4] <- varState[2]*(1.0-2.0*exp(-beta[2]*dt[i])+exp(-2.0*beta[2]*dt[2]))/(2.0*beta[2] ^ 2.0)

                              ## Create mean vector
                              state <- rep(NA,4)
                              state[1] <- X[1,i-1]+X[2,i-1]*(1.0-exp(-beta[1]*dt[i]))/beta[1]
                              state[2] <- gamma[1]+exp(-beta[1]*dt[i])*(X[2,i-1]-gamma[1])

                              state[3] <- X[3,i-1]+X[4,i-1]*(1.0-exp(-beta[2]*dt[i]))/beta[2]
                              state[4] <- gamma[2]+exp(-beta[1]*dt[i])*(X[2,i-1]-gamma[2])
                              
                              X[,i] <- rmvnorm(1,
                                               mu = state,
                                               sigma = cov)
                          }
                          ## Return positions
                          return(X[c(1,3),])
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
                                  doit <- TRUE
                              }
                          if("equaldrift" %in% names(args))
                              if(args$equaldrift){
                                  mpar[3:4] <- 3;
                                  doit <- TRUE
                              }
                          if("fixdrift" %in% names(args))
                              if(args$fixdrift){
                                  mpar[3:4] <- NA;
                                  doit <- TRUE
                              }

                          if(doit)
                              map$movePars <- factor(mpar)

                          return(map)
                          
                      }

                  
                  )
                  )
