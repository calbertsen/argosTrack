#' A Reference Class for fitting a Random Walk model.
#'
#' The reference class implements a Random Walk (e.g. Nielsen et al. 2006).
#' \deqn{X_t \sim N(X_{t-\Delta_t},\Delta_t diag(\sigma_1^2,\sigma_2^2))}
#' 
#' @seealso \link{Movement}
#'
#' @family "Movement models"
#' 
#' @examples
#' d <- subadult_ringed_seal
#' mov <- CTCRW(unique(as.POSIXct(d$date)))
#'
#' @references
#' Nielsen, A., Bigelow, K. A., Musyl, M. K. and Sibert, J. R. (2006) Improving light-based geolocation by including sea surface temperature. Fisheries Oceanography, 15: 314-325. doi: 10.1111/j.1365-2419.2005.00401.x
#' 
#' @export RW
#' @importFrom methods setRefClass new 
#' @exportClass RW
RW <- setRefClass("RW",
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
                                            pars = numeric(0),
                                            varPars = numeric(2),
                                            nauticalStates = FALSE,
                                            timeunit = "hours"
                                            ){
                          "Method to initialize the class. 'dates' is a vector of distinct and increasing POSIXct dates; 'pars' is zero length vector of the movement parameters; 'varPars' is a vector of movement variance parameters: \\eqn{log(\\sigma_{lat})}, \\eqn{log(\\sigma_{lon})}; 'nauticalStates' is a logical value indicating whether the states should be modelled in nautical miles, and 'timeunit' is the time unit to use for calculating time steps."
###############
## Do checks ##
###############
                          if(!is.POSIXct(dates))
                              stop("dates must be a POSIXct class.")
                          if(!missing(pars))
                          if(!(length(pars)==0 && is.numvec(pars)))
                              stop("pars must be a numeric vector of length 0.")
                          if(!(length(varPars)==2 && is.numvec(varPars)))
                              stop("varPars must be a numeric vector of length 2.")
                          if(!missing(nauticalStates))
                          if(!(length(nauticalStates)==1 && is.logical(nauticalStates)))
                              stop("nauticalStates must be logical.")
                          if(!missing(timeunit))
                          if(!(length(timeunit == 1 &&
                                      timeunit %in% c("auto", "secs", "mins", 
                                                      "hours", "days", "weeks"))))
                              stop("timeunit must be one of: 'auto', 'secs', 'mins', 'hours', 'days', 'weeks'.")

################
## initFields ##
################
                          
                          initFields(model = "Random Walk (RW)",
                                     dates = dates,
                                     parameters = numeric(0),
                                     varianceParameters = varPars,
                                     mu = matrix(0,2,length(dates)),
                                     vel = matrix(0,0,0),
                                     sdmu = matrix(NA,2,length(dates)),
                                     sdvel = matrix(0,0,0),
                                     nauticalStates = nauticalStates,
                                     vcov = diag(Inf,0),
                                     timeunit = timeunit,
                                     data = list(),
                                     options = list(moveModelCode = 0, parnames = c())
                                     )


                      },
                      simulate = function(x0 = c(0,0)){
                          "Function to simulate from the movement model. The initial states (latitudinal/y-coordinate location and longitudinal/x-coordinate location) must be given. The function only returns the locations. If nauticalStates==TRUE, the result is returned in nautical miles."
                          dt <- c(1,as.numeric(difftime(tail(.self$dates,-1),head(.self$dates,-1), units = .self$timeunit)))
                          X <- matrix(NA,2,length(.self$dates))
                          X[,1] <- x0
                          for(i in 2:length(.self$dates)){
                              X[,i] <- rmvnorm(1,
                                               mu = as.vector(X[,i-1]),
                                               sigma = dt[i]*diag(exp(2*.self$varianceParameters)))
                          }
                          return(X)
                      }

                  
                  )
                  )
