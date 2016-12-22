#' A Reference Class for fitting a Dirac movement model.
#'
#' The reference class implements a Dirac movement model with no movement
#' \deqn{X_t \sim \delta(X_0))}
#' 
#' @seealso \link{Movement}
#'
#' @family "Movement models"
#' 
#' @examples
#' d <- subadult_ringed_seal
#' mov <- NON(unique(as.POSIXct(d$date)))
#'
#' 
#' @importFrom methods setRefClass new 
#' @exportClass DIRAC
DIRAC <- setRefClass("DIRAC",
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
                                            pars = numeric(2),
                                            varPars = numeric(0),
                                            nauticalStates = FALSE,
                                            timeunit = "hours"
                                            ){
                          "Method to initialize the class. 'dates' is a vector of distinct and increasing POSIXct dates; 'pars' is a zero length vector of the movement parameters; 'varPars' is a zero length vector of movement variance parameters; 'nauticalStates' is a logical value indicating whether the states should be modelled in nautical miles, and 'timeunit' is the time unit to use for calculating time steps."
###############
## Do checks ##
###############
                          if(!is.POSIXct(dates))
                              stop("dates must be a POSIXct class.")
                          if(any(diff(dates) <= 0))
                              stop("dates must be sorted and different")
                          if(!missing(pars))
                          if(!(length(pars)==0 && is.numvec(pars)))
                              stop("pars must be a numeric vector of length 0.")
                          if(!(length(varPars)==0 && is.numvec(varPars)))
                              stop("varPars must be a numeric vector of length 0.")
                          if(!missing(nauticalStates))
                          if(!(length(nauticalStates)==1 && is.logical(nauticalStates)))
                              stop("nauticalStates must be logical.")
                          if(!missing(timeunit))
                          if(!(length(timeunit == 1) &&
                                      timeunit %in% c("auto", "secs", "mins", 
                                                      "hours", "days", "weeks")))
                              stop("timeunit must be one of: 'auto', 'secs', 'mins', 'hours', 'days', 'weeks'.")

################
## initFields ##
################
                          
                          initFields(model = "Dirac Movement (DIRAC)",
                                     dates = dates,
                                     parameters = numeric(0),
                                     varianceParameters = numeric(0),
                                     mu = matrix(0,2,length(dates)),
                                     vel = matrix(0,0,0),
                                     sdmu = matrix(NA,2,length(dates)),
                                     sdvel = matrix(0,0,0),
                                     nauticalStates = nauticalStates,
                                     vcov = diag(Inf,0),
                                     timeunit = timeunit,
                                     data = list(),
                                     options = list(moveModelCode = 10, parnames = c())
                                     )


                      },
                      simulate = function(x0 = c(0,0)){
                          "Function to simulate from the movement model. The initial states (latitudinal/y-coordinate location and longitudinal/x-coordinate location) must be given. The function only returns the locations. If nauticalStates==TRUE, the result is returned in nautical miles."
                          dt <- c(1,as.numeric(difftime(tail(.self$dates,-1),head(.self$dates,-1), units = .self$timeunit)))
                          X <- matrix(NA,2,length(.self$dates))
                          X[1,] <- x0[1]
                          X[2,] <- x0[2]
                          return(X)
                      },
                      getTMBmap = function(...){
                          "Function to return a map list for TMB::MakeADFun."
                          mu0 <- matrix(c(1,2),nrow(.self$mu),ncol(.self$mu))
                          map <- list(mu = factor(as.vector(mu0)))
                          return(map)
                      }
                  
                  )
                  )
