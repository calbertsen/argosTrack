#' A Reference Class for combining animal movement model parts.
#'
#' @field name String identifying the animal
#' @field observation Observation reference class
#' @field movement Movement reference class
#' @field measurement Measurement reference class
#' @field optim List to keep results of optimization through \code{\link{fitTrack}}
#' 
#' @seealso \code{\link{Movement}} \code{\link{Observation}} \code{\link{Measurement}}
#'
#' @examples
#' d <- subadult_ringed_seal
#' obs <- Observation(lon = d$lon,
#'                    lat = d$lat,
#'                    dates = as.POSIXct(d$date),
#'                    locationclass = d$lc
#'                    )
#' meas <- Measurement(model="n")
#' mov <- RW(unique(obs$dates))
#' anim <- Animal(obs,mov,meas,"Subadult")
#' plot(anim)
#'
#' @note The reference class methods are not intended to be used by the user. Please use appropriate functions such as \code{\link{fitTrack}}, \code{\link{plot}}, \code{\link{plotLon}}, \code{\link{plotLat}}, \code{\link{plotMap}}, and \code{\link{simTrack}}.
#' @author Christoffer Moesgaard Albertsen
#' 
#' @export Animal
#' @importFrom methods setRefClass new 
#' @exportClass Animal
Animal <- setRefClass("Animal",
                      sealed = TRUE,
                      field = c(name = "character",
                                observation = "refObject",
                                movement = "refObject",
                                measurement = "refObject",
                                optim = "list"
                                ),
                      methods = list(
                          copy = function (shallow = FALSE) 
                          {
                              def <- .refClassDef
                              value <- new(def,
                                           observation = .self$observation$copy(shallow),
                                           movement = .self$movement$copy(shallow),
                                           measurement = .self$measurement$copy(shallow),
                                           name = .self$name)
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
                          initialize = function(observation,
                                                movement,
                                                measurement,
                                                name = "",
                                                ...
                                                ){
                              "Method to initialize the class. 'observation' is an Observation reference class object, 'movement' is a Movement reference class object, 'measurement' is a Measuremenbt reference class object, and 'name' is an identifier string."
                              
###############
## Do checks ##
###############
                              if(class(observation) != "Observation")
                                  stop("observation must be an Observation object")
                              if(class(measurement) != "Measurement")
                                  stop("measurement must be a Measurement object")
                             
                              if(!("Movement" %in% getRefClass(class(movement))$def@refSuperClasses))
                                   stop(paste("movement must be one of the classes:",
                                              paste(names(getClass("Movement")@subclasses),collapse = ", ")
                                              ))                                    


################
## initFields ##
################                                   
                                   initFields(observation = observation,
                                              movement = movement,
                                              measurement = measurement,
                                              name = name,
                                              optim = list()
                                              )


                      },
                      getTMBdata = function(){
                          "Function to return a data list for TMB::MakeADFun"
                          ## prevState + stateFrac
                          prevState <- sapply(1:length(observation$lon),
                                              function(i) max((1:length(movement$dates))[movement$dates <= observation$dates[i]]))-1
                           stateFrac <- sapply(1:length(observation$lon),
                            function(i) 1 - (observation$dates[i] - movement$dates[prevState[i]+1]) / na.omit(c(diff(movement$dates[prevState[i]+1 + 0:1]),1))[1])
                          
                          dat <- list(prevState = prevState,
                                      stateFrac = stateFrac)

                          od <- observation$getTMBdata()
                          mvd <- movement$getTMBdata()
                          med <- measurement$getTMBdata(dayOfYearSpline = observation$dayOfYear)
                          
                          return(c(od,mvd,med,dat))
                      },
                      getTMBparameters = function(){
                          "Function to return a parameter list for TMB::MakeADFun"
                          op <- observation$getTMBparameters()
                          mvp <- movement$getTMBparameters()
                          mep <- measurement$getTMBparameters()

                          return(c(op,mvp,mep))
                      },
                      getTMBmap = function(fixcorrection = FALSE,...){
                          "Function to return a map list for TMB::MakeADFun. If fixcorrection = TRUE, the ratio between measurement errors for argos data is fixed."
                          useSpline <- any(.self$observation$varModelCode == 2)
                          numPrClass <- table(.self$observation$qual[.self$observation$varModelCode == 0])
                          usedLevels <- names(numPrClass)
                          om <- .self$observation$getTMBmap(...)
                          mvm <- .self$movement$getTMBmap(...)
                          mem <- .self$measurement$getTMBmap(correlated = FALSE,
                                                             useSpline = useSpline,
                                                             fixcorrection = fixcorrection,
                                                             usedLevels = usedLevels,
                                                             numPerLevel = numPrClass)
                          return(c(om,mvm,mem))
                      },
                      updateFromFit = function(opt){
                          "Function to save the result from nlminb."
                          optim <<- opt
                      },
                      simulate = function(newObject = FALSE){
                          "Function to simulate movement and measurement data for the Animal. A list containing the movement (X) and measurement (Y) data is returned. If newObject = TRUE, an Animal object is added to the list."
                          dat <- .self$getTMBdata()
                          dat$prevState
                          dat$stateFrac
                          X <- .self$movement$simulate()
                          Y <- .self$measurement$simulate(.self$observation)
                          mps <- sapply(dat$prevState+2,
                                        function(x)min(x,dim(X)[2]))
                          dat$stateFrac[dat$prevState==dim(X)[2]] <- 0
                          intState <- dat$stateFrac * X[,dat$prevState+1] +
                              (1 - dat$stateFrac) * X[,mps]
                          retList <- list(X = X,
                                          Y = Y + intState)
                          if(newObject){
                              nanim <- .self$copy()
                              nanim$name <- paste0(.self$name,"_copy")
                              nanim$observation$lat <- Y[1,] + intState[1,]
                              nanim$observation$lon <- Y[2,] + intState[2,]
                              retList$Animal <- nanim
                          }
                          return(retList)
                      },
                      show = function(){
                          cat("\n\nAnimal ",.self$name,"\n")
                          cat("------------------\n")
                          cat("------------------\n\n")
                          if(length(.self$optim) > 0){
                              cat("\n\nEstimation results\n")
                              cat("------------------\n\n")
                              cat("Time to estimate:",unname(.self$optim$estimation_time[3]),"\n")
                              cat("Convergence message:\n")
                              cat(.self$optim$message,"\n")
                              cat("\nNegative log likelihood:", .self$optim$objective,"\n")
                              cat("Maximum gradient component:",format(max(abs(.self$optim$gr)),trim=TRUE,digits=2,scientific=TRUE),"\n")
                          }else{
                              cat("\nThe model has not been fitted.\n\n")
                          }
                          .self$observation$show()
                          .self$measurement$show()
                          .self$movement$show()
                      },
                      addToLatPlot = function(obsarglist = list(pch = 16,
                                                             col = "grey"),
                                              movarglist = list(),
                                              sdarglist = list(col = "grey",
                                                               border = NA,
                                                               lwd = 3,
                                                               lty = 2),
                                              sd = TRUE){
                          "Function to add estimated movement and observation latitudes to a plot. obsarglist is a list of base graphics arguments to be used for observation data, movarglist is a list of base graphics arguments to be used for movement estimates, and sdarglist is a list of base graphics arguments to be used for movement standard errors (if sd = TRUE)."
                          if(sd)
                              .self$movement$addToLatPlotSd(sdarglist)
                          .self$observation$addToLatPlot(obsarglist)
                          .self$movement$addToLatPlot(movarglist)
                      },
                      addToLonPlot = function(obsarglist = list(pch = 16,
                                                             col = "grey"),
                                              movarglist = list(),
                                              sdarglist = list(col = "grey",
                                                               border = NA,
                                                               lwd = 3,
                                                               lty = 2),
                                              sd = TRUE){
                          "Function to add estimated movement and observation longitudes to a plot. obsarglist is a list of base graphics arguments to be used for observation data, movarglist is a list of base graphics arguments to be used for movement estimates, and sdarglist is a list of base graphics arguments to be used for movement standard errors (if sd = TRUE)."
                          if(sd)
                              .self$movement$addToLonPlotSd(sdarglist)
                          .self$observation$addToLonPlot(obsarglist)
                          .self$movement$addToLonPlot(movarglist)
                      },
                      addToMapPlot = function(obsarglist = list(type = "b",
                                                             pch=16,
                                                             col="grey"),
                                              movarglist = list()){
                           "Function to add estimated movement and observation coordinates to a plot. obsarglist is a list of base graphics arguments to be used for observation data, movarglist is a list of base graphics arguments to be used for movement estimates, and sdarglist is a list of base graphics arguments to be used for movement standard errors (if sd = TRUE)."
                          .self$observation$addToMapPlot(obsarglist)
                          .self$movement$addToMapPlot(movarglist)
                      },
                      getRange = function(sd = FALSE, zoomToState = FALSE){
                          "Returns the range of animal coordinates. If zoomToState = TRUE, only movement data is used. If sd = TRUE, the range includes standard errors of the movement estimates."
                          strng <- movement$getRange(sd)
                          if(zoomToState)
                              return(strng)

                          obsrng <- observation$getRange()
                          animrng <- matrix(NA,2,2)
                          animrng[1,1] <- min(strng[1,1],obsrng[1,1])
                          animrng[2,1] <- min(strng[2,1],obsrng[2,1])
                          animrng[1,2] <- max(strng[1,2],obsrng[1,2])
                          animrng[2,2] <- max(strng[2,2],obsrng[2,2])

                          return(animrng)
                      }
                  )
                  )
