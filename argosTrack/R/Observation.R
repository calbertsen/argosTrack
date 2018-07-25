#' A Reference Class for argosTrack observations.
#'
#' @field lat Latitude of observations
#' @field lon Longitude of observations
#' @field dates POSIXct vector of time stamps for observations
#' @field dayOfYear Day of year corresponding to dates
#' @field qual Factor of Argos location quality classes
#' @field varModelCode Integer vector of variance model codes (0, 1, or 2)
#' @field include Logical vector of whether observations should be included in likelihood
#' 
#' @seealso \code{\link{Movement}}, \code{\link{Measurement}}, \code{\link{Animal}}
#'
#' @examples
#' d <- subadult_ringed_seal
#' obs <- Observation(lon = d$lon,
#'                    lat = d$lat,
#'                    dates = as.POSIXct(d$date,tz="GMT"),
#'                    locationclass = d$lc
#'                    )
#' plot(obs)
#'
#' @author Christoffer Moesgaard Albertsen
#' 
#' @export Observation
#' @importFrom methods setRefClass new initRefFields
#' @exportClass Observation
Observation <- setRefClass("Observation",
                           field = c(lat = "numeric",
                                     lon = "numeric",
                                     dates = "POSIXct",
                                     dayOfYear = "numeric",
                                     qual = "factor",
                                     varModelCode = "integer",
                                     include = "logical"
                                     ),
                           methods = list(

                               copy = function (shallow = FALSE) 
                               {
                                   def <- .refClassDef
                                   value <- new(def,
                                                lon = .self$lon,
                                                lat = .self$lat,
                                                dates = .self$dates,
                                                locationclass = .self$getLocationClassInput(),
                                                include = .self$include)
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


                               initialize = function(lon,
                                                     lat,
                                                     dates,
                                                     locationclass,
                                                     include = rep(TRUE,length(dates)),
                                                     ...
                                                     ){
                                   "Method to initialize the class. 'lon' is a vector of longitudes, 'lat' is a vector of latitudes, 'dates' is a vector of POSIXct dates, 'locationclass' is a factor of Argos locatin classes, and Observation reference class object, 'movement' is a Movement reference class object, 'measurement' is a Measuremenbt reference class object, and 'include' is a logical vector of whether observations should be included in likelihood."
###############
## Do checks ##
###############
                                   if(!(is.numvec(lon)))
                                       stop("lon must be a numeric vector.")
                                   if(any(is.null(lon)) ||
                                      any(is.na(lon)) ||
                                      any(is.nan(lon)))
                                       stop("lon can not have NULL, NA, or NaN  elements.")
                                   if(any(!is.finite(lon)))
                                       stop("lon can only have finite elements.")

                                   if(!(is.numvec(lat)))
                                       stop("lat must be a numeric vector.")
                                   if(any(is.null(lat)) ||
                                      any(is.na(lat)) ||
                                      any(is.nan(lat)))
                                       stop("lat can not have NULL, NA, or NaN  elements.")
                                   if(any(!is.finite(lat)))
                                       stop("lat can only have finite elements.")

                                   if(!is.POSIXct(dates))
                                       stop("dates must be a POSIXct class.")
                                   if(any(is.null(dates)) ||
                                      any(is.na(dates)) ||
                                      any(is.nan(dates)))
                                       stop("dates can not have NULL, NA, or NaN  elements.")
                                   if(any(!is.finite(dates)))
                                       stop("dates can only have finite elements.")

                                   if(!is.character(locationclass) &&
                                      !is.factor(locationclass))
                                       stop("locationclass must be a factor or character vector.")
                                   if(any(is.null(locationclass)) ||
                                      any(is.na(locationclass)) ||
                                      any(is.nan(locationclass)))
                                       stop("locationclass can not have NULL, NA, or NaN elements.")
                                  
                                   if(!is.logical(include))
                                       stop("include must be a logical vector.")   
                                   if(any(is.null(include)))
                                       stop("include can not have NULL elements.")
                                   if(any(!is.finite(include)) ||
                                      any(is.na(include)) ||
                                      any(is.nan(include)))
                                       stop("include can only have finite elements.")
                                   
                                   if(!is.samelength(lon,lat) ||
                                      !is.samelength(lon,as.numeric(dates)) ||
                                      !is.samelength(lon,as.numeric(factor(locationclass))))
                                       stop("lon, lat, dates, locationclass and include must be same length")
                                   if(!missing(include))
                                       if(!is.samelength(lon,as.numeric(include)))
                                           stop("lon, lat, dates, locationclass and include must be same length")
                                   
                                   
                                                                     


################
## initFields ##
################
                                   argosClasses <- c("GPS","3", "2", "1", "0", "A", "B","Z")

                                   varModelCodeIn <- ifelse(locationclass=="K",1L,
                                                     ifelse(locationclass=="S", 2L,
                                                     ifelse(locationclass=="P", 3L, 0L)))
                                   dayOfYearIn <- as.numeric(strftime(dates,"%j"))


                                   qualIn <- factor(locationclass,
                                                    levels=argosClasses)
                                   if(any(is.na(qualIn[varModelCodeIn==0]))){
                                       stop("Location classes must be: GPS, 3, 2, 1, 0, A, B, or Z")
                                   }
                                   if(all(is.na(qualIn))){
                                       qualIn <- factor(rep("3",length(locationclass)))
                                   }

                                   
                                   initFields(lon = lon,
                                              lat = lat,
                                              dates = dates,
                                              dayOfYear = dayOfYearIn,
                                              qual = qualIn,
                                              varModelCode = varModelCodeIn,
                                              include = include
                                              )


                      },
                      getTMBdata = function(){
                          "Function to return a data list for TMB::MakeADFun"
                          dat <- list(lon = .self$lon,
                                      lat = .self$lat,
                                      dayOfYear = .self$dayOfYear,
                                      include = as.numeric(.self$include),
                                      qual = .self$qual,
                                      varModelCode = .self$varModelCode
                               )
                          return(dat)
                      },
                      getTMBparameters = function(){
                          "Function to return a parameter list for TMB::MakeADFun"
                          pars <- list()
                          return(pars)
                      },
                      getTMBmap = function(...){
                          "Function to return a map list for TMB::MakeADFun."
                          map <- list()
                          if(all(varModelCode %in% c(1,2)))
                              map$logSdObs <- factor(c(NA,NA)) ## Should probably be in Measurement class??
                          return(map) 
                      },
                      getLocationClassInput = function(){
                          lc <- as.character(.self$qual)
                          lc[.self$varModelCode == 1] <- "K"
                          lc[.self$varModelCode == 2] <- "S"
                          lc[.self$varModelCode == 3] <- "A"
                          return(lc)
                      },
                      show = function(){
                          cat("Observations:\n")
                          cat("-------------\n\n")
                          cat("Number of observations:",length(.self$lat),"\n")
                          cat("First date:",strftime(min(.self$dates),"%Y-%m-%d"),"\n")
                          cat("Last date:",strftime(max(.self$dates),"%Y-%m-%d"),"\n")
                          cat("Location classes:\n")
                          print(table(qual))
                      },
                      addToLatPlot = function(arglist = list(pch = 16,
                                                             col = "grey")){
                           "Function to add observed latitudes to a plot. arglist is a list of base graphics arguments to be used for plotting."
                          arglist$x = .self$dates
                          arglist$y = .self$lat
                          do.call("points",arglist)

                      },
                      addToLonPlot = function(arglist = list(pch = 16,
                                                             col = "grey")){
                          "Function to add observed longitudes to a plot. arglist is a list of base graphics arguments to be used for plotting."
                          arglist$x = .self$dates
                          arglist$y = .self$lon
                          do.call("points",arglist)

                      },
                      addToMapPlot = function(arglist = list(type = "b",
                                                             pch=16,
                                                             col="grey")){
                          "Function to add observed coordinates to a plot. arglist is a list of base graphics arguments to be used for plotting."
                          arglist$x = .self$lon
                          arglist$y = .self$lat
                          do.call("points",arglist)

                      },
                      getRange = function(){
                          "Returns the range of observed coordinates."
                          return(rbind(range(.self$lat),range(.self$lon)))
                      }
                  )
                  )
