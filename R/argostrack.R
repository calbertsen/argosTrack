
#' Estimate track from Argos data
#'
#' Estimate animal movement track from Argos data using TMB (tmb-project.org).
#'
#' @param lon
#' @param lat
#' @param dates
#' @param locationclass
#' @param include
#' @param equalbetas
#' @param fixgammas
#' @param df
#' @param errordistribution
#' @param verbose Write maximum gradient components to the terminal?

#' @export

#### TO DO ####
# Handle FALSE in include correctly
# What should be in the return object?
# Write documentation

argosTrack <- function(lon,lat,dates,locationclass,
                       include = rep(TRUE,length(dates)),
                       equalbetas = TRUE,
                       fixgammas = TRUE,
                       df = 10,
                       errordistribution = "t",
                       verbose = TRUE){
    

    argosClasses <- c("3", "2", "1", "0", "A", "B","Z")

    if(is.factor(dates)){
        dates <- as.character(dates)
    }
    
    dates_in <- dates
    if(is.numeric(dates)){
        dates <- c(1,diff(dates))

    }else if(is.character(dates)){
        dates <- as.POSIXct(dates)
        dates <- c(1,as.numeric(difftime(dates[(1+1):length(dates)],
                                         dates[1:(length(dates)-1)],
                                         units="min")
                                )
                   )
    }else if(class(dates)[1]=="POSIXct"){
        dates <- c(1,as.numeric(difftime(dates[(1+1):length(dates)],
                                         dates[1:(length(dates)-1)],
                                         units="min")
                                )
                   )
    }else{
        stop("Dates must be a factor, numeric, character or POSIXct vector")
    }

    # No negative time differences
    if(any(dates<0)){
        stop("Dates must be ordered. Differences can not be negative")
    }
    locclassfactor <- factor(locationclass,
                            levels=argosClasses[argosClasses%in%locationclass])
    if(any(is.na(locclassfactor))){
        stop("Location classes must be: 3, 2, 1, 0, A, B, or Z")
    }

    dat <- list(lon = lon,
                 lat = lat,
                 dt = dates,
                 qual = locclassfactor,
                 include = as.numeric(include)
                 )
                   

    parameters <- list(logbeta = c(0,0),
                       logSdState = c(0,0),
                       logSdObs = c(0,0),
                       logCorrection = matrix(0,
                           nrow=2,
                           ncol=length(argosClasses)-1),
                       gamma = c(0,0),
                       mu = matrix(0,
                           nrow=2,
                           ncol=length(dat$lon[dat$dt>0])),
                       vel = matrix(0,
                           nrow=2,
                           ncol=length(dat$lon[dat$dt>0])),
                       df = c(1,1)*df
                       )

    if(any(!include)){
        argosClassUse <- argosClasses[argosClasses%in%locationclass[include]]
        dat$qual[!include] <- argosClassUse[1]
        dat$qual <- factor(dat$qual,levels=argosClassUse)
        parameters$logCorrection = matrix(0,
                           nrow=2,
                           ncol=length(argosClassUse)-1)
    }


    map <- list(df=factor(NA*parameters$df))

    if(equalbetas){
        map$logbeta <- factor(c(1,1))
    }
    if(fixgammas){
        map$gamma <- factor(NA*parameters$gamma)
    }

    if(errordistribution == "t"){
        usedll <- "ringednt"
    }else if(errordistribution == "n"){
        usedll <- "ringednn"
    }else{
        stop("Invalid error distribution. Must be either n or t")
    }

    obj <- TMB::MakeADFun(dat,parameters,map,random=c("mu","vel"),DLL=usedll)
    obj$env$inner.control$trace <- verbose
    obj$env$tracemgc <- verbose

    esttime <- system.time(opt <- nlminb(obj$par,obj$fn,obj$gr))
    srep <- summary(TMB::sdreport(obj))
    track <- srep[rownames(srep)=="mu",]
    sdtrack <- matrix(track[,2],nrow=2)
    esttrack <- matrix(track[,1],nrow=2)
    res <- list()
    class(res) <- "argostrack"

    res$dates <- dates_in
    res$locationclass <- factor(locationclass,levels=argosClasses)
    res$observations <- t(cbind(lat,lon))
    rownames(res$observations) <- c("latitude","longitude")
    res$positions <- expandMu(esttrack,dat$dt)
    #res$positions_sd <- expandMu(sdtrack,dat$dt)
    rownames(res$positions) <- c("latitude","longitude")
    res$optimization <- opt
    res$estimation_time <- esttime
    res$tmb_object <- obj
    res$sdreport_summary <- srep

    return(res)
    
}

                       
