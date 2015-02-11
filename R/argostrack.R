
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
#' @param fixcorrection
#' @param df
#' @param errordistribution
#' @param verbose Write maximum gradient components to the terminal?
#' @param timeunit

#' @export

#### TO DO ####
# Handle FALSE in include correctly
# What should be in the return object?
# Write documentation

argosTrack <- function(lon,lat,dates,locationclass,
                       include = rep(TRUE,length(dates)),
                       equalbetas = TRUE,
                       fixgammas = TRUE,
                       fixcorrection = FALSE,
                       df = 10,
                       errordistribution = "t",
                       verbose = TRUE,
                       timeunit = "mins",
                       nlminb.control=list(x.tol=1.5e-4,rel.tol=1e-4)){
    

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
                                         units=timeunit)
                                )
                   )
    }else if(class(dates)[1]=="POSIXct"){
        dates <- c(1,as.numeric(difftime(dates[(1+1):length(dates)],
                                         dates[1:(length(dates)-1)],
                                         units=timeunit)
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


    logCorrect <- matrix(c(0.6507,0.8231,
                           2.3432,2.0532,
                           3.8514,2.9602,
                           3.8280,3.0463,
                           4.4417,3.6951,
                           5.5714,5.5149),
                         nrow=2,
                         ncol=length(argosClasses)-1)
    if(!fixcorrection)
        logCorrect <- 0*logCorrect

    
    dat <- list(lon = lon,
                 lat = lat,
                 dt = dates,
                 qual = locclassfactor,
                 include = as.numeric(include)
                 )
                   

    parameters <- list(logbeta = c(0,0),
                       logSdState = c(0,0),
                       logSdObs = c(0,0),
                       logCorrection = logCorrect,
                       gamma = c(0,0),
                       mu = matrix(0,
                           nrow=2,
                           ncol=length(dat$lon[dat$dt>0])),
                       vel = matrix(0,
                           nrow=2,
                           ncol=length(dat$lon[dat$dt>0]))
                       )

    if(any(!include)){
        argosClassUse <- argosClasses[argosClasses%in%locationclass[include]]
        dat$qual[!include] <- argosClassUse[1]
        dat$qual <- factor(dat$qual,levels=argosClassUse)
        parameters$logCorrection = matrix(0,
                           nrow=2,
                           ncol=length(argosClassUse)-1)
    }
    if(length(df) < nlevels(dat$qual))
        df <- c(df,rep(df[length(df)],nlevels(dat$qual)-length(df)))
    parameters$df <- log(df-2)

    #map <- list(df=factor(NA*parameters$df))
    map <- list()
    
    if(equalbetas){
        map$logbeta <- factor(c(1,1))
    }
    if(fixgammas){
        map$gamma <- factor(NA*parameters$gamma)
    }
    if(fixcorrection){
        map$logCorrection <-  factor(NA*parameters$logCorrection)
    }

    if(errordistribution == "t"){
        usedll <- "ringednt"
        numPrClass <- as.numeric(table(dat$qual[include]))
        dfFac <- numeric(length(df))
        dfFac[1] <- 1
        for(qq in 2:length(dfFac)){
            dfFac[qq] <- qq
            if(numPrClass[qq] < 20)
                dfFac[qq] <- dfFac[qq-1]
        }
        if(dfFac[1] < 20)
            dfFac[1] <- dfFac[2]
                
    }else if(errordistribution == "n"){
        usedll <- "ringednn"
        map$df <- factor(NA*parameters$df)
    }else{
        stop("Invalid error distribution. Must be either n or t")
    }

    obj <- TMB::MakeADFun(dat,parameters,map,random=c("mu","vel"),DLL=usedll)
    obj$env$inner.control$trace <- verbose
    obj$env$tracemgc <- verbose

    esttime <- system.time(opt <- nlminb(obj$par,obj$fn,obj$gr,control=nlminb.control))
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

                       
