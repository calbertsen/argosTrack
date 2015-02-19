
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
#' @param dfMap
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
                       dfVals = NULL,
                       dfMap = NULL,
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

    if(!is.null(dfVals)){
        parameters$df <- log(dfVals)
    }else{
        parameters$df <- rep(log(8),nlevels(dat$qual))
    }

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
        if(is.null(dfMap)){
            numPrClass <- as.numeric(table(dat$qual[include]))
            dfMap <- numeric(length(parameters$df))
            dfMap[1] <- 1
            for(qq in 2:length(dfMap)){
                dfMap[qq] <- qq
                if(numPrClass[qq] < 20)
                    dfMap[qq] <- dfMap[qq-1]
            }
            if(numPrClass[1] < 20)
                dfMap[1] <- dfMap[2]
            #print(dfMap)
        }
        if(!is.factor(dfMap))
            dfMap <- factor(dfMap)
        map$df <- dfMap
                
    }else if(errordistribution == "n"){
        usedll <- "ringednn"
        map$df <- factor(NA*parameters$df)
    }else{
        stop("Invalid error distribution. Must be either n or t")
    }

    obj <- TMB::MakeADFun(dat,parameters,map,random=c("mu","vel"),DLL=usedll)
    obj$env$inner.control$trace <- verbose
    obj$env$tracemgc <- verbose

    parm <- obj$par[names(obj$par)!="df"]
    paro <- obj$par[names(obj$par)=="df"]
    grm <- function(par,df){
        pp <- obj$par
        pp[names(pp)!="df"] <- par
        pp[names(pp)=="df"] <- df
        obj$gr(pp)[names(pp)!="df"]
    }
    fnm <- function(par,df){
        pp <- obj$par
        pp[names(pp)!="df"] <- par
        pp[names(pp)=="df"] <- df
        obj$fn(pp)
    }
    fno <- function(par){
        pp <- obj$env$last.par.best[-obj$env$random]
        nlminb(pp[names(pp)!="df"],fnm,grm,control=nlminb.control,df=par)$objective
    }
    #low <- rep(-Inf,length(obj$par))
    #low[names(obj$par)=="df"] <- log(2.5)
    esttime <- system.time(opt <- nlminb(obj$par,obj$fn,obj$gr,control=nlminb.control,lower=low))
    #esttime <- system.time(opt <- nlminb(paro,fno,control=nlminb.control,lower=rep(2.05,length(paro))))

    
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
    #res$positions <- expandMu(esttrack,dat$dt)
    #res$positions_sd <- expandMu(sdtrack,dat$dt)
    #rownames(res$positions) <- c("latitude","longitude")
    res$optimization <- opt
    res$estimation_time <- esttime
    res$tmb_object <- obj
    res$sdreport_summary <- srep

    return(res)
    
}

                       
