
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
#' @param modelcode
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
                       timevarybeta = 1,
                       fixgammas = TRUE,
                       fixcorrection = FALSE,
                       dfVals = NULL,
                       dfMap = NULL,
                       minDf = 3.0,
                       errordistribution = "t",
                       movementmodel = "ctcrw",
                       verbose = TRUE,
                       timeunit = "mins",
                       nlminb.control = list(eval.max=2000,
                           iter.max=1500,
                           rel.tol=1e-3,
                           x.tol=1.5e-2)){
    

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
    movModNames <- c("rw","ctcrw","mmctcrw")
    modelCodeNum <- as.integer(factor(movementmodel,levels=movModNames))[1]-1
    if(is.na(modelCodeNum))
       stop(paste0("Wrong movement model code. Must be one of: ",paste(movModNames,sep=", "),"."))

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
                include = as.numeric(include),
                minDf = minDf,
                moveModelCode = modelCodeNum,
                timevary = as.integer(timevarybeta>1)
                )

    ## numStates <- ifelse(movementmodel == "mmctcrw",4,2)
    if(movementmodel == "mmctcrw"){
        numStates <- c(4,4)
    }else{
        numStates <- c(2,2)
    }
    
    parameters <- list(logbeta = matrix(0,
                           nrow=numStates[1],
                           ncol=length(dat$lon[dat$dt>0])),
                       logSdbeta = rep(200,numStates[1]),
                       logSdState = rep(0,numStates[2]),
                       logSdObs = c(0,0),
                       logCorrection = logCorrect,
                       gamma = rep(0,numStates[1]),
                       mu = matrix(0,
                           nrow=2,
                           ncol=length(dat$lon[dat$dt>0])),
                       vel = matrix(0,
                           nrow=numStates[1],
                           ncol=length(dat$lon[dat$dt>0]))
                       )
 
    if(any(!include) || any(!argosClasses%in%locationclass)){
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

     if(!(timevarybeta>1)){
        mbe <- matrix(1:numStates[1],
                      nrow=numStates[1],
                      ncol=length(dat$lon[dat$dt>0]))
    }else{
        mbe_vals <- rep(1:timevarybeta,
                        each=ceiling(length(dat$lon[dat$dt>0])/timevarybeta))
        mbe <- matrix(rep(mbe_vals,numStates[1]),
                      nrow=numStates[1],
                      ncol=length(dat$lon[dat$dt>0]),
                      TRUE)              
        ## mbe <- matrix(1:length(parameters$logbeta),
        ##               nrow=numStates[1],
        ##               ncol=length(dat$lon[dat$dt>0]))
    }
    
    if(equalbetas){
        if(movementmodel == "mmctcrw"){
            mbe[2,] <- mbe[1,]
            mbe[4,] <- mbe[3,]
        }else{
            mbe[2,] <- mbe[1,]
        }
    }
    map$logbeta <- factor(as.vector(mbe))
    ##if(!timevarybeta){
        map$logSdbeta <- factor(rep(NA,length(parameters$logSdbeta)))
    ##}else{
    ##    map$logSdbeta <- factor(mbe[,1])
    ##}
    
    if(movementmodel == "mmctcrw"){
        map$gamma <- factor(c(1,2,NA,NA))  # Drift in the slow process
        tt <- cumsum(dat$dt)
        parameters$logbeta <- matrix(c(-2,-2,0,0),
                           nrow=numStates[1],
                           ncol=length(dat$lon[dat$dt > 0]))
    }
    if(fixgammas){
        map$gamma <- factor(NA*parameters$gamma)
    }
    if(fixcorrection){
        map$logCorrection <-  factor(NA*parameters$logCorrection)
    }

    if(errordistribution == "t"){
        dat$modelCode <- 0
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
        dat$modelCode <- 1
        map$df <- factor(NA*parameters$df)
    }else{
        stop("Invalid error distribution. Must be either n or t")
    }


    if(movementmodel == "rw"){
        map$vel <- factor(parameters$vel*NA)
        map$logbeta <- factor(parameters$logbeta*NA)
        map$gamma <- factor(parameters$gamma*NA)
    }



    parameters$numdata <- length(dat$lon)
    map$numdata <- factor(NA)
    ## if(timevarybeta){
    ##     rnd <- c("mu","vel","logbeta")
    ## }else{
        rnd <- c("mu","vel")
    ## }
    obj <- TMB::MakeADFun(dat,parameters,map,random=rnd,DLL="argosTrack")
    obj$env$inner.control$trace <- verbose
    obj$env$tracemgc <- verbose

    esttime <- system.time(opt <- nlminb(obj$par,obj$fn,obj$gr,control=nlminb.control))
    
    srep <- TMB::summary.sdreport(TMB::sdreport(obj))
    track <- srep[rownames(srep)=="mu",]
    sdtrack <- matrix(track[,2],nrow=2)
    esttrack <- matrix(track[,1],nrow=2)
    res <- list()
    class(res) <- "argostrack"
    
    res$errordistribution <- errordistribution
    res$dates <- dates_in
    res$locationclass <- factor(locationclass,levels=argosClasses)
    res$observations <- t(cbind(lat,lon))
    rownames(res$observations) <- c("latitude","longitude")
    res$positions <- expandMu(esttrack,dat$dt)
    res$positions_sd <- expandMu(sdtrack,dat$dt)
    rownames(res$positions) <- c("latitude","longitude")
    res$optimization <- opt
    res$estimation_time <- esttime
    res$tmb_object <- obj
    res$sdreport_summary <- srep

    return(res)
    
}

                       
