
## devtools::install_local("~/argosTrack")

## devtools::install_github("calbertsen/argosTrack",ref="timevarying")

plot.resid <- function(resid,object){
    par(mfrow=c(4,2));
    apply(resid,1,function(x){qqnorm(x);abline(0,1)})
    apply(resid,1,function(x){plot(x,ylim=c(-1.1,1.1)*(max(abs(x),na.rm=TRUE)),pch=16);
                                   abline(h=qnorm(c(0.025,0.5,0.975),0,1),col="grey")})
    redu <- which(apply(resid,2,function(x)all(is.finite(x))))
    apply(resid[,redu],1,function(x)acf(x,na.action=na.omit))
    vdat <- data.frame(lat = resid[1,redu],
                       lon = resid[2,redu])
    time <- cumsum(object$tmb_object$env$data$dt)[redu]
    spdat <- SpatialPointsDataFrame(coords = cbind(time,0),
                                    data=vdat)
    v1 <- variogram(lat ~ 1, data=spdat)
    plot(v1$dist,v1$gamma,xlab="time",ylab="semivariance",
         ylim=c(0,max(v1$gamma)*1.1))
    abline(1,0,col="grey")
    v1 <- variogram(lat ~ 1, data=spdat)
    plot(v1$dist,v1$gamma,xlab="time",ylab="semivariance",
         ylim=c(0,max(v1$gamma)*1.1))
    abline(1,0,col="grey")
}


library(argosTrack)
library(gstat)
library(sp)

dat <- subadult_ringed_seal
dat <- dat[!(dat$lc=="Z"),]
dat$lc <- factor(dat$lc)
table(dat$lc)

osam <- NULL

## Fit with normal distribution - continuous time correlated random walk movement model
args <- list(lon = dat$lon + rnorm(length(dat$lon),0,0.00001),
             lat = dat$lat + rnorm(length(dat$lon),0,0.00001),
             dates = as.character(dat$date),
             locationclass = dat$lc,
             verbose=FALSE,
             fixcorrection=FALSE,
             errordistribution="n",
             movementmodel="ctcrw",
             timeunit="hours"
             )
fitctcrw <- do.call(argosTrack,args)
resid_ctcrw <- residuals(fitctcrw,osamethod=osam)

args <- list(lon = dat$lon + rnorm(length(dat$lon),0,0.00001),
             lat = dat$lat + rnorm(length(dat$lon),0,0.00001),
             dates = as.character(dat$date),
             locationclass = dat$lc,
             verbose=FALSE,
             fixcorrection=FALSE,
             equalbetas=FALSE,
             errordistribution="n",
             movementmodel="ctcrw",
             timeunit="hours"
             )
fitctcrwne <- do.call(argosTrack,args)
resid_ctcrwne <- residuals(fitctcrwne,osamethod=osam)

args <- list(lon = dat$lon + rnorm(length(dat$lon),0,0.00001),
             lat = dat$lat + rnorm(length(dat$lon),0,0.00001),
             dates = as.character(dat$date),
             locationclass = dat$lc,
             verbose=FALSE,
             fixcorrection=FALSE,
             equalbetas=FALSE,
             fixgamma=FALSE,
             errordistribution="n",
             movementmodel="ctcrw",
             timeunit="hours"
             )
fitctcrwned <- do.call(argosTrack,args)
resid_ctcrwned <- residuals(fitctcrwned,osamethod=osam)


## Fit with normal distribution - continuous time correlated random walk movement model
tvb <- 100
args <- list(lon = dat$lon + rnorm(length(dat$lon),0,0.00001),
             lat = dat$lat + rnorm(length(dat$lon),0,0.00001),
             dates = as.character(dat$date),
             locationclass = dat$lc,
             timevarybeta=tvb,
             verbose=FALSE,
             fixcorrection=FALSE,
             errordistribution="n",
             movementmodel="ctcrw",
             timeunit="hours"
             )
fittv <- do.call(argosTrack,args)
resid_tv <- residuals(fittv,osamethod=osam)

## tvb <- 50
args <- list(lon = dat$lon + rnorm(length(dat$lon),0,0.00001),
             lat = dat$lat + rnorm(length(dat$lon),0,0.00001),
             dates = as.character(dat$date),
             locationclass = dat$lc,
             timevarybeta=tvb,
             verbose=FALSE,
             fixcorrection=FALSE,
             equalbetas=FALSE,
             errordistribution="n",
             movementmodel="ctcrw",
             timeunit="hours"
             )
fittvne <- do.call(argosTrack,args)
resid_tvne <- residuals(fittvne,osamethod=osam)

args <- list(lon = dat$lon + rnorm(length(dat$lon),0,0.00001),
             lat = dat$lat + rnorm(length(dat$lon),0,0.00001),
             dates = as.character(dat$date),
             locationclass = dat$lc,
             timevarybeta=tvb,
             verbose=FALSE,
             fixcorrection=FALSE,
             equalbetas=FALSE,
             fixgamma=FALSE,
             errordistribution="n",
             movementmodel="ctcrw",
             timeunit="hours"
             )
fittvned <- do.call(argosTrack,args)
resid_tvned <- residuals(fittvned,osamethod=osam)


                                        # Fit with normal distribution - mixed memory continuous time correlated random walk movement model
args <- list(lon = dat$lon + rnorm(length(dat$lon),0,0.00001),
             lat = dat$lat + rnorm(length(dat$lon),0,0.00001),
             dates = as.character(dat$date),
             locationclass = dat$lc,
             verbose=FALSE,
             fixcorrection=FALSE,
             errordistribution="n",
             movementmodel="mmctcrw",
             timeunit="hours"
             )
fitmmctcrw <- do.call(argosTrack,args)
resid_mmctcrw <- residuals(fitmmctcrw,osamethod=osam)


args <- list(lon = dat$lon + rnorm(length(dat$lon),0,0.00001),
             lat = dat$lat + rnorm(length(dat$lon),0,0.00001),
             dates = as.character(dat$date),
             locationclass = dat$lc,
             verbose=FALSE,
             fixcorrection=FALSE,
             equalbetas = FALSE,
             errordistribution="n",
             movementmodel="mmctcrw",
             timeunit="hours"
             )
fitmmctcrwne <- do.call(argosTrack,args)
resid_mmctcrwne <- residuals(fitmmctcrwne,osamethod=osam)

args <- list(lon = dat$lon + rnorm(length(dat$lon),0,0.00001),
             lat = dat$lat + rnorm(length(dat$lon),0,0.00001),
             dates = as.character(dat$date),
             locationclass = dat$lc,
             verbose=FALSE,
             fixcorrection=FALSE,
             equalbetas = FALSE,
             fixgamma = FALSE,
             errordistribution="n",
             movementmodel="mmctcrw",
             timeunit="hours"
             )
fitmmctcrwned <- do.call(argosTrack,args)
resid_mmctcrwned <- residuals(fitmmctcrwned,osamethod=osam)


                                        # Fit with normal distribution - random walk movement model
args <- list(lon = dat$lon + rnorm(length(dat$lon),0,0.00001),
             lat = dat$lat + rnorm(length(dat$lon),0,0.00001),
             dates = as.character(dat$date),
             locationclass = dat$lc,
             verbose=FALSE,
             fixcorrection=FALSE,
             errordistribution="n",
             movementmodel="rw",
             timeunit="hours"
             )
fitrw <- do.call(argosTrack,args)
resid_rw <- residuals(fitrw,osamethod=osam)


pdf("~/Skrivebord/residuals.pdf",width=20,height=20)

if(exists("resid_rw")){
    plot.resid(resid_rw,fitrw)
    title("Random Walk",outer=TRUE,line=-1)
}

if(exists("resid_ctcrw")){
    plot.resid(resid_ctcrw,fitctcrw)
    title("Continuous-Time Correlated Random Walk (equal betas)",outer=TRUE,line=-1)
}

if(exists("resid_ctcrwne")){
    plot.resid(resid_ctcrwne,fitctcrwne)
    title("Continuous-Time Correlated Random Walk (not equal betas)",outer=TRUE,line=-1)
}

if(exists("resid_ctcrwned")){
    plot.resid(resid_ctcrwned,fitctcrwned)
    title("Continuous-Time Correlated Random Walk (not equal betas; drift)",outer=TRUE,line=-1)
}


if(exists("resid_tv")){
    plot.resid(resid_tv,fittv)
    title("Timevarying Continuous-Time Correlated Random Walk (equal betas)",outer=TRUE,line=-1)

}


if(exists("resid_tvne")){
    plot.resid(resid_tvne,fittvne)
    title("Timevarying Continuous-Time Correlated Random Walk (not equal betas)",outer=TRUE,line=-1)
}

if(exists("resid_tvned")){
    plot.resid(resid_tvned,fittvned)
    title("Timevarying Continuous-Time Correlated Random Walk (not equal betas; drift)",outer=TRUE,line=-1)
}


if(exists("resid_mmctcrw")){
    plot.resid(resid_mmctcrw,fitmmctcrw)
    title("Double Continuous-Time Correlated Random Walk (equal betas)",outer=TRUE,line=-1)
}

if(exists("resid_mmctcrwne")){
    plot.resid(resid_mmctcrwne,fitmmctcrwne)
    title("Double Continuous-Time Correlated Random Walk (not equal betas)",outer=TRUE,line=-1)
}

if(exists("resid_mmctcrwned")){
    plot.resid(resid_mmctcrwned,fitmmctcrwned)
    title("Double Continuous-Time Correlated Random Walk (not equal betas; drift)",outer=TRUE,line=-1)
}


dev.off()






















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


    ## Get dtStates
    if(movementmodel %in% c("rw","ctcrw","mmctcrw")){
        dtStates <- dates[dates>0]
    }else{
        ## no cases yet
    }

    ## Get previous state
    if(movementmodel %in% c("rw","ctcrw","mmctcrw")){
        nonZerodt <- which(dates != 0)
        prevState <- sapply(1:length(lon),
                            function(i) max((1:length(nonZerodt))[nonZerodt <= i]))
    }else{
        ## no cases yet
    }

    ## Get stateFrac
    if(movementmodel %in% c("rw","ctcrw","mmctcrw")){
        stateFrac <- rep(1,length(lon))
    }else{
        ## no cases yet
    }

    
    dat <- list(lon = lon,
                lat = lat,
                dtStates = dtStates,
                prevState = prevState - 1,
                stateFrac = stateFrac,
                qual = locclassfactor,
                include = as.numeric(include),
                minDf = minDf,
                moveModelCode = modelCodeNum,
                nauticalObs = 0,
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
                           ncol=length(dat$dtStates)),
                       logSdState = rep(0,numStates[2]),
                       logSdObs = c(0,0),
                       logCorrection = logCorrect,
                       gamma = rep(0,numStates[1]),
                       mu = matrix(0,
                           nrow=2,
                           ncol=length(dat$dtStates)),
                       vel = matrix(0,
                           nrow=numStates[1],
                           ncol=length(dat$dtStates))
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
                      ncol=length(dat$dtStates))
    }else{
        mbe_vals <- rep(1:timevarybeta,
                        each=ceiling(length(dat$dtStates)/timevarybeta))
        mbe <- matrix(rep(mbe_vals,numStates[1]),
                      nrow=numStates[1],
                      ncol=length(dat$dtStates),
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
    
    if(movementmodel == "mmctcrw"){
        map$gamma <- factor(c(1,2,NA,NA))  # Drift in the slow process
        tt <- cumsum(dat$dt)
        parameters$logbeta <- matrix(c(-2,-2,0,0),
                           nrow=numStates[1],
                           ncol=length(dat$dtStates))
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

    return(obj)
    
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
    res$positions <- expandMu(esttrack,dates)
    res$positions_sd <- expandMu(sdtrack,dates)
    rownames(res$positions) <- c("latitude","longitude")
    res$optimization <- opt
    res$estimation_time <- esttime
    res$tmb_object <- obj
    res$sdreport_summary <- srep

    return(res)
    
}

                       


plot.argostrack <- function(x,bg_style="none",only_map = FALSE,min_area = 0.01,zoom_to_obs=TRUE, ...){
    object <- x
    srep <- object$sdreport_summary
    track <- srep[rownames(srep)=="mu",]
    sdtrack <- matrix(track[,2],nrow=2)
    esttrack <- matrix(track[,1],nrow=2)
    obs <- object$observations
    dates <- object$dates

    if(is.character(dates)){
        dates <- as.POSIXct(dates)      
    }

    dt <- diff(dates)

    if(!only_map)
        layout(matrix(c(1,1,2,3),ncol=2))

    if(zoom_to_obs){
        xrng <- c(min(obs[2,])-0.2, max(obs[2,])+0.2)
        yrng <- c(min(obs[1,])-0.2, max(obs[1,])+0.2)
    }else{
        xrng <- c(min(esttrack[2,])-0.2, max(esttrack[2,])+0.2)
        yrng <- c(min(esttrack[1,])-0.2, max(esttrack[1,])+0.2)
    }
    
    if(bg_style=="none"){
        
        plot(obs[2,],obs[1,],type="l",lty=2,col="grey",
             xlim=xrng,
             ylim=yrng,
             asp=1/cos((mean(yrng) * pi) / 180),
             xlab = expression(paste("Longitude (",degree,")",sep="")),
             ylab = expression(paste("Latitude (",degree,")",sep="")))
        lines(esttrack[2,],esttrack[1,])
        
    }else if(bg_style=="map"){ 
        data('worldShorelines',package="argosTrack")
        data('worldShorelinesArea',package="argosTrack")
        plot(NA, xlim=xrng, ylim=yrng,asp=1/cos((mean(yrng) * pi) / 180),
             xlab = expression(paste("Longitude (",degree,")",sep="")),
             ylab = expression(paste("Latitude (",degree,")",sep="")))
        # Need faster way to plot the polygons
        invisible(lapply(worldShorelines[worldShorelinesArea>min_area],function(x){
            polygon(x[,1],x[,2],col=grey(0.8),border=NA)
        }))
        box()
        lines(obs[2,],obs[1,],type="l",lty=2,col=grey(0.5))
        lines(esttrack[2,],esttrack[1,])

    }else{
        stop("Background style is not valid.")
    }

    if(!only_map){
        plot(dates,obs[2,],pch=16,col="grey",
             xlab = "Date",
             ylab =  expression(paste("Longitude (",degree,")",sep="")))
        lines(dates[dt>0],esttrack[2,])
        lines(dates[dt>0],esttrack[2,]+2*sdtrack[2,],lty=3)
        lines(dates[dt>0],esttrack[2,]-2*sdtrack[2,],lty=3)
        
        plot(dates,obs[1,],pch=16,col="grey",
             xlab = "Date",
             ylab =  expression(paste("Latitude (",degree,")",sep="")))
        lines(dates[dt>0],esttrack[1,])
        lines(dates[dt>0],esttrack[1,]+2*sdtrack[1,],lty=3)
        lines(dates[dt>0],esttrack[1,]-2*sdtrack[1,],lty=3)
    }

}

#' @export
plot.argostrack_bootstrap <- function(x, vertical = TRUE, ...){
    object <- x
    msearray <- object$mse
    pdatlat <- data.frame(V1 = object$mse[1,,1])
    pdatlon <- data.frame(V1 = object$mse[2,,1])

    if(dim(msearray)[3]>1){
        for(i in 1:dim(msearray)[3]){
            pdatlat[,i] <- object$mse[1,,i]
            pdatlon[,i] <- object$mse[2,,i]
        }
    }
    dnam <- dimnames(object$mse)
    if(!is.null(dnam)){
        if(!is.null(dnam[[3]])){
            colnames(pdatlat) <- dnam[[3]]
            colnames(pdatlon) <- dnam[[3]]
        }
    }
    #if(!is.null(names)){
    #    colnames(pdatlat) <- names
    #    colnames(pdatlon) <- names
    #}

    if(vertical){
        layout(matrix(c(1,2),ncol=1))
    }else{
        layout(matrix(c(1,2),nrow=1))
    }
    boxplot(pdatlon,na.rm=TRUE,main=NULL,
            ylab=expression(paste("MSE for estimates, Longitude (",degree,")",sep="")),...)
    boxplot(pdatlat,na.rm=TRUE,main=NULL,
            ylab=expression(paste("MSE for estimates, Latitude (",degree,")",sep="")),...)
}
