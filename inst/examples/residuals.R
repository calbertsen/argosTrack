
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
                       nStates = NULL,
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
    movModNames <- c("rw","ctcrw","mmctcrw","dtcrw","dsb")
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
        stateTimeStamp <- if(is.numeric(dates_in)){ dates_in }else{ as.POSIXct(dates_in)}
    }else if(movementmodel %in% c("dsb")){
        dateExtremes <- range(cumsum(dates)-1)
        dtStates <- rep(c(1,(ceiling(dateExtremes[2])-dateExtremes[1])/(nStates-1)),times=c(1,nStates-1))
        stateTimeStamp <- if(is.numeric(dates_in)){ dates_in[1]+cumsum(dtStates)-1 }else{ as.POSIXct(dates_in[1]) + as.difftime(cumsum(dtStates)-1, units = timeunit)}
    }else{

    }

    ## Get previous state
    if(movementmodel %in% c("rw","ctcrw","mmctcrw")){
        nonZerodt <- which(dates != 0)
        prevState <- sapply(1:length(lon),
                            function(i) max((1:length(nonZerodt))[nonZerodt <= i]))
    }else if(movementmodel %in% c("dsb")){
        dateObs <- cumsum(dates)
        dateStates <- cumsum(dtStates)
        prevState <- sapply(1:length(lon),
                            function(i) max((1:length(dateStates))[dateStates <= dateObs[i]]))
        ## dateStates <- dateStates[1:(max(prevState)+1)]
        ## dtStates <- dtStates[1:(max(prevState)+1)]
    }

    ## Get stateFrac
    if(movementmodel %in% c("rw","ctcrw","mmctcrw")){
        stateFrac <- rep(1,length(lon))
    }else{
        stateFrac <- sapply(1:length(lon),
                            function(i) 1 - (dateObs[i] - dateStates[prevState[i]]) / diff(dateStates[prevState[i] + 0:1]))
        ## if(stateFrac[length(stateFrac)] == 1)
        ##     dateStates <- dateStates[-length(dateStates)]
        ## if(max(prevState) < length(
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
                nauticalObs = 1,
                timevary = as.integer(timevarybeta>1)
                )

    ## numStates <- ifelse(movementmodel == "mmctcrw",4,2)
    if(movementmodel == "mmctcrw"){
        numStates <- c(4,4)
    }else{
        numStates <- c(2,2)
    }


    if(movementmodel == "dsb"){
        y <- t(apply(cbind(lon,lat),1,
                   function(x)c(x[1]*60*cos(x[2]*pi/180),x[2]*60)))
        logsinit <- log(sqrt(y[,1] ^ 2 + y[,2] ^ 2))
        phiinit <- atan2(y[,2],y[,1])
        fntmp <- function(par) -sum(dweibull(x=exp(logsinit),
                                             scale=exp(par[1]),
                                             shape=exp(par[2]),
                                             log=TRUE))
        initpars <- nlminb(c(logScale=0,logShape=0),fntmp)$par/2
    }else{
        initpars <- rep(0,numStates[2])
    }
    
    parameters <- list(logbeta = matrix(0,
                           nrow=numStates[1],
                           ncol=length(dat$dtStates)),
                       logSdState = initpars,
                       logSdObs = c(0,0),
                       logCorrection = logCorrect,
                       gamma = rep(0,numStates[1]),
                       mu = matrix(
                          c(0,rnorm((2*length(dat$dtStates)-1)*(movementmodel=="dsb"),0,0.1)),
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
                        each=ceiling(length(dat$dtStates)/timevarybeta))[1:length(dat$dtStates)]
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
    if(movementmodel == "dsb")
        mbe[2,] <- mbe[1,]

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

    if(movementmodel == "dsb"){
        map$vel <- factor(parameters$vel*NA)
        map$gamma <- factor(parameters$gamma*NA)
    }



    parameters$numdata <- length(dat$lon)
    map$numdata <- factor(NA)
    ## if(timevarybeta){
    ##     rnd <- c("mu","vel","logbeta")
    ## }else{
        rnd <- c("mu","vel")
    ## }
    obj <- TMB::MakeADFun(dat,parameters,map,random=rnd,DLL="argosTrack",
                          inner.control = list(LaplaceNonZeroGradient=TRUE))
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
    res$state_dates <- stateTimeStamp
    res$locationclass <- factor(locationclass,levels=argosClasses)
    res$observations <- t(cbind(lat,lon))
    rownames(res$observations) <- c("latitude","longitude")
    ## res$positions <- expandMu(esttrack,dates)
    ## res$positions_sd <- expandMu(sdtrack,dates)
    rownames(res$positions) <- c("latitude","longitude")
    res$optimization <- opt
    res$estimation_time <- esttime
    res$tmb_object <- obj
    res$sdreport_summary <- srep

    return(res)
    
}

                       
dat <- data.frame(lon = cumsum(rnorm(100)+0.5) + rnorm(100,0,1.5),
             lat = cumsum(rnorm(100)+0.5) + rnorm(100,0,1.5),
             date = Sys.time() + as.difftime(0:(100-1),units="hours"),
             lc = rep("3",100))

## Fit with normal distribution - continuous time correlated random walk movement model
args <- list(lon = dat$lon + rnorm(length(dat$lon),0,0.00001),
             lat = dat$lat + rnorm(length(dat$lon),0,0.00001),
             dates = as.character(dat$date),
             locationclass = dat$lc,
             verbose=FALSE,
             fixcorrection=FALSE,
             errordistribution="n",
             movementmodel="dsb",
             timeunit="hours",
             nStates = 350,
             include = rep(TRUE,length(dat$date)),
             equalbetas = TRUE,
             timevarybeta = 1,
             fixgammas = TRUE,
             dfVals = NULL,
             dfMap = NULL,
             minDf = 3.0,
             nlminb.control = list(eval.max=2000,
                 iter.max=1500,
                 rel.tol=1e-3,
                 x.tol=1.5e-2)
             )
attach(args)

plot(lon,lat,type="l")

if(exists("f"))rm(f)
f <- do.call(argosTrack,args)
if(exists("opt"))rm(opt)
opt <- nlminb(f$par,f$fn,f$gr)
opt

plot(t(f$env$parList(par=f$env$last.par.best)$mu),type="l",col="red",ylim=range(lon),xlim=range(lat))
points(lat,lon,pch=".")

xx <- f$env$par[-(1:5)]

fn <- function(x)f$env$f(c(f$env$par[1:5],x))
gr <- function(x)f$env$f(c(f$env$par[1:5],x),order=1)[-(1:5)]
he <- function(x)as.matrix(f$env$spHess(c(f$env$par[1:5],x),random=TRUE))
optin <- nlminb(xx,fn,gr,he,control=list(eval.max=1000,iter.max=1000))
optin
gr(optin$par)

fn(xx)
gr(xx)
he(xx)
