# NOTE: This example is memory and time consuming to run!

library(argosTrack)
library(TMB)
library(covafillr)

Sys.setenv(TZ='UTC')

compile("movement.cpp", CXXFLAGS=paste("-O3",
                                       covafillr::cxxFlags(),
                                       paste0("-I", system.file("include", package = "argosTrack")))
        )
dyn.load(dynlib("movement"))

## Data
data(subadult_ringed_seal)
d0 <- subadult_ringed_seal
d <- d0[!(d0$lc %in% c("Z")),]

## argosTrack objects
makeObj <- function(nn=Inf,nsi=1){
    obs <- Observation(lon=d$lon,
                       lat=d$lat,
                       dates=as.POSIXct(d$date),
                       locationclass=d$lc)
    ns <- rep(nsi,length.out=2)
    meas <- Measurement(model="t")
    date1 <- unique(sort(c(as.POSIXct(d$date),seq(min(as.POSIXct(d$date)),max(as.POSIXct(d$date)),nn*60*60))))
    mov <- GDCRW(dates=date1)
    anim <- Animal(movement = mov,
                   measurement = meas,
                   observation = obs)
    ## Prepare for TMB
    dat <- anim$getTMBdata()
    date0 <- strptime(strftime(mov$dates,"%y-%m-%d 00:00:00"),"%y-%m-%d %H:%M:%S")
    dat$knots_mu <- as.matrix(expand.grid(seq(min(d$lat),max(d$lat),len=ns[1]),
                                          seq(min(d$lon),max(d$lon),len=ns[2])))
    dat$rep_lat <- seq(min(d$lat),max(d$lat),0.1)
    dat$rep_lon <- seq(min(d$lon),max(d$lon),0.1)
    dat$p <- 3
    dat$h <- covafillr::suggestBandwith(dat$knots_mu,dat$p)
    dat$d <- 2
    pars <- anim$getTMBparameters()
    pars$mu[1,] <- mean(d$lat)
    pars$mu[2,] <- mean(d$lon)
    pars$knotpar_mu_lat <- rnorm(nrow(dat$knots_mu),0,0.001)
    pars$knotpar_mu_lon <- rnorm(nrow(dat$knots_mu),0,0.001)
    pars$logLambdaLat <- -3
    pars$logLambdaLon <- -3
    map <- anim$getTMBmap(fixcorrection=FALSE,fixdrift=TRUE,equaldecay=FALSE,fixrotation=FALSE,fixmovecor=FALSE)
    obj <- MakeADFun(dat,pars,map,
                     random=c("mu","knotpar_mu_lat","knotpar_mu_lon"),
                     inner.control = list(maxit = 100),
                     DLL = "movement")
    obj
}

obj <- makeObj(1,c(15,15))

obj$fn()
rp0 <- obj$report(par=obj$env$last.par.best)

opt <- nlminb(obj$par,obj$fn,obj$gr,control=list(iter.max=1000,eval.max=1000))

rp <- obj$report(par=obj$env$last.par.best)



#######################
#### REPORT RESULT ####
#######################

filled.contour(obj$env$data$rep_lon,obj$env$data$rep_lat,t(rp$muLonOut),color.palette=colorRampPalette(c("blue","white","red")),nlevels=100,main="Lon local mean velocity",
               plot.axes = {
                   lines(rp$slon,rp$slat)
                   points(obj$env$data$knots_mu[,2:1],col=rgb(1,0,0,0.5),pch=16)
               })

filled.contour(obj$env$data$rep_lon,obj$env$data$rep_lat,t(rp$muLatOut),color.palette=colorRampPalette(c("blue","white","red")),nlevels=100,main="Lat mean velocity",
               plot.axes = {
                   lines(rp$slon,rp$slat)
                   points(obj$env$data$knots_mu[,2:1],col=rgb(1,0,0,0.5),pch=16)
})

#############################
#### GET STANDARD ERRORS ####
#############################


hess <- optimHess(opt$par,obj$fn,obj$gr)

sr <- sdreport(obj,par.fixed=opt$par,hessian.fixed=hess)
sdr <- summary(sr)

pl <- obj$env$parList(par=obj$env$last.par.best)


##############
#### Save ####
##############

save(obj,opt,rp,sr,sdr,hess,pl,file="res_case_study.RData")

