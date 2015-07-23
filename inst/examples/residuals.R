
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
tvb <- 35
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


