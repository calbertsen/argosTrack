
devtools::install_local("~/argosTrack")

## devtools::install_github("calbertsen/argosTrack",ref="timevarying") 

library(argosTrack)

dat <- subadult_ringed_seal[1:100,]
dat <- dat[!(dat$lc=="Z"),]
table(dat$lc)

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

res0 <- residuals(fitctcrw,type="onestep")

# Fit with normal distribution - continuous time correlated random walk movement model
args <- list(lon = dat$lon,
             lat = dat$lat,
             dates = as.character(dat$date),
             locationclass = dat$lc,
             timevarybeta=10,
             verbose=TRUE,
             fixcorrection=TRUE,
             errordistribution="n",
             movementmodel="ctcrw",
             timeunit="hours"
             )
fittv <- do.call(argosTrack,args)

res1 <- residuals(fittv,type="onestep")

par(mfrow=c(2,2))
plot(res0[[1]][1,],main="een beta latitude")
plot(res0[[1]][2,],main="een beta longitude")
plot(res1[[1]][1,],main="flere beta latitude")
plot(res1[[1]][2,],main="flere beta longitude")

# Fit with normal distribution - mixed memory continuous time correlated random walk movement model
args <- list(lon = dat$lon,
             lat = dat$lat,
             dates = as.character(dat$date),
             locationclass = dat$lc,
             verbose=FALSE,
             fixcorrection=TRUE,
             errordistribution="n",
             movementmodel="mmctcrw",
             timeunit="hours"
             )
fitmmctcrw <- do.call(argosTrack,args)

1/cumsum(exp(fitctcrw$optimization$par[1]))
1/cumsum(exp(fitmmctcrw$optimization$par[1:2]))

# Fit with normal distribution - random walk movement model
args <- list(lon = dat$lon,
             lat = dat$lat,
             dates = as.character(dat$date),
             locationclass = dat$lc,
             verbose=FALSE,
             errordistribution="n",
             movementmodel="rw",
             timeunit="hours"
             )
fitrw <- do.call(argosTrack,args)

# See difference in fits
pos<-t(fitctcrw$positions-fitrw$positions)
summary(pos)




system.time(res2 <- residuals(fitmmctcrw,type="onestep"))
