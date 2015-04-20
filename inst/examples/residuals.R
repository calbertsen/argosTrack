
library(devtools)
install_github("calbertsen/argosTrack",ref="onestepresid")

library(argosTrack)

dat <- subadult_ringed_seal

# Fit with normal distribution - continuous time correlated random walk movement model
args <- list(lon = dat$lon,
             lat = dat$lat,
             dates = as.character(dat$date),
             locationclass = dat$lc,
             verbose=FALSE,
             errordistribution="n",
             movementmodel="ctcrw"
             )
fitctcrw <- do.call(argosTrack,args)


# Fit with normal distribution - random walk movement model

args <- list(lon = dat$lon,
             lat = dat$lat,
             dates = as.character(dat$date),
             locationclass = dat$lc,
             verbose=FALSE,
             errordistribution="n",
             movementmodel="rw"
             )
fitrw <- do.call(argosTrack,args)

# See difference in fits
pos<-t(fitctcrw$positions-fitrw$positions)
summary(pos)




