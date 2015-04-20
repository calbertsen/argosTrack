
library(devtools)
install_github("calbertsen/argosTrack",ref="onestepresid")

library(argosTrack)

dat <- subadult_ringed_seal

# Fit with normal distribution - advanced movement model
args <- list(lon = dat$lon,
             lat = dat$lat,
             dates = as.character(dat$date),
             locationclass = dat$lc,
             verbose=FALSE,
             errordistribution="n"
             )
fitobjn <- do.call(argosTrack,args)
