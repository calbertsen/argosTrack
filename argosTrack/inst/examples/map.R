library(argosTrack)





d <- subadult_ringed_seal

obs <- Observation(lon = d$lon,
                   lat = d$lat,
                   dates = as.POSIXct(d$date),
                   locationclass = d$lc
                   )

anim <- Animal(name = as.character(d$id[1]),
               observation = obs,
               movement =  argosTrack:::RW(unique(obs$dates),
                                 timeunit = "hours"),
               measurement = Measurement(model="n"))
fitTrack(anim, nlminb.control=list(iter.max=1000,eval.max=1000))

anim

library(rworldxtra)
library(sp)

data("countriesHigh", package = "rworldxtra")
rng <- anim$getRange()
plotMap(anim, args=list(type="n"), obsArgs = list(type="n"))
sp::plot(countriesHigh, add = TRUE, col="grey",border=NA)
plotMap(anim,add=TRUE, obsArgs=list(pch=16,cex=0.5,col=rgb(0,0,1,0.3)))
box()
