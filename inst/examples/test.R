
library(argosTrack)



d <- subadult_ringed_seal

obs <- Observation(lon = d$lon,
                   lat = d$lat,
                   dates = as.POSIXct(d$date),
                   locationclass = d$lc
                   )

meas <- Measurement(model="n")
meas2 <- Measurement(model="sh")
meas3 <- Measurement(model="t")


Yn <- meas$simulate(obs)
Ysh <- meas2$simulate(obs)
Yt <- meas3$simulate(obs)


indx <- which(obs$qual == "B")

par(mfrow=c(3,2))
hist(Yn[1,indx],nclass=35,prob=TRUE)
hist(Yn[2,indx],nclass=35,prob=TRUE)
hist(Ysh[1,indx],nclass=35,prob=TRUE)
hist(Ysh[2,indx],nclass=35,prob=TRUE)
hist(Yt[1,indx],nclass=35,prob=TRUE)
hist(Yt[2,indx],nclass=35,prob=TRUE)


## Different movementmodels

mov <- RW(unique(obs$dates))
mov1 <- DCRW(seq(min(obs$dates),
                  max(obs$dates),
                  "day"),timeunit="days")
mov2 <- argosTrack:::IDCRW(unique(obs$dates),
               timeunit = "hours")
mov3 <- CTCRW(unique(obs$dates),
               timeunit = "hours")
mov4 <- DSBHN(seq(min(obs$dates),
                  max(obs$dates),
                  len=250),
              timeunit="auto")
mov5 <- DSBW(seq(min(obs$dates),
                  max(obs$dates),
                  len=200),
             timeunit="auto")

mov6 <- argosTrack:::OUL(unique(obs$dates),
                         timeunit = "hours")

## xx <- mov$simulate()
## xx1 <- mov1$simulate()
## xx2 <- mov2$simulate()
## xx3 <- mov3$simulate()
## xx4 <- mov4$simulate()
## xx5 <- mov5$simulate()
## xx6 <- mov6$simulate()

anim <- Animal(name = "0",
               observation = obs,
               movement = mov,
               measurement = Measurement(model="n"))
anim1 <- Animal(name = "1",
               observation = obs,
               movement = mov1,
               measurement = Measurement(model="n"))
anim2 <- Animal(name = "2",
               observation = obs,
               movement = mov2,
               measurement = Measurement(model="n"))
anim2t <- Animal(name = "2t",
               observation = obs,
               movement = argosTrack:::IDCRW(unique(obs$dates),
                                 timeunit = "hours"),
               measurement = Measurement(model="t"))
anim3 <- Animal(name = "3",
               observation = obs,
               movement = mov3,
               measurement = Measurement(model="n"))
anim4 <- Animal(name = "4",
               observation = obs,
               movement = mov4,
               measurement = Measurement(model="n"))
anim5 <- Animal(name = "5",
               observation = obs,
               movement = mov5,
               measurement = Measurement(model="n"))
anim6 <- Animal(name = "6",
               observation = obs,
               movement = mov6,
               measurement = Measurement(model="n"))


fitTrack(anim)
fitTrack(anim1)
fitTrack(anim2)
fitTrack(anim2t, fixdrift = TRUE, equaldecay = TRUE, fixcorrection = TRUE)
fitTrack(anim3)
fitTrack(anim4)
fitTrack(anim5)
fitTrack(anim6, symdecay = TRUE, independentdecay = FALSE, equaldecay = TRUE)

anim
anim1
anim2
anim4
anim5

roseplot(anim)
roseplot(anim1)
a <- roseplot(anim2)

plot(anim)

plot(anim2, sd = TRUE)

anim

plotLat(mov4,sd=TRUE)
plotLat(mov,add=TRUE)

plotLat(obs)

plotLat(anim,sd=FALSE)
plotLat(anim,sd=TRUE,add=TRUE)

X <- mov$simulate()
Y <- meas$simulate(obs)
Z <- anim$simulate()

dev.new()
plot(t(Z$Y)); lines(t(Z$X),col="red")


