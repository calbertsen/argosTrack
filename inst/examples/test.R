
library(argosTrack)



d <- subadult_ringed_seal

obs <- Observation(lon = d$lon,
                   lat = d$lat,
                   dates = as.POSIXct(d$date),
                   locationclass = d$lc
                   )
meas <- Measurement(model="n")


## Different movementmodels

mov <- RW(unique(obs$dates))
mov1 <- DTCRW(seq(min(obs$dates),
                  max(obs$dates),
                  "day"),timeunit="day")
mov2 <- IDTCRW(unique(obs$dates),
               timeunit = "hour")
mov3 <- CTCRW(unique(obs$dates),
               timeunit = "hour")
mov4 <- DSBHN(seq(min(obs$dates),
                  max(obs$dates),
                  len=250),
              timeunit="auto")
mov5 <- DSBW(seq(min(obs$dates),
                  max(obs$dates),
                  len=200),
              timeunit="auto")

xx <- mov$simulate()
xx1 <- mov1$simulate()
xx2 <- mov2$simulate()
xx3 <- mov3$simulate()
xx4 <- mov4$simulate()
xx5 <- mov5$simulate()

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
               movement = IDTCRW(unique(obs$dates),
                                 timeunit = "hour"),
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

fitTrack(anim)
fitTrack(anim1)
fitTrack(anim2)
fitTrack(anim2t)
fitTrack(anim3)
fitTrack(anim4)
fitTrack(anim5)

anim2
fitTrack(anim2,equaldrift=TRUE)
anim2

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

plot(Z4$Animal)

dev.new()
plot(Z2$Animal$observation$lon,
     Z2$Animal$observation$lat)

lines(t(Z2$Animal$movement$mu[2:1,]),col="red")



Z4 <- anim4$simulate(newObject = TRUE)
fitTrack(Z4$Animal)
plot(Z4$Animal)

cov2cor(solve(anim5$optim$hessian))




