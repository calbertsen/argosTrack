library(argosTrack)

n <- 2000

dates <- Sys.time() + 24*60*60 * (0:n)

mov <- argosTrack:::IDCRW(dates,pars = c(1,1,0.5,0,0,0),varPars = c(-1,-1), timeunit="day")
X <- mov$simulate()

plot(t(X),type="l")

Y <- argosTrack:::rmvsh(n+1,c(0,0),diag(0.3,2),3)
Z <- X+Y

obs <- Observation(lat = Z[1,],
                   lon = Z[2,],
                   dates=dates,
                   locationclass = rep("GPS",n+1))

plot(obs)

anim <- Animal(obs,
               argosTrack:::IDCRW(dates, timeunit="day"),
               Measurement(model = "n"))

fitTrack(anim, equaldecay = TRUE, fixdrift = TRUE)

plot(anim)
anim

args <- list(anim)
graphics::layout(matrix(c(rep(1,4*3),2,2,3,3,2,2,3,3),nrow=4))
names(args)[1] <- "object"
do.call("plotMap",args)
lines(t(X)[,2:1],col="blue")
do.call("plotLat",args)
lines(dates,t(X)[,1],col="blue")
do.call("plotLon",args)
lines(dates,t(X)[,2],col="blue")


anim$movement$vcov
