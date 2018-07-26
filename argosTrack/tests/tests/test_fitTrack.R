
#### Test that fitTrack works

## method = mcmc gives error
mod <- Animal(measurement=Measurement(model="n"),
              movement=RW(as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60),observation=Observation(lon=rep(0,100),lat=rep(0,100),locationclass=rep("GPS",100),dates=as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60),name="TestAnim")
sim <- simTrack(mod,1)[,1]$Animal
gives_error(fitTrack(sim,method="mcmc"))

## default fitTrack works and updates parameters; should give output
mod <- Animal(measurement=Measurement(model="n"),
              movement=RW(as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60),observation=Observation(lon=rep(0,100),lat=rep(0,100),locationclass=rep("GPS",100),dates=as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60),name="TestAnim")
sim <- simTrack(mod,1)[,1]$Animal
oldPar <- sim$getTMBparameters()
## Before fitting, the optim list should be empty
is_identical(length(sim$optim),0L)    
gives_output(fit<-fitTrack(sim))
is_equal(class(fit),"fittedTrack")
newPar <- sim$getTMBparameters()
is_not_equal(oldPar,newPar)
## After fitting, the optim list should not be empty
is_true(length(sim$optim) > 0)
## Animal$show after fitTrack
gives_output(sim)

## fitTrack silent works",
mod <- Animal(measurement=Measurement(model="n"),
              movement=RW(as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60),observation=Observation(lon=rep(0,100),lat=rep(0,100),locationclass=rep("GPS",100),dates=as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60),name="TestAnim")
sim <- simTrack(mod,1)[,1]$Animal
oldPar <- sim$getTMBparameters()
gives_no_output(fit<-fitTrack(sim,silent=TRUE))

