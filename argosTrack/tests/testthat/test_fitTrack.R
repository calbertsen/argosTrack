
context("Test that fitTrack works")

test_that("method = mcmc gives error",
{
    mod <- Animal(measurement=Measurement(model="n"),
           movement=RW(as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60),observation=Observation(lon=rep(0,100),lat=rep(0,100),locationclass=rep("GPS",100),dates=as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60),name="TestAnim")
    sim <- simTrack(mod,1)[,1]$Animal
    expect_error(fitTrack(sim,method="mcmc"))
})

test_that("default fitTrack works and updates parameters; should give output",
{
    mod <- Animal(measurement=Measurement(model="n"),
           movement=RW(as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60),observation=Observation(lon=rep(0,100),lat=rep(0,100),locationclass=rep("GPS",100),dates=as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60),name="TestAnim")
    sim <- simTrack(mod,1)[,1]$Animal
    oldPar <- sim$getTMBparameters()
    ## Before fitting, the optim list should be empty
    expect_equal(length(sim$optim),0)    
    expect_output(fit<-fitTrack(sim))
    expect_equivalent(class(fit),"fittedTrack")
    newPar <- sim$getTMBparameters()
    comp <- compare(oldPar,newPar)
    expect_false(comp$equal)
    ## After fitting, the optim list should not be empty
    expect_gt(length(sim$optim),0)
    ## Animal$show after fitTrack
    expect_gt(length(capture.output(sim)),0)
})

test_that("fitTrack silent works",
{
    mod <- Animal(measurement=Measurement(model="n"),
           movement=RW(as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60),observation=Observation(lon=rep(0,100),lat=rep(0,100),locationclass=rep("GPS",100),dates=as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60),name="TestAnim")
    sim <- simTrack(mod,1)[,1]$Animal
    oldPar <- sim$getTMBparameters()
    expect_silent(fit<-fitTrack(sim,silent=TRUE))
})
