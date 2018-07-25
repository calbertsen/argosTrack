context("Roseplot function")

test_that("roseplot for object function works",
{
    mod <- Animal(measurement=Measurement(model="n"),
           movement=RW(as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60),observation=Observation(lon=rep(0,100),lat=rep(0,100),locationclass=rep("GPS",100),dates=as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60),name="TestAnim")
    sim <- simTrack(mod,1)[,1]$Animal
    expect_silent(roseplot(sim))
    expect_silent(roseplot(sim,type="step"))
    expect_silent(roseplot(sim,type="angle"))
    expect_silent(roseplot(sim,bearings=TRUE))
    expect_true(is.list(roseplot(sim)))
    expect_true(is.list(argosTrack:::shortfrac(2,1,"pi")))
})


test_that(".roseplot works",
{
    xx <- runif(1000,-pi,pi)
    expect_silent(argosTrack:::.roseplot(xx))
    expect_silent(argosTrack:::.roseplot(xx,prob = FALSE))
    expect_silent(argosTrack:::.roseplot(xx, breaks = 100))
    expect_silent(argosTrack:::.roseplot(xx,breaks = "FD"))
    expect_silent(argosTrack:::.roseplot(xx,breaks = "scott"))
    expect_error(argosTrack:::.roseplot(xx,breaks = "error"))
})
