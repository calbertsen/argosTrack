#### Roseplot function

## roseplot for object function works
mod <- Animal(measurement=Measurement(model="n"),
              movement=RW(as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60),observation=Observation(lon=rep(0,100),lat=rep(0,100),locationclass=rep("GPS",100),dates=as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60),name="TestAnim")
sim <- simTrack(mod,1)[,1]$Animal
gives_no_output(roseplot(sim))
gives_no_output(roseplot(sim,type="step"))
gives_no_output(roseplot(sim,type="angle"))
gives_no_output(roseplot(sim,bearings=TRUE))
is_true(is.list(roseplot(sim)))
is_true(is.list(argosTrack:::shortfrac(2,1,"pi")))

## .roseplot works
xx <- runif(1000,-pi,pi)
gives_no_output(argosTrack:::.roseplot(xx))
gives_no_output(argosTrack:::.roseplot(xx,prob = FALSE))
gives_no_output(argosTrack:::.roseplot(xx, breaks = 100))
gives_no_output(argosTrack:::.roseplot(xx,breaks = "FD"))
gives_no_output(argosTrack:::.roseplot(xx,breaks = "scott"))
gives_error(argosTrack:::.roseplot(xx,breaks = "error"))

