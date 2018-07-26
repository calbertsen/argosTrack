
#### Test TMB object

for(meas in c("n","t","sh"))
    for(lc in c("argos","gps","spline","known","combi"))
        for(mm in moveNamesUse){
            check_tmb_object(meas=meas,
                             mov = mm,
                             lc=lc,
                             nobs=100)
        }

## Wrong movement model code gives error
mod <- Animal(measurement=Measurement(model="n"),
              movement=RW(as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60),observation=Observation(lon=rep(0,100),lat=rep(0,100),locationclass=rep("GPS",100),dates=as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60),name="TestAnim")
sim <- simTrack(mod,1)[,1]$Animal
dat <- sim$getTMBdata()
par <- sim$getTMBparameters()
map <- sim$getTMBmap()
dat$moveModelCode <- -1
gives_error(is.list(TMB::MakeADFun(data = dat,
                                   parameters = par,
                                   map = map,
                                   random = c("mu","vel"),
                                   DLL = "argosTrack",
                                   checkParameterOrder=FALSE)))
