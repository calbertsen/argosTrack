#### updateFromFit method

## updateFromFit works without sdList
if(require(TMB, quietly = TRUE)){
    mod <- Animal(measurement=Measurement(model="n"),
                  movement=RW(as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60),observation=Observation(lon=rep(0,100),lat=rep(0,100),locationclass=rep("GPS",100),dates=as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60),name="TestAnim")
    obj <- TMB::MakeADFun(mod$getTMBdata(),mod$getTMBparameters(),mod$getTMBmap(),
                          random = c("mu","vel"),
                          DLL = "argosTrack",
                          checkParameterOrder=FALSE)
    he <- diag(length(obj$par))
    rownames(he) <- colnames(he) <- names(obj$par)
    pl <- obj$env$parList(par = runif(length(obj$env$last.par)))
    gives_no_output(mod$movement$updateFromFit(pl,he))
    is_equal(pl$movePars,mod$movement$parameters)
    is_equal(pl$logSdState,mod$movement$varianceParameters)
    is_equal(pl$mu,mod$movement$mu)
    is_equal(pl$vel,mod$movement$vel)
    is_equal(solve(he)[rownames(he) %in% c("movePars","logSdState"),rownames(he) %in% c("movePars","logSdState")],mod$movement$vcov)
    is_equal(NA * pl$mu,mod$movement$sdmu)
    is_equal(NA * pl$vel,mod$movement$sdvel)
}

## updateFromFit works with sdList
if(require(TMB, quietly = TRUE)){
    mod <- Animal(measurement=Measurement(model="n"),
                  movement=RW(as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60),observation=Observation(lon=rep(0,100),lat=rep(0,100),locationclass=rep("GPS",100),dates=as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60),name="TestAnim")
    obj <- TMB::MakeADFun(mod$getTMBdata(),mod$getTMBparameters(),mod$getTMBmap(),
                          random = c("mu","vel"),
                          DLL = "argosTrack",
                          checkParameterOrder=FALSE)
    he <- diag(length(obj$par))
    rownames(he) <- colnames(he) <- names(obj$par)
    pl <- obj$env$parList(par = runif(length(obj$env$last.par)))
    gives_no_output(mod$movement$updateFromFit(pl,he,pl))
    is_equal(pl$movePars,mod$movement$parameters)
    is_equal(pl$logSdState,mod$movement$varianceParameters)
    is_equal(pl$mu,mod$movement$mu)
    is_equal(pl$vel,mod$movement$vel)
    is_equal(solve(he)[rownames(he) %in% c("movePars","logSdState"),rownames(he) %in% c("movePars","logSdState")],mod$movement$vcov)
    is_equal(pl$mu,mod$movement$sdmu)
    is_equal(pl$vel,mod$movement$sdvel)
}

