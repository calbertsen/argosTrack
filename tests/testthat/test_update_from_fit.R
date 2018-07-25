context("updateFromFit method")

test_that("updateFromFit works without sdList",
{
    skip_if_not_installed("TMB")
    mod <- Animal(measurement=Measurement(model="n"),
                  movement=RW(as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60),observation=Observation(lon=rep(0,100),lat=rep(0,100),locationclass=rep("GPS",100),dates=as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60),name="TestAnim")
    obj <- TMB::MakeADFun(mod$getTMBdata(),mod$getTMBparameters(),mod$getTMBmap(),
                          random = c("mu","vel"),
                          DLL = "argosTrack",
                          checkParameterOrder=FALSE)
    he <- diag(length(obj$par))
    rownames(he) <- colnames(he) <- names(obj$par)
    pl <- obj$env$parList(par = runif(length(obj$env$last.par)))
    expect_silent(mod$movement$updateFromFit(pl,he))
    expect_equivalent(pl$movePars,mod$movement$parameters)
    expect_equivalent(pl$logSdState,mod$movement$varianceParameters)
    expect_equivalent(pl$mu,mod$movement$mu)
    expect_equivalent(pl$vel,mod$movement$vel)
    expect_equivalent(solve(he)[rownames(he) %in% c("movePars","logSdState"),rownames(he) %in% c("movePars","logSdState")],mod$movement$vcov)
    expect_equivalent(NA * pl$mu,mod$movement$sdmu)
    expect_equivalent(NA * pl$vel,mod$movement$sdvel)
})

test_that("updateFromFit works with sdList",
{
    skip_if_not_installed("TMB")
    mod <- Animal(measurement=Measurement(model="n"),
                  movement=RW(as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60),observation=Observation(lon=rep(0,100),lat=rep(0,100),locationclass=rep("GPS",100),dates=as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60),name="TestAnim")
    obj <- TMB::MakeADFun(mod$getTMBdata(),mod$getTMBparameters(),mod$getTMBmap(),
                          random = c("mu","vel"),
                          DLL = "argosTrack",
                          checkParameterOrder=FALSE)
    he <- diag(length(obj$par))
    rownames(he) <- colnames(he) <- names(obj$par)
    pl <- obj$env$parList(par = runif(length(obj$env$last.par)))
    expect_silent(mod$movement$updateFromFit(pl,he,pl))
    expect_equivalent(pl$movePars,mod$movement$parameters)
    expect_equivalent(pl$logSdState,mod$movement$varianceParameters)
    expect_equivalent(pl$mu,mod$movement$mu)
    expect_equivalent(pl$vel,mod$movement$vel)
    expect_equivalent(solve(he)[rownames(he) %in% c("movePars","logSdState"),rownames(he) %in% c("movePars","logSdState")],mod$movement$vcov)
    expect_equivalent(pl$mu,mod$movement$sdmu)
    expect_equivalent(pl$vel,mod$movement$sdvel)
})
