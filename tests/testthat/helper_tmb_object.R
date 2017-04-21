
check_tmb_object <- function(meas,mov,lc,nobs){

    test_that(sprintf("TMB object can be created %s-%s-%s-%s",meas,mov,lc,nobs),
    {
        skip_if_not_installed("TMB")
        skip_on_travis()
        lcc <- switch(lc,
                      "argos"=sample(c("3","2","1","0","A","B","Z"),nobs,replace=TRUE),
                      "gps"=rep("GPS",nobs),
                      "spline"=rep("S",nobs),
                      "known"=rep("K",nobs),
                      "combi"=sample(c("3","2","1","0","A","B","Z","S","K","GPS"),nobs,replace=TRUE)
                      )
        
        expr <- sprintf('Animal(measurement=Measurement(model="%s"),movement=%s(as.POSIXct("2017-01-01 00:00:00") + (1:%s) * 60 * 60),observation=Observation(lon=rep(0,%s),lat=rep(0,%s),locationclass=%s,dates=as.POSIXct("2017-01-01 00:00:00") + (1:%s) * 60 * 60),name="TestAnim")',
                        meas,mov,nobs,nobs,nobs,paste0(deparse(lcc),collapse=""),nobs)
        mod <- eval(parse(text=expr))
        sim <- simTrack(mod,1)[,1]$Animal
        dat <- sim$getTMBdata()
        par <- sim$getTMBparameters()
        map <- sim$getTMBmap()
        expect_true(is.list(TMB::MakeADFun(data = dat,
                                     parameters = par,
                                     map = map,
                                     random = c("mu","vel"),
                                     DLL = "argosTrack",
                                     checkParameterOrder=FALSE)))
        ## Nautical
        mod$measurement$nauticalObs <- TRUE
        mod$movement$nauticalStates <- TRUE
        sim <- simTrack(mod,1)[,1]$Animal
        dat <- sim$getTMBdata()
        par <- sim$getTMBparameters()
        map <- sim$getTMBmap()
        expect_true(is.list(TMB::MakeADFun(data = dat,
                                     parameters = par,
                                     map = map,
                                     random = c("mu","vel"),
                                     DLL = "argosTrack",
                                     checkParameterOrder=FALSE)))
                          
    })
}
