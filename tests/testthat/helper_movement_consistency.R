
check_movement_model_consistency <- function(mov){

    context(sprintf("%s movement model - consistency",mov))
 
    test_that(sprintf("%s movement model - gradient is zero at true value",mov),
    {
        skip_if_not_installed("TMB")
        ##skip_if_not_installed("numDeriv")
        skip_on_cran()
        nobs <- 20
        expr <- sprintf('Animal(measurement=Measurement(),movement=%s(as.POSIXct("2017-01-01 00:00:00") + (1:%s) * 60 * 60),observation=Observation(lon=rep(0,%s),lat=rep(0,%s),locationclass=rep("GPS",%s),dates=as.POSIXct("2017-01-01 00:00:00") + (1:%s) * 60 * 60),name="TestAnim")',mov,nobs,nobs,nobs,nobs,nobs)
        mod <- eval(parse(text=expr))
        getGrHe <- function(){
            sim <- simTrack(mod,1)[,1]
            tra <- sim$Animal
            tra$movement$mu <- sim$X[1:2,]
            if(nrow(sim$X) > 2)
                tra$movement$vel <- sim$X[-(1:2),,drop=FALSE]
            map <- tra$getTMBmap()
            ## mu and vel should be treated as data in the joint model
            map$mu <- factor(rep(NA,length(tra$movement$mu)))
            map$vel <- factor(rep(NA,length(tra$movement$vel)))
            obj <- TMB::MakeADFun(data = tra$getTMBdata(),
                                  parameters = tra$getTMBparameters(),
                                  map = map,
                                  DLL = "argosTrack")
            ## list(gr=obj$gr(obj0$par)#,
            ##      #he=obj$he(obj0$par)
            ##      )
            return(as.vector(obj$gr(obj$par)))
        }
        vals <- replicate(1000,getGrHe())
        meanGr <- rowMeans(vals)
        ## res <- vals[2,1]$he
        ## for(i in 2:ncol(vals))
        ##     res <- res + vals[2,i]$he
        ## res <- res / ncol(vals)
        ## R <- diag(1,length(meanGr))
        ## testv <- t(R %*% meanGr) %*% solve(t(R)%*%solve(res)%*%R) %*% (R %*% meanGr)
        expect_true(all(apply(vals,1,function(x)quantile(x,.4) < 0 & quantile(x,0.6) > 0)))
        ##expect_true(all(abs(meanGr) < 1))
    })
    
}
