
check_movement_model_consistency <- function(mov){

    if(require(TMB, quietly=TRUE) & !check_env_var("TRAVIS")){
        nobs <- 20
        nsim <- 1000
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
                                  silent = TRUE,
                                  DLL = "argosTrack")
            ## list(gr=obj$gr(obj0$par)#,
            ##      #he=obj$he(obj0$par)
            ##      )
            return(as.vector(obj$gr(obj$par)))
        }
        vals <- replicate(nsim,getGrHe())
        ## mu <- rowMeans(vals)
        ## H <- var(t(vals))
        ## iH <- solve(H)
        ## mu.scaled <- sqrt(nsim) * mu
        ## q <- as.vector( t(mu.scaled) %*% iH %*% mu.scaled )
        ## p.value <- 1 - pchisq(q, df=length(mu))
        ## bias <- -iH %*% mu
        ##expect_true(p.value > 0.05)
        is_true(all(apply(vals,1,function(x)quantile(x,.4) < 0 & quantile(x,0.6) > 0)))
        ##expect_true(all(abs(meanGr) < 1))
    }    
}
