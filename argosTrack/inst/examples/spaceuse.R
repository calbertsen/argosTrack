library(argosTrack)
library(Matrix)

d <- adult_ringed_seal

obs <- Observation(lon = d$lon,
                   lat = d$lat,
                   dates = as.POSIXct(d$date),
                   locationclass = d$lc
                   )

anim <- Animal(name = as.character(d$id[1]),
               observation = obs,
               movement =  argosTrack:::IDCRW(unique(obs$dates),
                                 timeunit = "hours"),
               measurement = Measurement(model="n"))


fitTrack(anim, nlminb.control=list(iter.max=1000,eval.max=1000))


extDates <- seq(min(anim$movement$dates),
                max(anim$movement$dates),
                by = "hours")
allDates <- sort(unique(c(anim$movement$dates,extDates)))
useDates <- which(allDates %in% extDates)
              
newMove <- eval(parse(text=paste0("argosTrack:::",class(anim$movement),
                                  "(allDates,",
                                  "pars=anim$movement$parameters,",
                                  "varPars=anim$movement$varianceParameters,",
                                  "nauticalStates = anim$movement$nauticalStates,",
                                  "timeunit = anim$movement$timeunit",
                                  ")")))

object <- Animal(name = "tmp",
                 observation = anim$observation$copy(),
                 measurement = anim$measurement$copy(),
                 movement = newMove)

dat <- object$getTMBdata()
par <- object$getTMBparameters()
map <- object$getTMBmap(fixcorrection = FALSE)
obj <- TMB::MakeADFun(data = dat,
                      parameters = par,
                      map = map,
                      random = c("mu","vel"),
                      DLL = "argosTrack",
                      inner.control = list(iter.max=1000,eval.max=1000),
                      checkParameterOrder=FALSE,
                      silent = FALSE)
obj$fn()

pall <- obj$env$last.par[obj$env$random]
indx <- which(names(pall)=="mu")
mu <- pall[indx]
indxUse <- as.vector(matrix(1:length(mu),nrow=2)[,useDates])
mu <- mu[indxUse]
sigma <- Matrix::solve(obj$env$spHess(random=TRUE))[indx,indx][indxUse,indxUse]

X <- argosTrack:::rmvnorm(100,mu,as.matrix(sigma))

Y <- apply(X,1,function(xx)list(matrix(xx,nrow=2)))

plot(t(Y[[1]][[1]]))

Y2 <- do.call("c",Y)
Y3 <- t(do.call("cbind",Y2))

z <- MASS::kde2d(Y3[,2],Y3[,1],n=200)

cl <- contourLines(z,nlevels=500)

levels <- unique(unlist(lapply(cl,function(x)x$level)))

cols <- colorRampPalette(c("#FFFFFF00","yellow","orange","red","darkred"))(length(levels))

                                        #plot(x=NA,type="n",ylab="Latitude",xlab="longitude",ylim=range(z$y),xlim=range(z$x))

library(rworldxtra)
library(sp)


addAlpha <- Vectorize(function(x,alpha=1){
    a <- col2rgb(x,alpha=TRUE)
    args <- structure(as.list(a),"names"=rownames(a))
    args$alpha <- 255*alpha
    args$maxColorValue <- 255
    do.call("rgb",args)
},"x")


data("countriesHigh", package = "rworldxtra")
rng <- anim$getRange()
plotMap(anim, args=list(type="n"), obsArgs = list(type="n"))
plot(countriesHigh, add = TRUE, col="grey",border=NA)
##plotMap(anim,add=TRUE, obsArgs=list(pch=16,cex=0.5,col=rgb(0,0,1,0.3)))
invisible(lapply(cl,function(xx)polygon(xx$x,xx$y,col=addAlpha(cols[which(xx$level==levels)],0.2),border=NA)))
plotMap(anim,add=TRUE,obsArgs=list(type="n"), args=list(pch=16,cex=0.5,col=rgb(0,0,0,0.5)))
