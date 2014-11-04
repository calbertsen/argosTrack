


#' @export

plot.argostrack <- function(object,bg_style="none",only_map = FALSE,minArea = 10){

    srep <- object$sdreport_summary
    track <- srep[rownames(srep)=="mu",]
    sdtrack <- matrix(track[,2],nrow=2)
    esttrack <- matrix(track[,1],nrow=2)
    obs <- object$observations
    dates <- object$dates
    dt <- object$tmb_object$env$data$dt

    if(is.character(dates)){
        dates <- as.POSIXct(dates)      
    }

    if(!only_map)
        layout(matrix(c(1,1,2,3),ncol=2))

    if(bg_style=="none"){
        xrng <- c(min(obs[2,])-0.2, max(obs[2,])+0.2)
        yrng <- c(min(obs[1,])-0.2, max(obs[1,])+0.2)
        
        plot(obs[2,],obs[1,],type="l",lty=2,col="grey",
             xlim=xrng,
             ylim=yrng,
             asp=cos((mean(yrng) * pi) / 180),
             xlab = expression(paste("Longitude (",degree,")",sep="")),
             ylab = expression(paste("Latitude (",degree,")",sep="")))
        lines(esttrack[2,],esttrack[1,])
        
    }else if(bg_style=="map" || bg_style == "pbs"){ 
        data('worldShorelines',package="argosTrack")
        data('worldShorelinesArea',package="argosTrack")
        xrng <- c(min(obs[2,])-0.2, max(obs[2,])+0.2)
        yrng <- c(min(obs[1,])-0.2, max(obs[1,])+0.2)
        plot(NA, xlim=xrng, ylim=yrng,asp=cos((mean(yrng) * pi) / 180),
             xlab = expression(paste("Longitude (",degree,")",sep="")),
             ylab = expression(paste("Latitude (",degree,")",sep="")))
        # Need faster way to plot the polygons
        invisible(lapply(worldShorelines[worldShorelinesArea>minArea],function(x){
            polygon(x$X,x$Y,col=grey(0.8),border=NA)
        }))
        box()
        lines(obs[2,],obs[1,],type="l",lty=2,col=grey(0.5))
        lines(esttrack[2,],esttrack[1,])

    }else{
        stop("Background style is not valid.")
    }

    if(!only_map){
        plot(dates,obs[2,],pch=16,col="grey",
             xlab = "Date",
             ylab =  expression(paste("Longitude (",degree,")",sep="")))
        lines(dates[dt>0],esttrack[2,])
        lines(dates[dt>0],esttrack[2,]+2*sdtrack[2,],lty=3)
        lines(dates[dt>0],esttrack[2,]-2*sdtrack[2,],lty=3)
        
        plot(dates,obs[1,],pch=16,col="grey",
             xlab = "Date",
             ylab =  expression(paste("Latitude (",degree,")",sep="")))
        lines(dates[dt>0],esttrack[1,])
        lines(dates[dt>0],esttrack[1,]+2*sdtrack[1,],lty=3)
        lines(dates[dt>0],esttrack[1,]-2*sdtrack[1,],lty=3)
    }

}

#' @export
plot.argostrack_bootstrap <- function(object, vertical = TRUE, ...){

    msearray <- object$mse
    pdatlat <- data.frame(V1 = object$mse[1,,1])
    pdatlon <- data.frame(V1 = object$mse[2,,1])

    if(dim(msearray)[3]>1){
        for(i in 1:dim(msearray)[3]){
            pdatlat[,i] <- object$mse[1,,i]
            pdatlon[,i] <- object$mse[2,,i]
        }
    }
    dnam <- dimnames(object$mse)
    if(!is.null(dnam)){
        if(!is.null(dnam[[3]])){
            colnames(pdatlat) <- dnam[[3]]
            colnames(pdatlon) <- dnam[[3]]
        }
    }
    #if(!is.null(names)){
    #    colnames(pdatlat) <- names
    #    colnames(pdatlon) <- names
    #}

    if(vertical){
        layout(matrix(c(1,2),ncol=1))
    }else{
        layout(matrix(c(1,2),nrow=1))
    }
    boxplot(pdatlon,na.rm=TRUE,main=NULL,
            ylab=expression(paste("MSE for estimates, Longitude (",degree,")",sep="")),...)
    boxplot(pdatlat,na.rm=TRUE,main=NULL,
            ylab=expression(paste("MSE for estimates, Latitude (",degree,")",sep="")),...)
}
