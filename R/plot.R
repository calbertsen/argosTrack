


#' @export

plot.argostrack <- function(object){

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

    
    layout(matrix(c(1,1,2,3),ncol=2))
    plot(obs[2,],obs[1,],type="l",lty=2,col="grey",
         xlab = expression(paste("Longitude (",degree,")",sep="")),
         ylab = expression(paste("Latitude (",degree,")",sep="")))
    lines(esttrack[2,],esttrack[1,])

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

#' @export
plot.argostrack_bootstrap <- function(object, names = NULL){

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
    if(!is.null(names)){
        colnames(pdatlat) <- names
        colnames(pdatlon) <- names
    }

    layout(matrix(c(1,2),nrow=1))
    
    boxplot(pdatlon,na.rm=TRUE,main=NULL,
            ylab=expression(paste("MSE for estimates, Longitude (",degree,")",sep="")))
    boxplot(pdatlat,na.rm=TRUE,main=NULL,
            ylab=expression(paste("MSE for estimates, Longitude (",degree,")",sep="")))
}
