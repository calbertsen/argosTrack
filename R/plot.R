

##' Plot the fit from argosTrack
##' @param x fitted argosTrack object
##' @param bg_style If 'none' an empty background is plotted. If 'map' a map is plotted in the background.
##' @param only_map Should only a map be plotted or also coordinate wise estimates?
##' @param min_area Minimum area of land polygons to plot.
##' @param zoom_to_obs If TRUE all observations will be shown. If FALSE outliers may be left outside the plotting area.
##' @param ...
##' @author Christoffer Moesgaard Albertsen
##' @export
plot.argostrack <- function(x,
                            bg_style=c("none","map"),
                            only_map = FALSE,
                            min_area = 0.01,
                            zoom_to_obs=TRUE,
                            ...){
    
    bg_style <- match.arg(bg_style)
    object <- x
    ## srep <- object$sdreport_summary
    ## track <- srep[rownames(srep)=="mu",]
    sdtrack <- object$positions_sd ## matrix(track[,2],nrow=2)
    esttrack <- object$positions ## matrix(track[,1],nrow=2)
    obs <- object$observations
    dates <- object$dates
    posdates <- object$state_dates

    if(is.character(dates)){
        dates <- as.POSIXct(dates)      
    }

    dt <- diff(dates)

    if(!only_map)
        layout(matrix(c(1,1,2,3),ncol=2))

    if(zoom_to_obs){
        xrng <- c(min(obs[2,])-0.2, max(obs[2,])+0.2)
        yrng <- c(min(obs[1,])-0.2, max(obs[1,])+0.2)
    }else{
        xrng <- c(min(esttrack[2,])-0.2, max(esttrack[2,])+0.2)
        yrng <- c(min(esttrack[1,])-0.2, max(esttrack[1,])+0.2)
    }
    
    if(bg_style=="none"){
        
        plot(obs[2,],obs[1,],type="l",lty=2,col="grey",
             xlim=xrng,
             ylim=yrng,
             asp=1/cos((mean(yrng) * pi) / 180),
             xlab = expression(paste("Longitude (",degree,")",sep="")),
             ylab = expression(paste("Latitude (",degree,")",sep="")))
        lines(esttrack[2,],esttrack[1,])
        
    }else if(bg_style=="map"){ 
        ## data('worldShorelines',package="argosTrack")
        ## data('worldShorelinesArea',package="argosTrack")
        plot(NA, xlim=xrng, ylim=yrng,asp=1/cos((mean(yrng) * pi) / 180),
             xlab = expression(paste("Longitude (",degree,")",sep="")),
             ylab = expression(paste("Latitude (",degree,")",sep="")))
        # Need faster way to plot the polygons
        invisible(lapply(argosTrack::worldShorelines[argosTrack::worldShorelinesArea>min_area],function(x){
            polygon(x[,1],x[,2],col=grey(0.8),border=NA)
        }))
        box()
        lines(obs[2,],obs[1,],type="l",lty=2,col=grey(0.5))
        lines(esttrack[2,],esttrack[1,])

    }

    if(!only_map){
        plot(dates,obs[2,],pch=16,col="grey",
             xlab = "Date",
             ylab =  expression(paste("Longitude (",degree,")",sep="")))
        lines(posdates,esttrack[2,])
        lines(posdates,esttrack[2,]+2*sdtrack[2,],lty=3)
        lines(posdates,esttrack[2,]-2*sdtrack[2,],lty=3)
        
        plot(dates,obs[1,],pch=16,col="grey",
             xlab = "Date",
             ylab =  expression(paste("Latitude (",degree,")",sep="")))
        lines(posdates,esttrack[1,])
        lines(posdates,esttrack[1,]+2*sdtrack[1,],lty=3)
        lines(posdates,esttrack[1,]-2*sdtrack[1,],lty=3)
    }

}


##' Roseplot / radar histogram
##' @title Roseplot / radar histogram
##' @param x 
##' @param breaks 
##' @param prob 
##' @param main 
##' @param xlab 
##' @param ... 
##' @author Christoffer Moesgaard Albertsen
.roseplot <- function(x, breaks = "Sturges", prob=TRUE, main = NULL, xlab = "", ...){

    gcd <- function(a,b) ifelse (b==0, a, gcd(b, a %% b)) 
    shortfrac <- Vectorize(
        function(x,y,txt){  
            if(x==y)
                return(substitute(expression(d),list(d=as.symbol(txt))))
            v <- x/y
            if(round(v)==v){
                return(substitute(expression(a*d),list(a=v,d=as.symbol(txt))))
            }
            v <- gcd(x,y)
            x <- x/v
            y <- y/v
            if(x==1)
                return(substitute(expression(frac(d,b)),list(b=y,d=as.symbol(txt))))
            return(substitute(expression(frac(a,b)*d),list(a=x,b=y,d=as.symbol(txt))))
    })
    
    if(breaks == "Sturges"){    
        nbrk <- nclass.Sturges(x %% (2 * pi))
    }else if(is.numeric(breaks) & length(breaks) == 1){
        nbrk <- breaks
    }else{
        stop("WRONG BREAKS")
    }
    
    nslp <- 100
    h <- hist(x %% (2 * pi),
              breaks = seq(0, 2 * pi, len = nbrk),
              plot = FALSE)
    vals <- if(prob){h$density}else{h$counts}

    slices <- lapply(as.list(1:length(vals)),
                     function(i)complex(argument=c(0,
                                            seq(h$breaks[i],
                                                h$breaks[i + 1],
                                                len = nslp),
                                            0),
                                        modulus=c(0,
                                            rep(vals[i],nslp),
                                            0)
                                        )
                     )
    shw <- pretty(c(0,max(vals)),11)[-1]
    mshw <- max(shw)
    shw <- shw[-length(shw)]
    plot(mshw*cos(seq(0,2 * pi,len=1000)),mshw*sin(seq(0,2*pi,len=1000)),type="l",
         asp=1,axes=FALSE,
         main = main,
         ylab="",xlab=xlab)
    a <- lapply(as.list(shw),
                function(x)lines(x*cos(seq(0,2*pi,len=1000)),
                                 x*sin(seq(0,2*pi,len=1000)),
                                 col="grey",lty=2))
    a <- lapply(as.list(shw),
                function(x) text(0,-shw,labels=shw,col="grey",cex=0.75))
    segments(x0=mshw*cos(seq(0,pi,len=9)),
             y0=mshw*sin(seq(0,pi,len=9)),
             x1=mshw*cos(pi+seq(0,pi,len=9)),
             y1=mshw*sin(pi+seq(0,pi,len=9)),
             col="grey")
    a <- lapply(slices,function(x)polygon(x,...))

    nlabs <- 7
    labval <- cbind(1:nlabs,(nlabs+1)/2)
    
    pos <- cbind(1.05*mshw*cos(seq(0,2*pi,len=nlabs+2)[-1]),
                 1.05*mshw*sin(seq(0,2*pi,len=nlabs+2)[-1]))
    sapply(1:nrow(labval),function(i)
        text(pos[i,1],pos[i,2],eval(shortfrac(labval[i,1],labval[i,2],"pi")[[1]]),cex=0.75))
    text(pos[nlabs+1,1],pos[nlabs+1,2],"0",cex=0.75)
}

    



#' Plot of the summary of an argosTrack fit.
#' @param x 
#' @param nclass 
#' @param prob 
#' @param type 
#' @param bearings 
#' @param ...
#' @author Christoffer Moesgaard Albertsen
#' @export
plot.summary_argostrack <- function(x,nclass = 35,prob=TRUE,type="both",bearings=FALSE,...){
    if(type=="both"){
        layout(matrix(1:2,1,2))
    }
    if(type %in% c("both","step"))
        hist(x$steplengths_per_hour * 1.852,
             xlab = "Step length (km/h)",
             main = NULL,
             nclass = nclass,
             prob = prob,
             ...)
    if(type %in% c("both","angle"))
        if(bearings){
            .roseplot(cumsum(x$turningangles),
                      breaks = nclass,
                      prob=prob,
                      xlab="Directional bearings (radians)",...)
        }else{
            .roseplot(x$turningangles,
                      breaks = nclass,
                      prob=prob,
                      xlab="Turning angles between states (radians)",...)
        }
}
