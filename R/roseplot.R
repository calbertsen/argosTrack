##' Roseplot / radar histogram
##' @title Roseplot / radar histogram
##' @param x Vector of values
##' @param breaks Either \code{Sturges} or the number of breaks to use.
##' @param prob Should the bars be standardized?
##' @param main Main title of the plot
##' @param xlab x-axis label of the plot
##' @param ... Passed to polygons for plotting the bars
##' @author Christoffer Moesgaard Albertsen
##' @keywords internal
##' @importFrom grDevices nlcass.Sturges
##' @importFrom graphics hist lines text polygon segments
##'
##' 
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
        nbrk <- grDevices::nclass.Sturges(x %% (2 * pi))
    }else if(is.numeric(breaks) & length(breaks) == 1){
        nbrk <- breaks
    }else{
        stop("WRONG BREAKS")
    }
    
    nslp <- 100
    h <- graphics::hist(x %% (2 * pi),
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
    graphics::plot(mshw*cos(seq(0,2 * pi,len=1000)),mshw*sin(seq(0,2*pi,len=1000)),type="l",
         asp=1,axes=FALSE,
         main = main,
         ylab="",xlab=xlab)
    a <- lapply(as.list(shw),
                function(x)graphics::lines(x*cos(seq(0,2*pi,len=1000)),
                                 x*sin(seq(0,2*pi,len=1000)),
                                 col="grey",lty=2))
    a <- lapply(as.list(shw),
                function(x) graphics::text(0,-shw,labels=shw,col="grey",cex=0.75))
    graphics::segments(x0=mshw*cos(seq(0,pi,len=9)),
             y0=mshw*sin(seq(0,pi,len=9)),
             x1=mshw*cos(pi+seq(0,pi,len=9)),
             y1=mshw*sin(pi+seq(0,pi,len=9)),
             col="grey")
    a <- lapply(slices,function(x)graphics::polygon(x,...))

    nlabs <- 7
    labval <- cbind(1:nlabs,(nlabs+1)/2)
    
    pos <- cbind(1.05*mshw*cos(seq(0,2*pi,len=nlabs+2)[-1]),
                 1.05*mshw*sin(seq(0,2*pi,len=nlabs+2)[-1]))
    sapply(1:nrow(labval),function(i)
        text(pos[i,1],pos[i,2],eval(shortfrac(labval[i,1],labval[i,2],"pi")[[1]]),cex=0.75))
    graphics::text(pos[nlabs+1,1],pos[nlabs+1,2],"0",cex=0.75)
}

    


