

distance <- Vectorize(function(lat1,lon1,lat2,lon2){
    ## Haversine formula
    R <- 6371e3
    phi1 <- lat1 * pi / 180
    phi2 <- lat2 * pi / 180
    dPhi <- (lat2-lat1) * pi / 180
    dLam <- (lon2-lon1) * pi / 180
    a <- sin(dPhi * 0.5) ^ 2 + cos(phi1)*cos(phi2) * sin(dLam * 0.5) ^2
    c <- 2 * atan2(sqrt(a), sqrt(1-a))
    return(R*c)
})


ll2n <- Vectorize(function(lon,lat){
    .Call("ll2n",lon=lon,lat=lat,
          PACKAGE="argosTrack")
})

n2ll <- Vectorize(function(x,y){
    .Call("n2ll",x=x,y=y,
          PACKAGE="argosTrack")
})

bearing <- Vectorize(function(x0,y0,x1,y1,nautical)
    .Call("bearing",x0=x0,y0=y0,x1=x1,y1=y1,nautical=nautical,
          PACKAGE="argosTrack")
)
stepLength <- Vectorize(function(x0,y0,x1,y1,nautical)
    .Call("stepLength",x0=x0,y0=y0,x1=x1,y1=y1,nautical=nautical,
          PACKAGE="argosTrack")
)

is.POSIXct <- function(x) inherits(x,"POSIXct")
is.numsca <- function(x) is.numeric(x) & (length(x) == 1)
is.numint <- function(x) is.integer(x) & (length(x) == 1)
is.numvec <- function(x) is.vector(x) & is.numeric(x)
is.nummat <- function(x) is.matrix(x) & is.numeric(x)
is.samelength <- function(x,y){
    if(is.nummat(x) & is.nummat(y))
        return(all(dim(x)==dim(y)))
    if(is.numvec(x) & is.nummat(y))
        return(length(x) == dim(y)[1])
    if(is.nummat(x) & is.numvec(y))
        return(dim(x)[1] == length(y))
    if(is.numvec(x) & is.numvec(y))
        return(length(x) == length(y))
    return(FALSE)
}
