#### Util functions

## distance works
lat <- runif(100,-90,90)
lon <- runif(100,-180,180)
vals <- sapply(1:100,function(i)distance(lat[i],lon[i],lat[i],lon[i]))
is_true(all(vals==0))

vals <- sapply(1:100,function(i)distance(runif(1,-90,90),
                                         runif(1,-180,180),
                                         runif(1,-90,90),
                                         runif(1,-180,180)))
is_true(all(vals>0))

## ll2n and n2ll works
lat <- runif(100,-90,90)
lon <- runif(100,-180,180)
v1 <- ll2n(lon, lat)
is_equal(dim(v1),c(2,100))
v2 <- n2ll(v1[1,], v1[2,])
is_equal(t(v2),cbind(lon,lat))

## stepLength/bearing works
lat0 <- runif(100,-90,90)
lon0 <- runif(100,-180,180)
angle <- runif(100,-pi,pi)
sl <- rexp(100)

lat1 <- sl * sin(angle) + lat0
lon1 <- sl * cos(angle) + lon0
nc0 <- ll2n(lon0,lat0)
nc1 <- ll2n(lon1,lat1)
is_equal(stepLength(lon0,lat0,lon1,lat1,TRUE),sl)
is_equal(bearing(lon0,lat0,lon1,lat1,TRUE), angle, tolerance = 1e-4)
is_equal(stepLength(lon0,lat0,lon1,lat1,TRUE), sl)
is_equal(stepLength(lon1,lat1,lon0,lat0,TRUE), stepLength(lon0,lat0,lon1,lat1,TRUE))
is_equal(bearing(lon0,lat0,lon1,lat1,FALSE), bearing(nc0[1,],nc0[2,],nc1[1,],nc1[2,],TRUE))
is_equal(stepLength(lon0,lat0,lon1,lat1,FALSE), stepLength(nc0[1,],nc0[2,],nc1[1,],nc1[2,],TRUE))



## is.XXX works
is_true(is.POSIXct(as.POSIXct("2017-01-01")))
is_false(is.POSIXct(numeric(10)))
is_false(is.POSIXct(logical(10)))
is_false(is.POSIXct(character(10)))

is_true(is.numsca(0.7))
is_true(is.numsca(1L))
is_false(is.numsca(numeric(3)))
is_false(is.numsca(character(3)))
is_false(is.numsca(character(1)))
is_false(is.numsca(logical(1)))

is_true(is.numint(4L))
is_false(is.numint(0.7))
is_false(is.numint(numeric(3)))
is_false(is.numint(integer(3)))
is_false(is.numint(character(3)))
is_false(is.numint(character(1)))
is_false(is.numint(logical(1)))

is_true(is.samelength(matrix(0,3,3),matrix(1,3,3)))
is_true(is.samelength(matrix(0,3,3),rep(1,3)))
is_true(is.samelength(rep(1,3),matrix(0,3,3)))
is_true(is.samelength(rep(1,3),rep(0,3)))
is_false(is.samelength(matrix(0,10,10),c(1,2,3)))
is_false(is.samelength(list(1,2,3),c(1,2,3)))

