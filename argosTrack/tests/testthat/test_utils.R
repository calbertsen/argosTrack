context("Util functions")

test_that("distance works",
{
    lat <- runif(100,-90,90)
    lon <- runif(100,-180,180)
    vals <- sapply(1:100,function(i)argosTrack:::distance(lat[i],lon[i],lat[i],lon[i]))
    expect_true(all(vals==0))

    vals <- sapply(1:100,function(i)argosTrack:::distance(runif(1,-90,90),
                                                          runif(1,-180,180),
                                                          runif(1,-90,90),
                                                          runif(1,-180,180)))
    expect_true(all(vals>0))
})

test_that("ll2n and n2ll works",
{
    lat <- runif(100,-90,90)
    lon <- runif(100,-180,180)
    coord <- cbind(lat,lon)
    vals <- apply(coord,1,function(x){
        v1 <- argosTrack:::ll2n(x[2],x[1])
        v2 <- argosTrack:::n2ll(v1[1],v1[2])
        all(abs(v2-x[2:1]) < 1e-8 & abs(v1-x[2:1]) > 0)
    })
    expect_true(all(vals))
})

 test_that("stepLength/bearing works",
{
    lat0 <- runif(100,-90,90)
    lon0 <- runif(100,-180,180)
    angle <- runif(100,-pi,pi)
    sl <- rexp(100)

    lat1 <- sl * sin(angle) + lat0
    lon1 <- sl * cos(angle) + lon0
    nc0 <- ll2n(lon0,lat0)
    nc1 <- ll2n(lon1,lat1)
    expect_true(all(abs(stepLength(lon0,lat0,lon1,lat1,TRUE) - sl) < 1e-12))
    expect_true(all(abs(bearing(lon0,lat0,lon1,lat1,TRUE) - angle) < 1e-4))
    expect_true(all(abs(stepLength(lon0,lat0,lon1,lat1,TRUE) - sl) < 1e-12))
    expect_true(all(abs(stepLength(lon1,lat1,lon0,lat0,TRUE) - stepLength(lon0,lat0,lon1,lat1,TRUE)) < 1e-16))
    expect_true(all(abs(bearing(lon0,lat0,lon1,lat1,FALSE) - bearing(nc0[1,],nc0[2,],nc1[1,],nc1[2,],TRUE)) < 1e-16))
    expect_true(all(abs(stepLength(lon0,lat0,lon1,lat1,FALSE) - stepLength(nc0[1,],nc0[2,],nc1[1,],nc1[2,],TRUE)) < 1e-16))

})


test_that("is.XXX works",
{
    expect_true(argosTrack:::is.POSIXct(as.POSIXct("2017-01-01")))
    expect_false(argosTrack:::is.POSIXct(numeric(10)))
    expect_false(argosTrack:::is.POSIXct(logical(10)))
    expect_false(argosTrack:::is.POSIXct(character(10)))

    expect_true(argosTrack:::is.numsca(0.7))
    expect_true(argosTrack:::is.numsca(1L))
    expect_false(argosTrack:::is.numsca(numeric(3)))
    expect_false(argosTrack:::is.numsca(character(3)))
    expect_false(argosTrack:::is.numsca(character(1)))
    expect_false(argosTrack:::is.numsca(logical(1)))

    expect_true(argosTrack:::is.numint(4L))
    expect_false(argosTrack:::is.numint(0.7))
    expect_false(argosTrack:::is.numint(numeric(3)))
    expect_false(argosTrack:::is.numint(integer(3)))
    expect_false(argosTrack:::is.numint(character(3)))
    expect_false(argosTrack:::is.numint(character(1)))
    expect_false(argosTrack:::is.numint(logical(1)))

    expect_true(argosTrack:::is.samelength(matrix(0,3,3),matrix(1,3,3)))
    expect_true(argosTrack:::is.samelength(matrix(0,3,3),rep(1,3)))
    expect_true(argosTrack:::is.samelength(rep(1,3),matrix(0,3,3)))
    expect_true(argosTrack:::is.samelength(rep(1,3),rep(0,3)))
    expect_false(argosTrack:::is.samelength(matrix(0,10,10),c(1,2,3)))
    expect_false(argosTrack:::is.samelength(list(1,2,3),c(1,2,3)))

})
