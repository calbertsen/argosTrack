
context("Test Animal")


test_that("It works with expected input",
{
    expect_equivalent(class(Observation(lon=rep(0,100),lat=rep(0,100),
                                  locationclass=rep("GPS",100),
                                  dates=as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60)),
                      "Observation")
})

test_that("lon not numeric fails",
{
    expect_error(Observation(lon=rep("A",100),lat=rep(0,100),
                                  locationclass=rep("GPS",100),
                             dates=as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60))
        expect_error(Observation(lon=rep(Inf,100),lat=rep(0,100),
                                  locationclass=rep("GPS",100),
                             dates=as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60))
        expect_error(Observation(lon=numeric(100)*NA,lat=rep(0,100),
                                  locationclass=rep("GPS",100),
                             dates=as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60))
        expect_error(Observation(lon=NULL,lat=rep(0,100),
                                  locationclass=rep("GPS",100),
                             dates=as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60))
})

test_that("lat not numeric fails",
{
    expect_error(Observation(lon=rep(0,100),lat=rep("A",100),
                             locationclass=rep("GPS",100),
                             dates=as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60))
    expect_error(Observation(lon=rep(0,100),lat=rep(Inf,100),
                             locationclass=rep("GPS",100),
                             dates=as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60))
    expect_error(Observation(lon=rep(0,100),lat=numeric(100)*NA,
                             locationclass=rep("GPS",100),
                             dates=as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60))
    expect_error(Observation(lon=rep(0,100),lat=rep(NULL,100),
                             locationclass=rep("GPS",100),
                             dates=as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60))
})


test_that("dates not POSIXct fails",
{
    expect_error(Observation(lon=rep(0,100),lat=rep(0,100),
                             locationclass=rep("GPS",100),
                             dates=1 + (1:100) * 60 * 60))
    expect_error(Observation(lon=rep(0,100),lat=rep(0,100),
                             locationclass=rep("GPS",100),
                             dates="A"))
    expect_error(Observation(lon=rep(0,100),lat=rep(0,100),
                             locationclass=rep("GPS",100),
                             dates="2017-01-01 00:00:00"))
    expect_error(Observation(lon=rep(0,100),lat=rep(0,100),
                             locationclass=rep("GPS",100),
                             dates=c(as.POSIXct("2017-01-01 00:00:00"),NA)))
    expect_error(Observation(lon=rep(0,100),lat=rep(0,100),
                             locationclass=rep("GPS",100),
                             dates=NULL))
})

test_that("locationclass wrong fails",
{
    expect_error(Observation(lon=rep(0,100),lat=rep(0,100),
                                  locationclass=1,
                             dates=as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60)
                 )
    expect_error(Observation(lon=rep(0,100),lat=rep(0,100),
                                  locationclass=rep("NOT_A_CLASS",100),
                             dates=as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60))
    ## If no argos classes are used; set them to lc 3
    expect_equivalent(Observation(lon=rep(0,100),lat=rep(0,100),
                                  locationclass=rep("K",100),
                                  dates=as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60)$qual,factor(rep("3",100)))
})

numeric

test_that("Observation$show works",
{
    mod <- Observation(lon=rep(0,100),lat=rep(0,100),
                                  locationclass=rep("GPS",100),
                                  dates=as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60)
    expect_gt(length(capture.output(mod)),0)

})


test_that("Length of name > 1 gives error",
{
  expect_error(Animal(measurement=Measurement(model="n"),
                movement=RW(as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60),
                observation=Observation(lon=rep(0,100),lat=rep(0,100),
                                        locationclass=rep("GPS",100),
                                        dates=as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60),
                name=c("TestAnim","testName2")))
})


test_that("Observation Plot functions works",
{
    expr <- "Observation(lon=rep(0,100),lat=rep(0,100),
                                           locationclass=rep(\"GPS\",100),
                                           dates=as.POSIXct(\"2017-01-01 00:00:00\") + (1:100) * 60 * 60)"
    expect_null(eval(parse(text=paste0("plot(",expr,")"))))
    expect_null(eval(parse(text=paste0("plotLat(",expr,")"))))
    expect_null(eval(parse(text=paste0("plotLon(",expr,")"))))
    expect_null(eval(parse(text=paste0("plotMap(",expr,")"))))
})

