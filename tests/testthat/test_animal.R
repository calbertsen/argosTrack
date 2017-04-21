
context("Test Animal")


test_that("It works with expected input",
{

    expect_equivalent(class(Animal(measurement=Measurement(model="n"),
                                   movement=RW(as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60),
                                   observation=Observation(lon=rep(0,100),lat=rep(0,100),
                                                           locationclass=rep("GPS",100),
                                                           dates=as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60),
                                   name="TestAnim")),
                      "Animal")


})


test_that("Anima$show works",
{
    mod <- Animal(measurement=Measurement(model="n"),
                  movement=RW(as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60),observation=Observation(lon=rep(0,100),lat=rep(0,100),locationclass=rep("GPS",100),dates=as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60),name="TestAnim")
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

test_that("wrong class(observation) fails",
{
  expect_error(Animal(measurement=Measurement(model="n"),
                movement=RW(as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60),
                observation=Measurement(model="n"),
                name=c("TestAnim","testName2")))
})

test_that("wrong class(movement) fails",
{
  expect_error(Animal(measurement=Measurement(model="n"),
                movement=Measurement(model="n"),
                observation=Observation(lon=rep(0,100),lat=rep(0,100),
                                        locationclass=rep("GPS",100),
                                        dates=as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60),
                name=c("TestAnim","testName2")))
})


test_that("wrong class(measurement) fails",
{
  expect_error(Animal(measurement=RW(as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60),
                movement=RW(as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60),
                observation=Observation(lon=rep(0,100),lat=rep(0,100),
                                        locationclass=rep("GPS",100),
                                        dates=as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60),
                name=c("TestAnim","testName2")))
})


test_that("Animal Plot functions works",
{
    expr <- "Animal(measurement=Measurement(model=\"n\"),
                   movement=RW(as.POSIXct(\"2017-01-01 00:00:00\") + (1:100) * 60 * 60),
                   observation=Observation(lon=rep(0,100),lat=rep(0,100),
                                           locationclass=rep(\"GPS\",100),
                                           dates=as.POSIXct(\"2017-01-01 00:00:00\") + (1:100) * 60 * 60),
                   name=\"TestAnim\")"
    expect_null(eval(parse(text=paste0("plot(",expr,")"))))
    expect_null(eval(parse(text=paste0("plotLat(",expr,")"))))
    expect_null(eval(parse(text=paste0("plotLon(",expr,")"))))
    expect_null(eval(parse(text=paste0("plotMap(",expr,")"))))
})

