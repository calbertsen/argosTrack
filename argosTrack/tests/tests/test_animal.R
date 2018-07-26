
#### Test Animal


## It works with expected input
    is_equal(class(Animal(measurement=Measurement(model="n"),
                              movement=RW(as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60),
                              observation=Observation(lon=rep(0,100),lat=rep(0,100),
                                                      locationclass=rep("GPS",100),
                                                      dates=as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60),
                              name="TestAnim")),
                 "Animal")

## Anima$show works with n measurement
mod <- Animal(measurement=Measurement(model="n"),
              movement=RW(as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60),observation=Observation(lon=rep(0,100),lat=rep(0,100),locationclass=rep("GPS",100),dates=as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60),name="TestAnim")
gives_output(mod)


## Anima$show works with t measurement
mod <- Animal(measurement=Measurement(model="t"),
              movement=RW(as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60),observation=Observation(lon=rep(0,100),lat=rep(0,100),locationclass=rep("GPS",100),dates=as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60),name="TestAnim")
gives_output(mod)

## Anima$show works with sh measurement

gives_warning(mod <- Animal(measurement=Measurement(model="sh"),
                            movement=RW(as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60),observation=Observation(lon=rep(0,100),lat=rep(0,100),locationclass=rep("GPS",100),dates=as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60),name="TestAnim"))
gives_output(mod)

## Length of name > 1 gives error
gives_error(Animal(measurement=Measurement(model="n"),
                   movement=RW(as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60),
                   observation=Observation(lon=rep(0,100),lat=rep(0,100),
                                           locationclass=rep("GPS",100),
                                           dates=as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60),
                   name=c("TestAnim","testName2")))

## wrong class(observation) fails
gives_error(Animal(measurement=Measurement(model="n"),
                   movement=RW(as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60),
                   observation=Measurement(model="n"),
                   name=c("TestAnim","testName2")))

## wrong class(movement) fails
gives_error(Animal(measurement=Measurement(model="n"),
                   movement=Measurement(model="n"),
                   observation=Observation(lon=rep(0,100),lat=rep(0,100),
                                           locationclass=rep("GPS",100),
                                           dates=as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60),
                   name=c("TestAnim","testName2")))

## wrong class(measurement) fails
gives_error(Animal(measurement=RW(as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60),
                   movement=RW(as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60),
                   observation=Observation(lon=rep(0,100),lat=rep(0,100),
                                           locationclass=rep("GPS",100),
                                           dates=as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60),
                   name=c("TestAnim","testName2")))

## Animal Plot functions works
expr <- "Animal(measurement=Measurement(model=\"n\"),
                   movement=RW(as.POSIXct(\"2017-01-01 00:00:00\") + (1:100) * 60 * 60),
                   observation=Observation(lon=rep(0,100),lat=rep(0,100),
                                           locationclass=rep(\"GPS\",100),
                                           dates=as.POSIXct(\"2017-01-01 00:00:00\") + (1:100) * 60 * 60),
                   name=\"TestAnim\")"
gives_no_output(eval(parse(text=paste0("plot(",expr,")"))))
gives_no_output(eval(parse(text=paste0("plotLat(",expr,")"))))
gives_no_output(eval(parse(text=paste0("plotLon(",expr,")"))))
gives_no_output(eval(parse(text=paste0("plotMap(",expr,")"))))
gives_no_output(eval(parse(text=paste0("plot(",expr,", sd = TRUE)"))))
gives_no_output(eval(parse(text=paste0("plotLat(",expr,", sd = TRUE)"))))
gives_no_output(eval(parse(text=paste0("plotLon(",expr,", sd = TRUE)"))))
gives_no_output(eval(parse(text=paste0("plotMap(",expr,", sd = TRUE)"))))

