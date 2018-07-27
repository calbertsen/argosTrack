
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




## Check copying

anim <- Animal(measurement=Measurement(model="n"),
               movement=RW(as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60),
               observation=Observation(lon=rep(0,100),lat=rep(0,100),
                                       locationclass=rep("GPS",100),
                                       dates=as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60),
               name="TestAnim")
fieldNames <- names(getClass("Animal")@fieldClasses)

## Animal can be copied
anim2 <- anim$copy()    
for(fn in fieldNames){
    is_equal(anim$field(fn),
                 anim2$field(fn))
}

## Animal can be shallow copied
anim3 <- anim$copy(shallow = TRUE)
for(fn in fieldNames){
    is_equal(anim$field(fn),
             anim3$field(fn))
}



## Test getRange

## If zoonToState; result should match result from movement
is_equal(anim$getRange(FALSE,zoomToState = TRUE),
         anim$movement$getRange(FALSE))
is_equal(anim$getRange(TRUE,zoomToState = TRUE),
         anim$movement$getRange(TRUE))

## If !zoonToState; result should be range from movement and obs
r1 <- anim$movement$getRange(FALSE)
r2 <- anim$observation$getRange()
is_equal(anim$getRange(FALSE,zoomToState = FALSE),
         rbind(range(r1[1,],r2[1,]),range(r1[2,],r2[2,])))
r1 <- anim$movement$getRange(TRUE)
r2 <- anim$observation$getRange()
is_equal(anim$getRange(TRUE,zoomToState = FALSE),
         rbind(range(r1[1,],r2[1,]),range(r1[2,],r2[2,])))
