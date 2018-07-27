
#### Test Observation


## It works with expected input
is_equal(class(Observation(lon=rep(0,100),lat=rep(0,100),
                           locationclass=rep("GPS",100),
                           dates=as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60)),
         "Observation")



## wrong lengths fails
gives_error(Observation(lon=rep(0,100),lat=rep(0,99),
                        locationclass=rep("GPS",100),
                        dates=as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60))
gives_error(Observation(lon=rep(0,100),lat=rep(0,100),
                        locationclass=rep("GPS",99),
                        dates=as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60))
gives_error(Observation(lon=rep(0,100),lat=rep(0,100),
                        locationclass=rep("GPS",100),
                        dates=as.POSIXct("2017-01-01 00:00:00") + (1:99) * 60 * 60))
gives_error(Observation(lon=rep(0,100),lat=rep(0,100),
                        locationclass=rep("GPS",100),
                        include = rep(TRUE,99),
                        dates=as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60))

## include not logical fails
gives_error(Observation(lon=rep(0,100),lat=rep(0,100),
                        locationclass=rep("GPS",100),
                        include = NULL,
                        dates=as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60))
gives_error(Observation(lon=rep(0,100),lat=rep(0,100),
                        locationclass=rep("GPS",100),
                        include = rep(0,100),
                        dates=as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60))
gives_error(Observation(lon=rep(0,100),lat=rep(0,100),
                        locationclass=rep("GPS",100),
                        include = rep(NA,100),
                        dates=as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60))

## lon not numeric fails
gives_error(Observation(lon=rep("A",100),lat=rep(0,100),
                        locationclass=rep("GPS",100),
                        dates=as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60))
gives_error(Observation(lon=rep(Inf,100),lat=rep(0,100),
                        locationclass=rep("GPS",100),
                        dates=as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60))
gives_error(Observation(lon=numeric(100)*NA,lat=rep(0,100),
                        locationclass=rep("GPS",100),
                        dates=as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60))
gives_error(Observation(lon=NULL,lat=rep(0,100),
                        locationclass=rep("GPS",100),
                        dates=as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60))

## lat not numeric fails
gives_error(Observation(lon=rep(0,100),lat=rep("A",100),
                        locationclass=rep("GPS",100),
                        dates=as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60))
gives_error(Observation(lon=rep(0,100),lat=rep(Inf,100),
                        locationclass=rep("GPS",100),
                        dates=as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60))
gives_error(Observation(lon=rep(0,100),lat=numeric(100)*NA,
                        locationclass=rep("GPS",100),
                        dates=as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60))
gives_error(Observation(lon=rep(0,100),lat=rep(NULL,100),
                        locationclass=rep("GPS",100),
                        dates=as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60))


## dates not POSIXct fails
gives_error(Observation(lon=rep(0,100),lat=rep(0,100),
                             locationclass=rep("GPS",100),
                             dates=1 + (1:100) * 60 * 60))
    gives_error(Observation(lon=rep(0,100),lat=rep(0,100),
                             locationclass=rep("GPS",100),
                             dates="A"))
    gives_error(Observation(lon=rep(0,100),lat=rep(0,100),
                             locationclass=rep("GPS",100),
                             dates="2017-01-01 00:00:00"))
    gives_error(Observation(lon=rep(0,100),lat=rep(0,100),
                             locationclass=rep("GPS",100),
                             dates=c(as.POSIXct("2017-01-01 00:00:00"),NA)))
    gives_error(Observation(lon=rep(0,100),lat=rep(0,100),
                             locationclass=rep("GPS",100),
                             dates=c(as.POSIXct("2017-01-01 00:00:00"),Inf)))
    gives_error(Observation(lon=rep(0,100),lat=rep(0,100),
                             locationclass=rep("GPS",100),
                             dates=NULL))

## locationclass wrong fails
    gives_error(Observation(lon=rep(0,100),lat=rep(0,100),
                                  locationclass=1,
                             dates=as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60)
                 )
gives_error(Observation(lon=rep(0,100),lat=rep(0,100),
                                  locationclass=rep("NOT_A_CLASS",100),
                             dates=as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60))
    gives_error(Observation(lon=rep(0,100),lat=rep(0,100),
                                  locationclass=factor(rep(NA,100)),
                             dates=as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60))
    ## If no argos classes are used; set them to lc 3
    is_equal(Observation(lon=rep(0,100),lat=rep(0,100),
                         locationclass=rep("K",100),
                         dates=as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60)$qual,factor(rep("3",100)))


## Observation$show works
mod <- Observation(lon=rep(0,100),lat=rep(0,100),
                   locationclass=rep("GPS",100),
                   dates=as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60)
is_true(length(capture.output(mod)) > 0)

## Length of name > 1 gives error
gives_error(Animal(measurement=Measurement(model="n"),
                   movement=RW(as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60),
                   observation=Observation(lon=rep(0,100),lat=rep(0,100),
                                           locationclass=rep("GPS",100),
                                           dates=as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60),
                   name=c("TestAnim","testName2")))


## Observation Plot functions works
expr <- "Observation(lon=rep(0,100),lat=rep(0,100),
                                           locationclass=rep(\"GPS\",100),
                                           dates=as.POSIXct(\"2017-01-01 00:00:00\") + (1:100) * 60 * 60)"
gives_no_output(eval(parse(text=paste0("plot(",expr,")"))))
gives_no_output(eval(parse(text=paste0("plotLat(",expr,")"))))
gives_no_output(eval(parse(text=paste0("plotLon(",expr,")"))))
gives_no_output(eval(parse(text=paste0("plotMap(",expr,")"))))


## getTMBmap should fix logSdObs if all locationclasses are K, S or P
## Do not fix anything:
for(v in c("GPS","3","2","1","0","A","B","Z","K","S","P")){
    m <- Observation(lon=rep(0,100),lat=rep(0,100),
                     locationclass=rep(v,100),
                     dates=as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60)$getTMBmap()
    if(v %in% c("K","S","P")){
        is_equal(class(m), "list")
        is_equal(names(m), "logSdObs")
        is_equal(m$logSdObs, factor(c(NA,NA)))
    }else{
        is_equal(class(m), "list")
        is_equal(length(m), 0)
    }
}
