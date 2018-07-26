
#### Test simTrack function for measurements

## getTrack returns numeric array

obs <- Observation(lon=runif(100),
                   lat=runif(100),
                   locationclass=sample(c("S","K","GPS","3","2","1","0","B","Z"),100,
                                        replace=TRUE),
                   dates=as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60)
meas <- Measurement(model="n")
dd <- simTrack(meas,1,obs)

is_equal(dim(simTrack(meas,1,obs)),c(2,100,1))
is_equal(dim(simTrack(meas,5,obs)),c(2,100,5))
is_false(any(is.na(simTrack(meas,5,obs))))
is_true(all(is.numeric(simTrack(meas,5,obs))))

## observation argument must be Observation refclass and not missing
gives_error(simTrack(meas,1,meas))
gives_error(simTrack(meas,1))

