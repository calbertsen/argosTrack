
#### Test getTrack function

## getTrack returns data.frame

mov <- RW(as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60)
mov$mu[] <- runif(length(mov$mu))
obs <- Observation(lon=runif(100),
                   lat=runif(100),
                   locationclass=sample(c("S","K","GPS","3","2","1","0","A","B","Z"),100,
                                        replace=TRUE),
                   dates=as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60)
anim <- Animal(measurement=Measurement(model="n"),
               movement=mov,
               observation=obs,
               name="TestAnim")
is_equal(class(getTrack(mov)),"data.frame")
is_equal(class(getTrack(obs)),"data.frame")
is_equal(class(getTrack(anim)),"data.frame")

## getTrack data.frame for animal has same columns as Movement and Observation
mov <- RW(as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60)
mov$mu[] <- runif(length(mov$mu))
obs <- Observation(lon=runif(100),
                   lat=runif(100),
                   locationclass=sample(c("S","K","GPS","3","2","1","0","A","B","Z"),100,
                                        replace=TRUE),
                   dates=as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60)
anim <- Animal(measurement=Measurement(model="n"),
               movement=mov,
               observation=obs,
               name="TestAnim")
a <- getTrack(mov)
b <- getTrack(obs)
ab <- getTrack(anim)
## All colnames from a and b should be colnames of ab, however, we can add extra info in ab
is_true(all(colnames(a) %in% colnames(ab)))
is_true(all(colnames(b) %in% colnames(ab)))
is_equal(unique(ab[ab$dates %in% a$dates,colnames(a)]),unique(a))
is_equal(unique(ab[ab$dates %in% b$dates,colnames(b)]),unique(b))
