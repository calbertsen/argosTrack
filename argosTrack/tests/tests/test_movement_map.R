#### Extra tests for movement getTMB...

## RW parameters should all start at 0 (if nothing has been done)
mov <- RW(as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60)
is_equal(mov$getTMBparameters()$mu,matrix(0,2,100))
## DSBW parameters should not all start at same value
mov <- DSBW(as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60)
is_not_equal(mov$getTMBparameters()$mu,matrix(0,2,100))
## DSBHN parameters should not all start at same value
mov <- DSBHN(as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60)
is_not_equal(mov$getTMBparameters()$mu,matrix(0,2,100))

## DIRAC should map mu parameters
mu0 <- matrix(c(1,2),2,100)
mov <- DIRAC(as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60)
is_equal(mov$getTMBmap()$mu,factor(as.vector(mu0)))
