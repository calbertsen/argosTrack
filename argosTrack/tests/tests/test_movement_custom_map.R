
#### Test custom map for movement models

###########
## CTCRW ##
###########

## CTCRW equaldecay works
mod <- CTCRW(as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60)
is_equal(mod$getTMBmap(equaldecay=TRUE)$movePars,
         factor(c(1,1,3,4)))
is_true(is.null(mod$getTMBmap(equaldecay=FALSE)$movePars))


## CTCRW equaldrift works
mod <- CTCRW(as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60)
is_equal(mod$getTMBmap(equaldrift=TRUE)$movePars,
         factor(c(1,2,3,3)))
is_true(is.null(mod$getTMBmap(equaldrift=FALSE)$movePars))

## CTCRW fixdrift works
mod <- CTCRW(as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60)
is_equal(mod$getTMBmap(fixdrift=TRUE)$movePars,
         factor(c(1,2,NA,NA)))
is_true(is.null(mod$getTMBmap(fixdrift=FALSE)$movePars))

## CTCRW all arguments works
mod <- CTCRW(as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60)
is_equal(mod$getTMBmap(equaldecay=TRUE,fixdrift=TRUE)$movePars,
         factor(c(1,1,NA,NA)))
is_equal(mod$getTMBmap(equaldecay=TRUE,equaldrift=TRUE)$movePars,
         factor(c(1,1,3,3)))
## Fix drift overwrites equaldrift
is_equal(mod$getTMBmap(equaldrift=TRUE,fixdrift=TRUE)$movePars,
         factor(c(1,2,NA,NA)))
is_equal(mod$getTMBmap(equaldecay=TRUE,equaldrift=TRUE,fixdrift=TRUE)$movePars,
         factor(c(1,1,NA,NA)))

## CTCRW equalvar works
mod <- CTCRW(as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60)
is_equal(mod$getTMBmap(equalvar=TRUE)$logSdState,factor(c(1,1)))
is_true(is.null(mod$getTMBmap(fixmovecor=FALSE)$movePars))

## other arguments does not work
mod <- CTCRW(as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60)
is_true(is.null(mod$getTMBmap(hello=FALSE)$movePars))

###########
## GDCRW ##
###########

## GDCRW equaldecay works
mod <- GDCRW(as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60)
is_equal(mod$getTMBmap(equaldecay=TRUE)$movePars,factor(c(1,1,3,4,5,6)))
is_true(is.null(mod$getTMBmap(equaldecay=FALSE)$movePars))

## GDCRW equaldrift works
mod <- GDCRW(as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60)
is_equal(mod$getTMBmap(equaldrift=TRUE)$movePars,factor(c(1,2,3,4,5,5)))
is_true(is.null(mod$getTMBmap(equaldrift=FALSE)$movePars))

## GDCRW fixdrift works
mod <- GDCRW(as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60)
is_equal(mod$getTMBmap(fixdrift=TRUE)$movePars,factor(c(1,2,3,4,NA,NA)))
is_true(is.null(mod$getTMBmap(fixdrift=FALSE)$movePars))


## GDCRW fixrotation works
mod <- GDCRW(as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60)
is_equal(mod$getTMBmap(fixrotation=TRUE)$movePars,factor(c(1,2,NA,4,5,6)))
is_true(is.null(mod$getTMBmap(fixrotation=FALSE)$movePars))

## GDCRW fixmovecor works
mod <- GDCRW(as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60)
is_equal(mod$getTMBmap(fixmovecor=TRUE)$movePars,factor(c(1,2,3,NA,5,6)))
is_true(is.null(mod$getTMBmap(fixmovecor=FALSE)$movePars))

## GDCRW equalvar works
mod <- GDCRW(as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60)
is_equal(mod$getTMBmap(equalvar=TRUE)$logSdState,factor(c(1,1)))
is_true(is.null(mod$getTMBmap(fixmovecor=FALSE)$movePars))

## GDCRW several works
mod <- GDCRW(as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60)
is_equal(mod$getTMBmap(equaldecay=TRUE,fixdrift=TRUE)$movePars,
         factor(c(1,1,3,4,NA,NA)))

###########
## DCRW ##
###########

## DCRW fixrotation works
mod <- DCRW(as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60)
is_equal(mod$getTMBmap(fixrotation=TRUE)$movePars,factor(c(1,NA,3)))
is_true(is.null(mod$getTMBmap(fixrotation=FALSE)$movePars))

## DCRW fixmovecor works
mod <- DCRW(as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60)
is_equal(mod$getTMBmap(fixmovecor=TRUE)$movePars,factor(c(1,2,NA)))
is_true(is.null(mod$getTMBmap(fixmovecor=FALSE)$movePars))

## DCRW equalvar works
mod <- DCRW(as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60)
is_equal(mod$getTMBmap(equalvar=TRUE)$logSdState,factor(c(1,1)))
is_true(is.null(mod$getTMBmap(fixmovecor=FALSE)$movePars))


## DCRW several works
mod <- DCRW(as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60)
is_equal(mod$getTMBmap(fixrotation=TRUE,fixmovecor=TRUE)$movePars,
         factor(c(1,NA,NA)))



#########
## OUL ##
#########

mod <- OUL(as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60)
is_equal(mod$getTMBmap()$movePars,factor(c(1,2,3,4,5,6)))
## OUL symdecay
is_equal(mod$getTMBmap(symdecay=TRUE)$movePars,factor(c(1,2,2,4,5,6)))

## OUL diagdecay
is_equal(mod$getTMBmap(diagdecay=TRUE)$movePars,factor(c(1,NA,NA,4,5,6)))

## OUL equaldecay
is_equal(mod$getTMBmap(equaldecay=TRUE)$movePars,factor(c(1,2,3,1,5,6)))

## OUL sym equal  decay
is_equal(mod$getTMBmap(symdecay = TRUE,equaldecay=TRUE)$movePars,factor(c(1,2,2,1,5,6)))

## OUL diag equal  decay
is_equal(mod$getTMBmap(diagdecay = TRUE,equaldecay=TRUE)$movePars,factor(c(1,NA,NA,1,5,6)))


## OUL equaldrift
is_equal(mod$getTMBmap(equaldrift=TRUE)$movePars,factor(c(1,2,3,4,5,5)))

## OUL fixdrift
is_equal(mod$getTMBmap(fixdrift=TRUE)$movePars,factor(c(1,2,3,4,NA,NA)))
