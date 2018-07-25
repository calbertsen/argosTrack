
context("Test custom map for movement models")

###########
## CTCRW ##
###########

test_that("CTCRW equaldecay works",
{
    mod <- CTCRW(as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60)
    expect_equivalent(mod$getTMBmap(equaldecay=TRUE)$movePars,
                      factor(c(1,1,3,4)))
    expect_null(mod$getTMBmap(equaldecay=FALSE)$movePars)
})

test_that("CTCRW equaldrift works",
{
    mod <- CTCRW(as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60)
    expect_equivalent(mod$getTMBmap(equaldrift=TRUE)$movePars,
                      factor(c(1,2,3,3)))
    expect_null(mod$getTMBmap(equaldrift=FALSE)$movePars)
})

test_that("CTCRW fixdrift works",
{
    mod <- CTCRW(as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60)
    expect_equivalent(mod$getTMBmap(fixdrift=TRUE)$movePars,
                      factor(c(1,2,NA,NA)))
    expect_null(mod$getTMBmap(fixdrift=FALSE)$movePars)
})

test_that("CTCRW all arguments works",
{
    mod <- CTCRW(as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60)
    expect_equivalent(mod$getTMBmap(equaldecay=TRUE,fixdrift=TRUE)$movePars,
                      factor(c(1,1,NA,NA)))
    expect_equivalent(mod$getTMBmap(equaldecay=TRUE,equaldrift=TRUE)$movePars,
                      factor(c(1,1,3,3)))
    ## Fix drift overwrites equaldrift
    expect_equivalent(mod$getTMBmap(equaldrift=TRUE,fixdrift=TRUE)$movePars,
                      factor(c(1,2,NA,NA)))
    expect_equivalent(mod$getTMBmap(equaldecay=TRUE,equaldrift=TRUE,fixdrift=TRUE)$movePars,
                      factor(c(1,1,NA,NA)))
})

test_that("CTCRW equalvar works",
{
    mod <- CTCRW(as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60)
    expect_equivalent(mod$getTMBmap(equalvar=TRUE)$logSdState,factor(c(1,1)))
    expect_null(mod$getTMBmap(fixmovecor=FALSE)$movePars)
})


test_that("other arguments does not work",
{
    mod <- CTCRW(as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60)
    expect_null(mod$getTMBmap(hello=FALSE)$movePars)
})


###########
## IDCRW ##
###########

test_that("IDCRW equaldecay works",
{
    mod <- IDCRW(as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60)
    expect_equivalent(mod$getTMBmap(equaldecay=TRUE)$movePars,factor(c(1,1,3,4,5,6)))
    expect_null(mod$getTMBmap(equaldecay=FALSE)$movePars)
})

test_that("IDCRW equaldrift works",
{
    mod <- IDCRW(as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60)
    expect_equivalent(mod$getTMBmap(equaldrift=TRUE)$movePars,factor(c(1,2,3,4,5,5)))
    expect_null(mod$getTMBmap(equaldrift=FALSE)$movePars)
})

test_that("IDCRW fixdrift works",
{
    mod <- IDCRW(as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60)
    expect_equivalent(mod$getTMBmap(fixdrift=TRUE)$movePars,factor(c(1,2,3,4,NA,NA)))
    expect_null(mod$getTMBmap(fixdrift=FALSE)$movePars)
})


test_that("IDCRW fixrotation works",
{
    mod <- IDCRW(as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60)
    expect_equivalent(mod$getTMBmap(fixrotation=TRUE)$movePars,factor(c(1,2,NA,4,5,6)))
    expect_null(mod$getTMBmap(fixrotation=FALSE)$movePars)
})


test_that("IDCRW fixmovecor works",
{
    mod <- IDCRW(as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60)
    expect_equivalent(mod$getTMBmap(fixmovecor=TRUE)$movePars,factor(c(1,2,3,NA,5,6)))
    expect_null(mod$getTMBmap(fixmovecor=FALSE)$movePars)
})

test_that("IDCRW equalvar works",
{
    mod <- IDCRW(as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60)
    expect_equivalent(mod$getTMBmap(equalvar=TRUE)$logSdState,factor(c(1,1)))
    expect_null(mod$getTMBmap(fixmovecor=FALSE)$movePars)
})



test_that("IDCRW several works",
{
    mod <- IDCRW(as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60)
    expect_equivalent(mod$getTMBmap(equaldecay=TRUE,fixdrift=TRUE)$movePars,
                      factor(c(1,1,3,4,NA,NA)))
})





###########
## DCRW ##
###########

test_that("DCRW fixrotation works",
{
    mod <- DCRW(as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60)
    expect_equivalent(mod$getTMBmap(fixrotation=TRUE)$movePars,factor(c(1,NA,3)))
    expect_null(mod$getTMBmap(fixrotation=FALSE)$movePars)
})


test_that("DCRW fixmovecor works",
{
    mod <- DCRW(as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60)
    expect_equivalent(mod$getTMBmap(fixmovecor=TRUE)$movePars,factor(c(1,2,NA)))
    expect_null(mod$getTMBmap(fixmovecor=FALSE)$movePars)
})

test_that("DCRW equalvar works",
{
    mod <- DCRW(as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60)
    expect_equivalent(mod$getTMBmap(equalvar=TRUE)$logSdState,factor(c(1,1)))
    expect_null(mod$getTMBmap(fixmovecor=FALSE)$movePars)
})



test_that("DCRW several works",
{
    mod <- DCRW(as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60)
    expect_equivalent(mod$getTMBmap(fixrotation=TRUE,fixmovecor=TRUE)$movePars,
                      factor(c(1,NA,NA)))
})
