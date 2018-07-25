
context("Test getTrack function")

test_that("getTrack returns data.frame",
{
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
    expect_equivalent(class(getTrack(mov)),"data.frame")
    expect_equivalent(class(getTrack(obs)),"data.frame")
    expect_equivalent(class(getTrack(anim)),"data.frame")
})

test_that("getTrack data.frame for animal has same columns as Movement and Observation",
{
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
    expect_true(all(colnames(a) %in% colnames(ab)))
    expect_true(all(colnames(b) %in% colnames(ab)))
    expect_equivalent(unique(ab[ab$dates %in% a$dates,colnames(a)]),unique(a))
    expect_equivalent(unique(ab[ab$dates %in% b$dates,colnames(b)]),unique(b))
})
