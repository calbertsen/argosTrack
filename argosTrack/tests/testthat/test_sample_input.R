
context("Test input checks for sampling functions")

test_that("rmvnorm handles input",
{
    ## Sigma must be numeric square matrix
    expect_error(rmvnorm(1,c(0,0),c(0,0))) ## Not matrix
    expect_error(rmvnorm(1,c(0,0),matrix("A",2,2))) ## Not numeric
    expect_error(rmvnorm(1,c(0,0),matrix(0,2,5))) ## Not square
    ## mu must be a numeric vector
    expect_error(rmvnorm(1,c("A",0),matrix(0,2,2))) ## Not numeric
    expect_error(rmvnorm(1,matrix(0,2,3),matrix(0,2,2))) ## Not matrix
    ## mu and sigma must have compatible dimensions
    expect_error(rmvnorm(1,c(0,0),diag(1,3)))
    expect_error(rmvnorm(1,c(0,0,0),diag(1,2)))

    
    expect_equal(dim(rmvnorm(1,c(0,0),diag(1,2))),c(1,2)) ## OK
    expect_equal(dim(rmvnorm(100,c(0,0),diag(1,2))),c(100,2)) ## OK
    expect_lt(max(abs(colMeans(rmvnorm(100000,c(1,-1),diag(1,2)))-c(1,-1))),1e-2) ## OK
    expect_lt(max(abs(apply(rmvnorm(100000,c(1,-1),diag(1,2)),2,sd)-c(1,1))),1e-2) ## OK    
    expect_true(all(is.numeric(rmvnorm(100,c(0,0),diag(1,2))))) ## OK
    expect_true(all(is.finite(rmvnorm(100,c(0,0),diag(1,2))))) ## OK

})



test_that("rmvt handles input",
{
    ## df must be numeric scalar
    expect_error(rmvt(1,c(0,0),diag(1,2),"A")) ## Not numeric
    expect_error(rmvt(1,c(0,0),diag(1,2),c(1,2))) ## Not scalar
      ## Sigma must be numeric square matrix
    expect_error(rmvt(1,c(0,0),c(0,0),2)) ## Not matrix
    expect_error(rmvt(1,c(0,0),matrix("A",2,2),2)) ## Not numeric
    expect_error(rmvt(1,c(0,0),matrix(0,2,5),2)) ## Not square
    ## mu must be a numeric vector
    expect_error(rmvt(1,c("A",0),matrix(0,2,2),2)) ## Not numeric
    expect_error(rmvt(1,matrix(0,2,3),matrix(0,2,2),2)) ## Not matrix
    ## mu and sigma must have compatible dimensions
    expect_error(rmvt(1,c(0,0),diag(1,3),2))
    expect_error(rmvt(1,c(0,0,0),diag(1,2),2))
    ## Check output dim
    expect_equal(dim(rmvnorm(1,c(0,0),diag(1,2))),c(1,2)) ## OK
    expect_equal(dim(rmvnorm(100,c(0,0),diag(1,2))),c(100,2)) ## OK
    expect_true(all(is.numeric(rmvnorm(100,c(0,0),diag(1,2))))) ## OK
    expect_true(all(is.finite(rmvnorm(100,c(0,0),diag(1,2))))) ## OK

})


test_that("rwchauchy handles input",
{
    ## mu must be numeric scalar
    expect_error(rwcauchy(1,"A",2)) ## Not numeric
    expect_error(rwcauchy(1,c(0,0),2)) ## Not scalar
    ## gamma must be positive numeric scalar
    expect_error(rwcauchy(1,2,"A")) ## Not numeric
    expect_error(rwcauchy(1,2,c(0,0))) ## Not scalar
    expect_error(rwcauchy(1,2,-1)) ## Not positive
    ## Check output dim
    expect_equal(dim(rwcauchy(1,0,1)),NULL) ## OK
    expect_equal(length(rwcauchy(1,0,1)),1) ## OK
    expect_equal(dim(rwcauchy(100,0,1)),NULL) ## OK
    expect_equal(length(rwcauchy(100,0,1)),100) ## OK
    expect_true(all(is.numeric(rwcauchy(100,0,1)))) ## OK
    expect_true(all(is.finite(rwcauchy(100,0,1)))) ## OK

})

