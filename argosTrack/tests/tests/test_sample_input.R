
#### Test input checks for sampling functions

## rmvnorm handles input
## Sigma must be numeric square matrix
gives_error(rmvnorm(1,c(0,0),c(0,0))) ## Not matrix
gives_error(rmvnorm(1,c(0,0),matrix("A",2,2))) ## Not numeric
gives_error(rmvnorm(1,c(0,0),matrix(0,2,5))) ## Not square
## mu must be a numeric vector
gives_error(rmvnorm(1,c("A",0),matrix(0,2,2))) ## Not numeric
gives_error(rmvnorm(1,matrix(0,2,3),matrix(0,2,2))) ## Not matrix
## mu and sigma must have compatible dimensions
gives_error(rmvnorm(1,c(0,0),diag(1,3)))
gives_error(rmvnorm(1,c(0,0,0),diag(1,2)))


is_equal(dim(rmvnorm(1,c(0,0),diag(1,2))),c(1,2)) ## OK
is_equal(dim(rmvnorm(100,c(0,0),diag(1,2))),c(100,2)) ## OK
is_true(max(abs(colMeans(rmvnorm(100000,c(1,-1),diag(1,2)))-c(1,-1))) < 1e-2) ## OK
is_true(max(abs(apply(rmvnorm(100000,c(1,-1),diag(1,2)),2,sd)-c(1,1))) < 1e-2) ## OK    
is_true(all(is.numeric(rmvnorm(100,c(0,0),diag(1,2))))) ## OK
is_true(all(is.finite(rmvnorm(100,c(0,0),diag(1,2))))) ## OK

## rmvt handles input
## df must be numeric scalar
gives_error(rmvt(1,c(0,0),diag(1,2),"A")) ## Not numeric
gives_error(rmvt(1,c(0,0),diag(1,2),c(1,2))) ## Not scalar
## Sigma must be numeric square matrix
gives_error(rmvt(1,c(0,0),c(0,0),2)) ## Not matrix
gives_error(rmvt(1,c(0,0),matrix("A",2,2),2)) ## Not numeric
gives_error(rmvt(1,c(0,0),matrix(0,2,5),2)) ## Not square
## mu must be a numeric vector
gives_error(rmvt(1,c("A",0),matrix(0,2,2),2)) ## Not numeric
gives_error(rmvt(1,matrix(0,2,3),matrix(0,2,2),2)) ## Not matrix
## mu and sigma must have compatible dimensions
gives_error(rmvt(1,c(0,0),diag(1,3),2))
gives_error(rmvt(1,c(0,0,0),diag(1,2),2))
## Check output dim
is_equal(dim(rmvt(1,c(0,0),diag(1,2),4)),c(1,2)) ## OK
is_equal(dim(rmvt(100,c(0,0),diag(1,2),4)),c(100,2)) ## OK
is_true(all(is.numeric(rmvt(100,c(0,0),diag(1,2),4)))) ## OK
is_true(all(is.finite(rmvt(100,c(0,0),diag(1,2),4)))) ## OK

## rwchauchy handles input
## mu must be numeric scalar
gives_error(rwcauchy(1,"A",2)) ## Not numeric
gives_error(rwcauchy(1,c(0,0),2)) ## Not scalar
## gamma must be positive numeric scalar
gives_error(rwcauchy(1,2,"A")) ## Not numeric
gives_error(rwcauchy(1,2,c(0,0))) ## Not scalar
gives_error(rwcauchy(1,2,-1)) ## Not positive
## Check output dim
is_equal(dim(rwcauchy(1,0,1)),NULL) ## OK
is_equal(length(rwcauchy(1,0,1)),1) ## OK
is_equal(dim(rwcauchy(100,0,1)),NULL) ## OK
is_equal(length(rwcauchy(100,0,1)),100) ## OK
is_true(all(is.numeric(rwcauchy(100,0,1)))) ## OK
is_true(all(is.finite(rwcauchy(100,0,1)))) ## OK


## rmvsh handles input
## sigma must be numeric square vector
gives_error(rmvsh(1,c(0,0),c(0,0),1)) ## Not matrix
gives_error(rmvsh(1,c(0,0),matrix("A",2,2),1)) ## Not numeric
gives_error(rmvsh(1,c(0,0),matrix(0,2,5),1)) ## Not square
## mu must be a numeric vector
gives_error(rmvsh(1,c("A",0),matrix(0,2,2),1)) ## Not numeric
gives_error(rmvsh(1,matrix(0,2,3),matrix(0,2,2),1)) ## Not matrix
## mu and sigma must have compatible dimensions
gives_error(rmvsh(1,c(0,0),diag(1,3),1))
gives_error(rmvsh(1,c(0,0,0),diag(1,2),1))
## delta must be positive scalar
gives_error(rmvsh(1,c(0,0),diag(1,2),0)) ## Not positive
gives_error(rmvsh(1,c(0,0),diag(1,2),-1)) ## Not positive
gives_error(rmvsh(1,c(0,0),diag(1,2),"A")) ## Not numeric
gives_error(rmvsh(1,c(0,0),diag(1,2),c(1,1))) ## Not scalar


is_equal(dim(rmvsh(1,c(0,0),diag(1,2),1)),c(1,2)) ## OK
is_equal(dim(rmvsh(5,c(0,0),diag(1,2),1)),c(5,2)) ## OK
# OK - variance should be larger than for rmvnorm
is_true(all(is.numeric(rmvsh(5,c(0,0),diag(1,2),1)))) ## OK
is_true(all(is.finite(rmvsh(5,c(0,0),diag(1,2),1)))) ## OK

