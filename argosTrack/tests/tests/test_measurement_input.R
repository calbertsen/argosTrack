#### Test Measurement input checks

## model should ne n, t or sh
is_equal(class(Measurement("n")),"Measurement")
is_equal(class(Measurement("t")),"Measurement")
gives_warning(Measurement("sh")) # Sh should give warning
is_equal(class(suppressWarnings(Measurement("sh"))),"Measurement") # should return a measurement class - but we suppress the warnings first
gives_error(Measurement("exponential"))

## logSdObs should be numeric vector of length 2
is_equal(class(Measurement("n",logSdObs = c(0,0))),"Measurement") # OK
is_equal(class(Measurement("n",logSdObs = c(0L,1L))),"Measurement") #Not numeric but integer is OK
gives_error(Measurement("n",logSdObs = c("A","B"))) #Not numeric
gives_error(Measurement("n",logSdObs = c(0))) #Not length 2
gives_error(Measurement("n",logSdObs = c(0,1,2))) #Not length 2
gives_error(Measurement("n",logSdObs = matrix(0,2,1))) #Not vector
gives_error(Measurement("n",logSdObs = matrix(5,1,2))) #Not vector

## corObs should be numeric scalar
is_equal(class(Measurement("n",corObs = 0)),"Measurement") # OK
is_equal(class(Measurement("n",corObs = 0L)),"Measurement") # Not numeric but integer is OK
gives_error(Measurement("n",corObs = "A")) #Not numeric
gives_error(Measurement("n",corObs = c(0,0))) #Not length 1
gives_error(Measurement("n",corObs = numeric(0))) #Not length 1
gives_error(Measurement("n",corObs = matrix(0,1,1))) #Not scalar
gives_error(Measurement("n",corObs = matrix(5,1,1))) #Not scalar


## logCorrection must be a 2x7 numeric matrix
is_equal(class(Measurement("n",logCorrection = matrix(rnorm(14),2,7))),"Measurement") # OK
is_equal(class(Measurement("n",logCorrection = matrix(as.integer(rnorm(14)),2,7))),"Measurement") # Not numeric but integer OK
gives_error(Measurement("n",logCorrection = matrix("A",2,7))) # Not numeric
gives_error(Measurement("n",logCorrection = as.vector(matrix(rnorm(14),2,7)))) # Not matrix
gives_error(Measurement("n",logCorrection = as.data.frame(matrix(rnorm(14),2,7)))) # Not matrix
gives_error(Measurement("n",logCorrection = matrix(rnorm(12),2,6))) # Not correct dimension
gives_error(Measurement("n",logCorrection = matrix(rnorm(7),1,7))) # Not correct dimension

## splineXlogSD should be numeric scalar
is_equal(class(Measurement("n",splineXlogSd = 0)),"Measurement") # OK
is_equal(class(Measurement("n",splineXlogSd = 0L)),"Measurement") # Not numeric but integer is OK
gives_error(Measurement("n",splineXlogSd = "A")) #Not numeric
gives_error(Measurement("n",splineXlogSd = c(0,0))) #Not length 1
gives_error(Measurement("n",splineXlogSd = numeric(0))) #Not length 1
gives_error(Measurement("n",splineXlogSd = matrix(0,1,1))) #Not scalar
gives_error(Measurement("n",splineXlogSd = matrix(5,1,1))) #Not scalar

## knotPars should be numeric vector
is_equal(class(Measurement("n",knotPars = numeric(2))),"Measurement") # OK
is_equal(class(Measurement("n",knotPars = numeric(3))),"Measurement") # OK
is_equal(class(Measurement("n",knotPars = numeric(4))),"Measurement") # OK
is_equal(class(Measurement("n",knotPars = numeric(5))),"Measurement") # OK
is_equal(class(Measurement("n",knotPars = numeric(6))),"Measurement") # OK
is_equal(class(Measurement("n",knotPars = numeric(100))),"Measurement") # OK
is_equal(class(Measurement("n",knotPars = c(0L,1L))),"Measurement") #Not numeric but integer is OK
gives_error(Measurement("n",knotPars = c("A","B"))) #Not numericx
gives_error(Measurement("n",knotPars = matrix(0,2,1))) #Not vector
gives_error(Measurement("n",knotPars = matrix(5,1,2))) #Not vector



## df should be numeric vector of length 8
is_equal(class(Measurement("n",df = numeric(8))),"Measurement") # OK
is_equal(class(Measurement("n",df = integer(8))),"Measurement") #Not numeric but integer is OK
gives_error(Measurement("n",df = character(8))) #Not numeric
gives_error(Measurement("n",df = numeric(7))) #Not length 8
gives_error(Measurement("n",df = numeric(9))) #Not length 8
gives_error(Measurement("n",df = matrix(0,8,1))) #Not vector
gives_error(Measurement("n",df = matrix(5,1,8))) #Not vector



## minDf should be non negative numeric scalar
is_equal(class(Measurement("n",minDf = 0)),"Measurement") # OK
is_equal(class(Measurement("n",minDf = 0L)),"Measurement") # Not numeric but integer is OK
gives_error(Measurement("n",minDf = "A")) #Not numeric
gives_error(Measurement("n",minDf = c(0,0))) #Not length 1
gives_error(Measurement("n",minDf = numeric(0))) #Not length 1
gives_error(Measurement("n",minDf = matrix(0,1,1))) #Not scalar
gives_error(Measurement("n",minDf = matrix(5,1,1))) #Not scalar
gives_error(Measurement("n",minDf = -0.5)) #Not non negative



## nauticalObs should be logical vector of lenght 1
is_equal(class(Measurement("n",nauticalObs = TRUE)),"Measurement") # OK
is_equal(class(Measurement("n",nauticalObs = FALSE)),"Measurement") # Not numeric but integer is OK
gives_error(Measurement("n",nauticalObs = "A")) #Not numeric
gives_error(Measurement("n",nauticalObs = 1)) #Not length 1
gives_error(Measurement("n",nauticalObs = logical(2))) #Not length 1
gives_error(Measurement("n",nauticalObs = matrix(logical(1),1,1))) #Not scalar
gives_error(Measurement("n",nauticalObs = matrix(logical(1),1,1))) #Not scalar
