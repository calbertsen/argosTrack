

context("Movement abstract class")

test_that("Movement class fails to be constructed",
{
    expect_error(Movement())
    expect_error(Movement(as.POSIXct("2017-01-01 00:00:00")))
})


for(mm in moveNamesUse)
    check_movement_model_input(mov = mm,
                               nPar = length(eval(formals(getClass(mm)@refMethods$initialize)$pars)),
                               nVarPar = length(eval(formals(getClass(mm)@refMethods$initialize)$varPars)),
                               irregular = !(mm %in% regulars)) ## regulars is defined in helper_000_variables.R
