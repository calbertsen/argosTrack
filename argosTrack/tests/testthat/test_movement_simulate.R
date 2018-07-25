
## Create data frame of movement models to test

for(mm in moveNamesUse){
    np <- length(eval(formals(getClass(mm)@refMethods$simulate)$x0))
    check_movement_model_simulation_input(mov = mm,
                                          nPar = np)
    check_movement_model_simulation_output(mov = mm,
                                           nPar = np)
}
