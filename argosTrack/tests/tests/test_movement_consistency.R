
if(check_env_var("RUN_ALL_TESTS"))
    for(mm in readyMoveNames){ ##moveNamesUse){
        check_movement_model_consistency(mov = mm)
    }
