

check_movement_model_simulation_input<- function(mov,nPar,useIndx){

#### movement model simulation input

    ## movement model simulation can be given starting value
    expr <- sprintf("simTrack(%s(as.POSIXct(\"2017-01-01 00:00:00\") + (1:%s)*60*60),%s,%s)",mov,100,1,"rep(0,nPar)")
    is_equal(eval(parse(text=expr))[,1,1],rep(0,nPar))
    inp <- rnorm(nPar,0,10)
    expr <- sprintf("simTrack(%s(as.POSIXct(\"2017-01-01 00:00:00\") + (1:%s)*60*60),%s,%s)",mov,100,1,paste0(deparse(inp),collapse=""))
    is_equal(eval(parse(text=expr))[,1,1],inp)
    
}



check_movement_model_simulation_output<- function(mov,nPar){

#### movement model simulation output
    ## movement model simulation has numeric array output with correct dimensions
    if(check_env_var("RUN_ALL_TESTS") & !check_env_var("TRAVIS")){
        for(n in c(5,10,100))
            for(j in c(1,5,10)){
                expr <- sprintf("simTrack(%s(as.POSIXct(\"2017-01-01 00:00:00\") + (1:%s)*60*60),%s)",mov,n,j)
                is_true(is.array(eval(parse(text=expr))))
                is_true(is.numeric(eval(parse(text=expr))))
                is_equal(dim(eval(parse(text=expr))),c(nPar,n,j))
            }
    }
}
