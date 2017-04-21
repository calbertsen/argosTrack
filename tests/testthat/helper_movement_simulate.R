

check_movement_model_simulation_input<- function(mov,nPar,useIndx){

    context(sprintf("%s movement model simulation input",mov))

    test_that(sprintf("%s movement model simulation can be given starting value",mov),
    {
        expr <- sprintf("simTrack(%s(as.POSIXct(\"2017-01-01 00:00:00\") + (1:%s)*60*60),%s,%s)",mov,100,1,"rep(0,nPar)")
        expect_equivalent(eval(parse(text=expr))[,1,1],rep(0,nPar))
        inp <- rnorm(nPar,0,10)
        expr <- sprintf("simTrack(%s(as.POSIXct(\"2017-01-01 00:00:00\") + (1:%s)*60*60),%s,%s)",mov,100,1,paste0(deparse(inp),collapse=""))
        expect_equivalent(eval(parse(text=expr))[,1,1],inp)
    })  
    
}



check_movement_model_simulation_output<- function(mov,nPar){

    context(sprintf("%s movement model simulation output",mov))

    test_that(sprintf("%s movement model simulation has numeric array output with correct dimensions",mov),
    {
        skip_on_travis()
        for(n in c(5,10,100))
            for(j in c(1,5,10)){
                expr <- sprintf("simTrack(%s(as.POSIXct(\"2017-01-01 00:00:00\") + (1:%s)*60*60),%s)",mov,n,j)
                expect_true(is.array(eval(parse(text=expr))),info=sprintf("array %s-%s",n,j))
                expect_true(is.numeric(eval(parse(text=expr))),info=sprintf("numeric %s-%s",n,j))
                expect_equivalent(dim(eval(parse(text=expr))),c(nPar,n,j),info=sprintf("dim %s-%s",n,j))
            }
    })
}
