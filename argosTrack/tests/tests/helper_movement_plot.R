

check_movement_model_plots <- function(mov){
#### movement model plot functions
    ## movement model plot functions does not give error
    for(naut in c(TRUE,FALSE))
        for(plotSd in c(TRUE,FALSE))
            for(plotType in c("plot","plotLat","plotLon","plotMap")){
                expr <- sprintf('%s(%s(as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60,nauticalStates=%s),sd=%s)',plotType,mov,naut,plotSd)
                gives_no_output(eval(parse(text=expr)))            
            gives_no_output(eval(parse(text=expr)))
        }

    ## movement model getRange
    mod <- eval(parse(text=sprintf('%s(as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60)',mov)))
    ## Default values
    is_equal(mod$getRange(FALSE),t(apply(mod$mu,1,range)))
    ## Should also work with random values
    mod$mu[] <- runif(length(mod$mu))
    is_equal(mod$getRange(FALSE),t(apply(mod$mu,1,range)))
    ## And with sd; special case s.t. it should be range of mu +/- 2 * sdmu
    mod$sdmu[] <- 1
    is_equal(mod$getRange(TRUE),t(apply(mod$mu,1,range)) + matrix(c(-2,-2,2,2),2,2))
    
}
