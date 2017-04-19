

check_movement_model_plots <- function(mov){
    context(sprintf("%s movement model plot functions",mov))
    test_that(sprintf("%s movement model plot functions does not give error",mov),
    {
        for(naut in c(TRUE,FALSE))
            for(plotSd in c(TRUE,FALSE))
                for(plotType in c("plot","plotLat","plotLon","plotMap")){
                    expr <- sprintf('%s(%s(as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60,nauticalStates=%s),sd=%s)',plotType,mov,naut,plotSd)
            expect_null(eval(parse(text=expr)))
            
            expect_null(eval(parse(text=expr)))
        }
    })

    test_that(sprintf("%s movement model getRange",mov),
    {
        mod <- eval(parse(text=sprintf('%s(as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60)',mov)))
        ## Default values
        expect_equivalent(mod$getRange(FALSE),t(apply(mod$mu,1,range)))
        ## Should also work with random values
        mod$mu[] <- runif(length(mod$mu))
        expect_equivalent(mod$getRange(FALSE),t(apply(mod$mu,1,range)))
        ## And with sd; special case s.t. it should be range of mu +/- 2 * sdmu
        mod$sdmu[] <- 1
        expect_equivalent(mod$getRange(TRUE),t(apply(mod$mu,1,range)) + matrix(c(-2,-2,2,2),2,2))
    })
    
}
