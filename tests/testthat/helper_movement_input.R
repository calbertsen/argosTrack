
check_movement_model_input <- function(mov,nPar,nVarPar,irregular){

    context(sprintf("%s movement model - date input",mov))

    ## Test that movement model can be created with regular time steps
    test_that(sprintf("%s movement model can be created with regular time steps",mov),
    {
        expr <- sprintf('class(%s(as.POSIXct("2017-01-01 00:00:00") + (0:100) * 60 * 60))',mov)
        expect_equivalent(eval(parse(text=expr)),
                          mov)
    })

    ## Test that movement model can be created with irregular time steps
    test_that(sprintf("%s movement model can%s be created with regular time steps",mov,c("'t","")[irregular+1]),
    {
        expr <- sprintf('class(%s(as.POSIXct("2017-01-01 00:00:00") + cumsum(rexp(100) * 60 * 60)))',mov)
        if(irregular){
            expect_equivalent(eval(parse(text=expr)),
                              mov)
        }else{
            expect_error(eval(parse(text=expr)))
        }
    })
    

    ## Test that unsorted dates fails
    test_that(sprintf("%s movement model can not be created with unsorted dates",mov),
    {
        expr <- sprintf('class(%s(as.POSIXct("2017-01-01 00:00:00") + (sample(0:100,100) * 60 * 60)))',mov)
        expect_error(eval(parse(text=expr)))
    })
    ## Test that other input classes gives error
    test_that(sprintf("%s movement model can not be created with POSIXlt dates",mov),
    {
        expr <- sprintf('%s(as.POSIXlt("2017-01-01 00:00:00"))',mov)
        expect_error(eval(parse(text=expr)))
    })
    test_that(sprintf("%s movement model can not be created with string dates",mov),
    {
        expr <- sprintf('%s("2017-01-01 00:00:00")',mov)
        expect_error(eval(parse(text=expr)))
    })
    test_that(sprintf("%s movement model can not be created with numeric dates",mov),
    {
        expr <- sprintf('%s(431)',mov)
        expect_error(eval(parse(text=expr)))
    })
    test_that(sprintf("%s movement model can not be created with integer dates",mov),
    {
        expr <- sprintf('%s(431L)',mov)
        expect_error(eval(parse(text=expr)))
    })
    test_that(sprintf("%s movement model can not be created with logic dates",mov),
    {
        expr <- sprintf('%s(TRUE)',mov)
        expect_error(eval(parse(text=expr)))
    })
    test_that(sprintf("%s movement model can not be created with env dates",mov),
    {
        expr <- sprintf('%s(new.env())',mov)
        expect_error(eval(parse(text=expr)))
    })



    context(sprintf("%s movement model - movement parameter input",mov))

    ## Test that movement model can only be created with correct number of movement parameters, i.e., 0

    test_that(sprintf("%s movement model can only be created with %s movement parameters",mov,nPar),
    {
        for(i in 0:10){
            exprI <- sprintf('class(%s(as.POSIXct("2017-01-01 00:00:00") + (0:100) * 60 * 60, pars = integer(%s)))',mov,i)
            exprN <- sprintf('class(%s(as.POSIXct("2017-01-01 00:00:00") + (0:100) * 60 * 60, pars = numeric(%s)))',mov,i)
            if(i==nPar){
                expect_equivalent(eval(parse(text=exprI)),
                                  mov,
                                  info = sprintf("integer(%s)",i))
                expect_equivalent(eval(parse(text=exprN)),
                                  mov,
                                  info = sprintf("numeric(%s)",i))
            }else{
                expect_error(eval(parse(text=exprI)),
                             info = sprintf("integer(%s)",i))
                expect_error(eval(parse(text=exprN)),
                             info = sprintf("numeric(%s)",i))
            }
        }
    })


    ## Test that other input types errors
    test_that(sprintf("%s movement model can not be created with logical movement parameters",mov),
    {
        expr <- sprintf('class(DIRAC(as.POSIXct("2017-01-01 00:00:00") + (0:100) * 60 * 60, pars = logical(%s)))',mov,nPar)
        expect_error(eval(parse(text=expr)))
    })
    test_that(sprintf("%s movement model can not be created with character movement parameters",mov),
    {
        expr <- sprintf('class(DIRAC(as.POSIXct("2017-01-01 00:00:00") + (0:100) * 60 * 60, pars = character(%s)))',mov,nPar)
        expect_error(eval(parse(text=expr)))
    })

    context(sprintf("%s movement model - variance parameter input",mov))

    test_that(sprintf("%s movement model can only be created with %s variance parameters",mov,nVarPar),
    {
        for(i in 0:10){
            exprI <- sprintf('class(%s(as.POSIXct("2017-01-01 00:00:00") + (0:100) * 60 * 60, varPars = integer(%s)))',mov,i)
            exprN <- sprintf('class(%s(as.POSIXct("2017-01-01 00:00:00") + (0:100) * 60 * 60, varPars = numeric(%s)))',mov,i)
            if(i==nVarPar){
                expect_equivalent(eval(parse(text=exprI)),
                                  mov,
                                  info = sprintf("integer(%s)",i))
                expect_equivalent(eval(parse(text=exprN)),
                                  mov,
                                  info = sprintf("numeric(%s)",i))
            }else{
                expect_error(eval(parse(text=exprI)),
                             info = sprintf("integer(%s)",i))
                expect_error(eval(parse(text=exprN)),
                             info = sprintf("numeric(%s)",i))
            }
        }
    })


    ## Test that other input types errors
    test_that(sprintf("%s movement model can not be created with logical variance parameters",mov),
    {
        expr <- sprintf('class(%s(as.POSIXct("2017-01-01 00:00:00") + (0:100) * 60 * 60, varPars = logical(%s)))',mov,nVarPar)
        expect_error(eval(parse(text=expr)))
    })
    test_that(sprintf("%s movement model can not be created with character movement parameters",mov),
    {
        expr <- sprintf('class(%s(as.POSIXct("2017-01-01 00:00:00") + (0:100) * 60 * 60, varPars = character(%s)))',mov,nVarPar)
        expect_error(eval(parse(text=expr)))
    })



    context(sprintf("%s movement model - nautical state input",mov))

    test_that(sprintf("%s movement model accepts TRUE and FALSE in nauticalStates",mov),
    {
        accepts <- c(TRUE,FALSE)
        for(aa in accepts){
            expr <- sprintf('class(%s(as.POSIXct("2017-01-01 00:00:00") + (0:100) * 60 * 60, nauticalStates = %s))',mov,aa)
            expect_equivalent(eval(parse(text=expr)),
                              mov,
                              info=aa)
        }
    })

    test_that(sprintf("%s movement model errors with other nauticalStates inputs",mov),
    {
        expr <- sprintf('%s(as.POSIXct("2017-01-01 00:00:00") + (0:100) * 60 * 60, nauticalStates = 0)',mov)
        expect_error(eval(parse(text=expr)))
        expr <- sprintf('%s(as.POSIXct("2017-01-01 00:00:00") + (0:100) * 60 * 60, nauticalStates = 0)',mov)
        expect_error(eval(parse(text=expr)))
        expr <- sprintf('%s(as.POSIXct("2017-01-01 00:00:00") + (0:100) * 60 * 60, nauticalStates = 0L)',mov)
        expect_error(eval(parse(text=expr)))
        expr <- sprintf('%s(as.POSIXct("2017-01-01 00:00:00") + (0:100) * 60 * 60, nauticalStates = "TRUE")',mov)
        expect_error(eval(parse(text=expr)))
    })

    
    context(sprintf("%s movement model - time unit input",mov))


    test_that(sprintf("%s movement model accepts any unit from difftime",mov),
    {
        accepts <- eval(formals(difftime)$units)
        for(aa in accepts){
            expr <- sprintf('class(%s(as.POSIXct("2017-01-01 00:00:00") + (0:100) * 60 * 60, timeunit = \"%s\"))',mov,aa)
            expect_equivalent(eval(parse(text=expr)),
                              mov,
                              info=aa)
        }
    })

    test_that(sprintf("%s movement model errors with other inputs",mov),
    {
        accepts <- c("h","hour","hello","world","!")
        for(aa in accepts){
            expr <- sprintf('class(%s(as.POSIXct("2017-01-01 00:00:00") + (0:100) * 60 * 60, timeunit = \"%s\"))',mov,aa)
            expect_error(eval(parse(text=expr)),
                              info=aa)
        }
    })
    test_that(sprintf("%s movement model errors with other input classes",mov),
    {
        expr <- sprintf('%s(as.POSIXct("2017-01-01 00:00:00"), timeunit = 2)',mov)
        expect_error(eval(parse(text=expr)))
        expr <- sprintf('%s(as.POSIXct("2017-01-01 00:00:00"), timeunit = TRUE)',mov)
        expect_error(eval(parse(text=expr)))
        expr <- sprintf('%s(as.POSIXct("2017-01-01 00:00:00"), timeunit = new.env())',mov)
        expect_error(eval(parse(text=expr)))
    })
    
}
