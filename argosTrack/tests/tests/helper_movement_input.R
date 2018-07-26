
check_movement_model_input <- function(mov,nPar,nVarPar,irregular){

   ## movement model - date input

    ## Test that movement model can be created with regular time steps
    expr <- sprintf('class(%s(as.POSIXct("2017-01-01 00:00:00") + (0:100) * 60 * 60))',mov)
    is_equal(eval(parse(text=expr)),
                 mov)

    ## Test that movement model can be created with irregular time steps
    expr <- sprintf('class(%s(as.POSIXct("2017-01-01 00:00:00") + cumsum(rexp(100) * 60 * 60)))',mov)
    if(irregular){
        is_equal(eval(parse(text=expr)),
                              mov)
    }else{
        gives_error(eval(parse(text=expr)))
    }
    

    ## Test that unsorted dates fails
    ## movement model can not be created with unsorted dates
    expr <- sprintf('class(%s(as.POSIXct("2017-01-01 00:00:00") + (sample(0:100,100) * 60 * 60)))',mov)
    gives_error(eval(parse(text=expr)))

    ## Test that other input classes gives error
    ## movement model can not be created with POSIXlt dates
    expr <- sprintf('%s(as.POSIXlt("2017-01-01 00:00:00"))',mov)
    gives_error(eval(parse(text=expr)))
    ## movement model can not be created with string dates
    expr <- sprintf('%s("2017-01-01 00:00:00")',mov)
    gives_error(eval(parse(text=expr)))

    ## movement model can not be created with numeric dates
    expr <- sprintf('%s(431)',mov)
    gives_error(eval(parse(text=expr)))
    ## movement model can not be created with integer dates
    expr <- sprintf('%s(431L)',mov)
    gives_error(eval(parse(text=expr)))

    ## movement model can not be created with logic dates
        expr <- sprintf('%s(TRUE)',mov)
    gives_error(eval(parse(text=expr)))
    ## movement model can not be created with env dates
    expr <- sprintf('%s(new.env())',mov)
    gives_error(eval(parse(text=expr)))


#### movement model - movement parameter input

    ## Test that movement model can only be created with correct number of movement parameters, i.e., 0

    ## movement model can only be created with %s movement parameters
    for(i in 0:10){
        exprI <- sprintf('class(%s(as.POSIXct("2017-01-01 00:00:00") + (0:100) * 60 * 60, pars = integer(%s)))',mov,i)
        exprN <- sprintf('class(%s(as.POSIXct("2017-01-01 00:00:00") + (0:100) * 60 * 60, pars = numeric(%s)))',mov,i)
        if(i==nPar){
            is_equal(eval(parse(text=exprI)),
                             mov)
            is_equal(eval(parse(text=exprN)),
                             mov)
        }else{
            gives_error(eval(parse(text=exprI)))
            gives_error(eval(parse(text=exprN)))
        }
    }

    ## Test that other input types errors
    ## movement model can not be created with logical movement parameters
    expr <- sprintf('class(DIRAC(as.POSIXct("2017-01-01 00:00:00") + (0:100) * 60 * 60, pars = logical(%s)))',mov,nPar)
    gives_error(eval(parse(text=expr)))

    ## movement model can not be created with character movement parameters
    expr <- sprintf('class(DIRAC(as.POSIXct("2017-01-01 00:00:00") + (0:100) * 60 * 60, pars = character(%s)))',mov,nPar)
    gives_error(eval(parse(text=expr)))

#### movement model - variance parameter input

    ## movement model can only be created with %s variance parameters
    for(i in 0:10){
        exprI <- sprintf('class(%s(as.POSIXct("2017-01-01 00:00:00") + (0:100) * 60 * 60, varPars = integer(%s)))',mov,i)
        exprN <- sprintf('class(%s(as.POSIXct("2017-01-01 00:00:00") + (0:100) * 60 * 60, varPars = numeric(%s)))',mov,i)
        if(i==nVarPar){
            is_equal(eval(parse(text=exprI)),
                                  mov)
            is_equal(eval(parse(text=exprN)),
                             mov)
        }else{
                gives_error(eval(parse(text=exprI)))
                gives_error(eval(parse(text=exprN)))
            }
    }

    ## Test that other input types errors
    ## movement model can not be created with logical variance parameters
    expr <- sprintf('class(%s(as.POSIXct("2017-01-01 00:00:00") + (0:100) * 60 * 60, varPars = logical(%s)))',mov,nVarPar)
    gives_error(eval(parse(text=expr)))

    ## movement model can not be created with character movement parameters
    expr <- sprintf('class(%s(as.POSIXct("2017-01-01 00:00:00") + (0:100) * 60 * 60, varPars = character(%s)))',mov,nVarPar)
    gives_error(eval(parse(text=expr)))

#### movement model - nautical state input

    ## movement model accepts TRUE and FALSE in nauticalStates
    accepts <- c(TRUE,FALSE)
    for(aa in accepts){
        expr <- sprintf('class(%s(as.POSIXct("2017-01-01 00:00:00") + (0:100) * 60 * 60, nauticalStates = %s))',mov,aa)
        is_equal(eval(parse(text=expr)),
                         mov)
    }

    ## movement model errors with other nauticalStates inputs
    expr <- sprintf('%s(as.POSIXct("2017-01-01 00:00:00") + (0:100) * 60 * 60, nauticalStates = 0)',mov)
    gives_error(eval(parse(text=expr)))
    expr <- sprintf('%s(as.POSIXct("2017-01-01 00:00:00") + (0:100) * 60 * 60, nauticalStates = 0)',mov)
    gives_error(eval(parse(text=expr)))
    expr <- sprintf('%s(as.POSIXct("2017-01-01 00:00:00") + (0:100) * 60 * 60, nauticalStates = 0L)',mov)
    gives_error(eval(parse(text=expr)))
    expr <- sprintf('%s(as.POSIXct("2017-01-01 00:00:00") + (0:100) * 60 * 60, nauticalStates = "TRUE")',mov)
    gives_error(eval(parse(text=expr)))
    
#### movement model - time unit input

    ## movement model accepts any unit from difftime
    accepts <- eval(formals(difftime)$units)
    for(aa in accepts){
        expr <- sprintf('class(%s(as.POSIXct("2017-01-01 00:00:00") + (0:100) * 60 * 60, timeunit = \"%s\"))',mov,aa)
        is_equal(eval(parse(text=expr)),
                         mov)
    }

    ## movement model errors with other inputs"
    accepts <- c("h","hour","hello","world","!")
    for(aa in accepts){
        expr <- sprintf('class(%s(as.POSIXct("2017-01-01 00:00:00") + (0:100) * 60 * 60, timeunit = \"%s\"))',mov,aa)
        gives_error(eval(parse(text=expr)))
    }
    ## movement model errors with other input classes
    expr <- sprintf('%s(as.POSIXct("2017-01-01 00:00:00"), timeunit = 2)',mov)
    gives_error(eval(parse(text=expr)))
    expr <- sprintf('%s(as.POSIXct("2017-01-01 00:00:00"), timeunit = TRUE)',mov)
    gives_error(eval(parse(text=expr)))
    expr <- sprintf('%s(as.POSIXct("2017-01-01 00:00:00"), timeunit = new.env())',mov)
    gives_error(eval(parse(text=expr)))
   
}
