

check_movement_model_copy_method <- function(mov){

    expr <- sprintf('%s(as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60)',mov)
    fieldNames <- names(getClass(mov)@fieldClasses)

    ## movement model can be copied
    mod1 <- eval(parse(text=expr))
    mod2 <- mod1$copy()    
    for(fn in fieldNames){
        is_identical(mod1$field(fn),
                         mod2$field(fn))
    }

    ## movement model can be shallow copied
    mod1 <- eval(parse(text=expr))
    mod2 <- mod1$copy(shallow = TRUE)
    for(fn in fieldNames){
        is_identical(mod1$field(fn),
                         mod2$field(fn))
    }
}
