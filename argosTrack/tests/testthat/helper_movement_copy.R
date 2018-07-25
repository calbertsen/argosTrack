

check_movement_model_copy_method <- function(mov){

    expr <- sprintf('%s(as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60)',mov)
    fieldNames <- names(getClass(mov)@fieldClasses)

    context(sprintf("%s movement model copy",mov))
    test_that(sprintf("%s movement model is copied correctly",mov),
    {
        for(fn in fieldNames){
            mod1 <- eval(parse(text=expr))
            mod2 <- mod1$copy()
            expect_equal(mod1$field(fn),
                         mod2$field(fn))
        }
    })
    test_that(sprintf("%s movement model is shallow copied correctly",mov),
    {
        for(fn in fieldNames){
            mod1 <- eval(parse(text=expr))
            mod2 <- mod1$copy(shallow = TRUE)
            expect_equal(mod1$field(fn),
                         mod2$field(fn))
        }
    })
}
