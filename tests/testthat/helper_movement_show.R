

check_movement_model_show_method <- function(mov){
    expr <- sprintf('%s(as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60)',mov)
    context(sprintf("%s movement model show",mov))
    test_that(sprintf("%s movement model show method gives output",mov),
    {
        mod1 <- eval(parse(text=expr))
        expect_gt(length(capture.output(mod1)),0)
    })
}
