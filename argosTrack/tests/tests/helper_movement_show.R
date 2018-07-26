

check_movement_model_show_method <- function(mov){
    expr <- sprintf('%s(as.POSIXct("2017-01-01 00:00:00") + (1:100) * 60 * 60)',mov)
#### movement model show
    ## movement model show method gives output
    mod1 <- eval(parse(text=expr))
    gives_output(mod1)
}
