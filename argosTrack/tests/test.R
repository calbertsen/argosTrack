
run_tests <- function(package,dir){
    library(package)
    env <- new.env(parent = getNamespace(package))
    setPackageName(paste(package,as.character(Sys.time()),sep="-"),env)
    sys.source(file.path(dir,"000-testing_functions.R"),env, keep.source = TRUE)
    env$Test <- env$TestRC()
    lf <- list.files(dir,"^helper(-|_).+\\.R", full.names = TRUE)
    invisible(sapply(lf,sys.source,envir=env, keep.source = TRUE))
    lftest <- list.files(dir,"^test(-|_).+\\.R", full.names = TRUE)
    invisible(sapply(lftest,sys.source,envir=env, keep.source = TRUE, toplevel.env=getNamespace(package)))
    unloadNamespace(getPackageName(env))
    env$Test
}

run_tests("argosTrack","tests")
