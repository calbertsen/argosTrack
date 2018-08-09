
run_tests <- function(package,
                      dir,
                      install_opts = c()){
    ## From utils::install.packages
    get_package_name <- function(pkg) {
        gsub("_[.](zip|tar[.]gz|tar[.]bzip2|tar[.]xz)", "",
             gsub(.standard_regexps()$valid_package_version, 
                  "", basename(pkg)))
    }
    if(grepl("(zip|tar[.]gz|tar[.]bzip2|tar[.]xz)$",package)){
        tarball <- package
        package <- get_package_name(package)
        tmpdir <- tempdir()
        utils::install.packages(tarball,
                                lib = tmpdir,
                                repos = NULL,                       
                                dependencies = NA,
                                INSTALL_opts = install_opts,
                                keep_outputs = tmpdir,
                                Ncpus = 1,
                                type = "source")
        library(package, character.only = TRUE, lib.loc = tmpdir)
    }else{
        library(package, character.only = TRUE)
    }
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



run_tests(Sys.getenv("RUNTEST_PKG","argosTrack"),"tests")
