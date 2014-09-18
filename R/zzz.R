#' @import TMB
#' @import mvtnorm

.onLoad <- function(libname,pkgname){
    path <- paste(libname,pkgname,"TMB","",sep="/")
    if(!file.exists(TMB::dynlib(paste0(path,"ringednn"))))
        TMB::compile(paste0(path,"ringednn.cpp"))
        if(!file.exists(TMB::dynlib(paste0(path,"ringednt"))))
        TMB::compile(paste0(path,"ringednt.cpp"))

    dyn.load(TMB::dynlib(paste0(path,"ringednn")))
    dyn.load(TMB::dynlib(paste0(path,"ringednt")))

}
