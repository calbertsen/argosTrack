#' @import TMB

.onLoad <- function(libname,pkgname){
    library.dynam("argosTrack", pkgname, libname)
}

.onUnload <- function(libpath){
    library.dynam.unload("argosTrack", libpath)
}
