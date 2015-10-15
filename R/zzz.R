#' @import TMB

.onLoad <- function(libname,pkgname){
    cat("Loading compiled code...\n")
    library.dynam("argosTrack", pkgname, libname)
}
