

#' @export

print.argostrack <- function(object){
    cat("Time to estimate:\n")
    print(unname(object$estimation_time[3]))
    cat("\nOptimization result:\n")
    print(object$optimization)
}
#' @export

print.argostrack_bootstrap <- function(object){
    print(object$mse)
}
