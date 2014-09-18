
#' Extract model residuals
#'
#' @export

#residuals <- function(object, ...) UseMethod("residuals",object)


# Implementation for argostrack object

residuals.argostrack <- function(object){
    res <- object$observations - object$positions
    colnames(res) <- object$locationclass
    return(res)
}
