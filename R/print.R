

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

#' @export

print.summary_argostrack_bootstrap <- function(object){
	cat("MSE in latitude direction:\n\n")
	print(object$mseLatitude)
	cat("\nMSE in longitude direction:\n\n")
	print(object$mseLongitude)
	cat("Proportion converged:\n\n")
	print(apply(object$converged,2,function(x)x/sum(x)))
}

#' @export
print.summary_argostrack <- function(object){
	cat("Fitted track to Argos data\n\n")
	cat(paste(ifelse(object$converged,"Converged","Not converged"),"with a negative log likelihood of",round(sr$nlogLik,3),"\n\n"))
	cat(paste("Number of parameters:",object$numpar,"\n\n"))
	cat(paste("Number of observations:",object$nobs,"\n\n"))
	cat("Location classes:\n\n")
	print(object$locationclasses)

}
