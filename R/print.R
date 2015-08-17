

#' @export

print.argostrack <- function(x, ...){
    cat("Time to estimate:\n")
    print(unname(x$estimation_time[3]))
    cat("\nOptimization result:\n")
    print(x$optimization)
}
#' @export

print.argostrack_bootstrap <- function(x, ...){
    print(x$mse)
}

#' @export

print.summary_argostrack_bootstrap <- function(x, ...){
	cat("MSE in latitude direction:\n\n")
	print(x$mseLatitude)
	cat("\nMSE in longitude direction:\n\n")
	print(x$mseLongitude)
	cat("Proportion converged:\n\n")
	print(apply(x$converged,2,function(x)x/sum(x)))
}

#' @export
print.summary_argostrack <- function(x, ...){
	cat("Fitted track to Argos data\n\n")
	cat(paste(ifelse(x$converged,"Converged","Not converged"),"with a negative log likelihood of",round(x$nlogLik,3),"\n\n"))
	cat(paste("Number of parameters:",x$numpar,"\n\n"))
	cat(paste("Number of observations:",x$nobs,"\n\n"))
        cat(paste("Number of (unique) predicted locations:",x$nstate,"\n\n"))
        cat(paste("Average turning angle (radians):",
                  round(Arg(mean(complex(argument=x$turningangles))),4),
                  "\n\n"))
        cat(paste("Average step length (km/h):",
                  round(mean(x$steplengths_per_hour * 1.852),4),
                  "\n\n"))
	
	cat("Location classes:\n\n")
	print(x$locationclasses)

}
