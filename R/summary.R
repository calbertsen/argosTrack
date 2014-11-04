
#' @export

summary.argostrack <- function(object, ...){
	npar <- length(object$optimization$par)
	logLik <- object$optimization$objective
	conv <- object$optimization$convergence == 0
	nobs <- dim(object$observations[,object$tmb_object$env$data$include==1])[2]
	lcs <- table(object$locationclass[object$tmb_object$env$data$include==1])
	daterange <- range(object$dates)
	
	res<-list(numpar = npar,
		nlogLik = logLik,
		converged = conv,
		nobs = nobs,
		locationclasses = lcs,
		daterange = daterange)
	class(res) <- "summary_argostrack"
	return(res)
}
#' @export

summary.argostrack_bootstrap <- function(object, ...){
	convUse <- object$convergence
	convUse[is.na(convUse)] <- 1
    res <- list(mseLatitude = apply(object$mse,3,function(x)summary(x[1,])),
    	mseLongitude = apply(object$mse,3,function(x)summary(x[2,])),
    	converged = apply(convUse,3,function(x)table(x[1,]==0))
	)
	class(res) <- "summary_argostrack_bootstrap"
	return(res)
}
