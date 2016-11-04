
##' Simulate from a multivariate normal distribution
##'
##' Simulates values from a \eqn{k}-variate normal distribution \eqn{N_k(\mu,\Sigma)} with density
##' \deqn{f(x) = \sqrt{(2\pi)^k\mid\Sigma\mid}^{-1}\exp\left( -\frac{1}{2}(x-\mu)^T\Sigma^{-1}(x-\mu)  \right)}
##' @param n Number of replications
##' @param mu Mean vector to simulate with 
##' @param sigma Covariance matrix to simulate with
##' @return A n x length(mu) matrix of simulated values 
##' @author Christoffer Moesgaard Albertsen
##' @keywords internal
rmvnorm <- function(n, mu, sigma){
    if(!is.nummat(sigma))
        stop("sigma must be a numeric matrix.")
    if(!is.numvec(mu))
        stop("mu must be a numeric vector.")
    if(dim(sigma)[1] != dim(sigma)[2])
        stop("sigma must be a square matrix.")
    if(!is.samelength(sigma,mu))
        stop("mu and sigma must have compatible dimensions")
    X <- .Call("rmvnorm",n=as.integer(n),mu=mu,sigma=sigma, PACKAGE = "argosTrack")
    return(X)
}


##' Simulate from a multivariate Student's t-distribution
##'
##' Simulates values from a \eqn{k}-variate t-distribution \eqn{t_k(\mu,\Sigma,\nu)}. If \eqn{X \sim N_k(0,\Sigma)}, \eqn{v \sim \chi_\nu ^2}, and \eqn{Y = X\sqrt{\nu/v} + \mu} then
##' \deqn{Y \sim t_k(\mu,\Sigma,\nu)}
##' @param n Number of replications
##' @param mu Mean vector (\eqn{\mu}) to simulate with 
##' @param sigma Scale matrix (\eqn{\Sigma}) to simulate with
##' @param df Degrees of freedom (\eqn{\nu}) to simulate with
##' @return A n x length(mu) matrix of simulated values 
##' @author Christoffer Moesgaard Albertsen
##' @keywords internal
rmvt <- function(n, mu, sigma, df){
    if(!is.numsca(df))
        stop("df must be a scalar.")
    if(!is.nummat(sigma))
        stop("sigma must be a numeric matrix.")
    if(!is.numvec(mu))
        stop("mu must be a numeric vector.")
    if(dim(sigma)[1] != dim(sigma)[2])
        stop("sigma must be a square matrix.")
    if(!is.samelength(sigma,mu))
        stop("mu and sigma must have compatible dimensions")
    X <- .Call("rmvt",n=as.integer(n),mu=mu,sigma=sigma,df=df, PACKAGE = "argosTrack")
    return(X)
}

##' Simulate from a half-normal distribution
##'
##' Simulates from a univariate half-normal distribution, \eqn{HN(\sigma^2)}. If \eqn{X\sim N(0,\sigma^2)} then \eqn{|X|\sim HN(\sigma^2)}.
##' @param n Number of replications
##' @param sigma Scale parameter
##' @return Vector of simulated values
##' @author Christoffer Moesgaard Albertsen
##' @keywords internal
##' @importFrom stats rnorm
rhalfnorm <- function(n, sigma){
    X <- abs(stats::rnorm(n,0,sigma))
    return(X)
}


##' Simulate from a wrapped Cauchy distribution
##'
##' Simulates values from a wrapped Cauchy distribution, \eqn{WC(\mu,\gamma)}. If \eqn{X \sim t(1)} then
##' \deqn{\gammaX + \mu \text{mod} 2\pi \sim WC(\mu,\gamma}
##' @param n Number of replications
##' @param mu Location parameter
##' @param gamma Scale parameter
##' @return A vector of simulated values
##' @author Christoffer Moesgaard Albertsen
##' @keywords internal
##' @importFrom stats rt
rwcauchy <- function(n, mu, gamma){
    if(!is.numsca(mu))
        stop("mu must be a scalar")
    if(!is.numsca(gamma))
            stop("gamma must be a scalar")
    if(gamma < 0)
        stop("gamma must be positive")
    X <- replicate(n,((stats::rt(1,1) * gamma + mu)) %% (2 * pi))
    return(X)
}



dgig <- function(x, a = 1, b = 1, lambda = 1){
    return( 0.5 * (a / b) ^ (0.5 * lambda) / besselK(sqrt(a*b),lambda) * x ^ (lambda - 1) * exp( - 0.5 * (a * x + b / x) ) * (x>0) )
}

pgig <- Vectorize(function(q, a = 1, b = 1, lambda = 1){
    integrate(dgig,0,q, a = a, b = b, lambda = lambda)$value
})

qgig <- Vectorize(function(p, a = 1, b = 1, lambda = 1){
    uniroot(function(x)pgig(x)-p,interval=c(0,1e4))$root
})

#' @importFrom stats runif
rgig <- function(n, a = 1, b = 1, p = 1){
    U <- stats::runif(n)
    qgig(U)
}


rmvsh <- function(n, mu, sigma, alpha){
    u <- rgig(n, alpha ^ 2, 1, 0.5 * (ncol(sigma)+1))
    X <- sapply(u,function(u0) rmvnorm(1, mu, u0*sigma))
    return(X)
}
