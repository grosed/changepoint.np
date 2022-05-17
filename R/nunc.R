
#' NUNC
#' 
#' A sliding window non-parametric method for detecting changes in 
#' distribution suitable for online changepoint detection. This method searches for a change in distribution inside the points of 
#' data contained in the sliding window. An approximation for this algorithm can also be specified,
#' that only searches a subset of the points in the sliding window for a change in order to 
#' enhance computational efficiency.
#'  
#' 
#' @param data The vector of observations.
#' @param beta A positive numeric value for the threshold. At a given time, whether the statistics goes over this value, a change is detected. 
#' @param nquantiles The positive integer for the number of quantiles to use.
#' @param wsize A positive integer for the length of the sliding window.
#' @param max_checks An extra parameter to set the maximum amount of splits to check within a window. Defaults to \code{wsize}, the size of the window, equivalent of checking the whole window for a change.
#'  
#' @return Returns an S4 instance of class \code{nunc.class}, containing:
#' \describe{
#' \item{\code{data}}{The vector of observations.}
#' \item{\code{beta}}{The threshold value.}
#' \item{\code{nquantiles}}{The number of quantile values.}
#' \item{\code{detected}}{The iteration at which a change was detected.}
#' \item{\code{location}}{The estimated changepoint location.}
#' }
#' @export
#'
#' @examples 
#' # simulate data containing a change in distribution
#' set.seed(42)
#' y <- c(rnorm(1000, 1, 10), rcauchy(1200, 12, 0.1))
#' 
#' 
#' # running nunc local with a fixed threshold of 12, a window of 100 and 15 quantiles
#' r <- nunc(data = y, beta = 12, wsize = 100, nquantiles = 15)
#' plot(r)
#' 
#' # running nunc local with same parameters and an approximation
#' res <- nunc(data = y, beta = 12, wsize = 100, nquantiles = 15, max_checks = 5)
#' plot(r)
#' 
#' # running nunc local with the theoretically justified threshold for a run length 
#' # up to 2000 observations with an alpha of 0.05
#' r <- nunc(data = y, beta = find_beta(.05, 2000, 100, 15, "local"), wsize = 100, nquantiles = 15, max_checks = 5)
#' plot(r)

nunc <- function(data,beta,nquantiles,wsize,max_checks=wsize)
{
   out <- NUNCoffline(data=data,w=wsize,K=nquantiles,beta=beta,method="local",params = list(max_checks = max_checks))

   location <- numeric(0)
   detected <- numeric(0)
   if(out$changepoint != -1)
   {
     location <- out$changepoint
   }
   if(out$t != length(data))
   {
     detected <- out$t
   }
   
   return(nunc.class(data,beta,nquantiles,wsize,detected,location))
}