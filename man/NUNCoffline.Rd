% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NUNCoffline.R
\name{NUNCoffline}
\alias{NUNCoffline}
\title{NUNC Offline}
\usage{
NUNCoffline(
  data,
  w,
  beta = find_beta(0.05, length(data), w, K, method),
  K = 15,
  method = c("global", "local", "semi-param"),
  params = list()
)
}
\arguments{
\item{data}{The vector of observations.}

\item{w}{A positive integer for the size of the sliding window.}

\item{beta}{A positive numeric for the thresholds}

\item{K}{The positive integer for the number of quantiles to use.}

\item{method}{The NUNC method: one of "local", "global", or "semi-parametric".}

\item{params}{Additional arguments passed to NUNC - see details.}
}
\value{
Returns an object of class \code{NUNCout}, which is a \code{\link[=list]{list}}
of items containing
\describe{
\item{\code{$t}}{Time change is detected - indicated by the right edge of the window.}
\item{\code{$changepoint}}{Location of the changepoint in the data stream.}
\item{\code{$method}}{Method that has been specified by the user.} 
\item{\code{$zVals}}{If the method is set to \code{"global"} then this returns the
value of the empirical CDF for the historic data at the K quantiles.}
\item{\code{$data}}{The data inputted into the function.}
}
}
\description{
An offline sliding window non-parametric method for detecting changes in 
distribution.
}
\details{
NUNC offline applies the NUNC algorithm to a pre-observed stream of data and searches
this data for changes in distribution using a sliding window. An online version of the NUNC 
algorithm is provided in \link{NUNC}

Three different variants of NUNC exist: "NUNC Local", "NUNC Global", and "NUNC Semi-Parametric".
Each of these three variants tests for changes in distribution through use of a cost function
that makes a comparison between the pre and post change empirical CDFs for the data. 
This comparison is aggregated over K quantiles, as this enhances the power of the test by 
comparing both the centre, and tails, of the estimated distributions.

The three different methods can be described as follows:
\describe{
\item{NUNC Local}{This method searches for a change in distribution inside the points of 
data contained in the sliding window. An approximation for this algorithm can also be specified,
that only searches a subset of the points in the sliding window for a change in order to 
enhance computational efficiency.}
\item{NUNC Global}{This method tests if the data in the sliding window is drawn from a 
different distribution to the historic data.}
\item{NUNC Semi-Parametric}{This method is identical to NUNC Local, except the quantiles are
pre-specified rather than being estimated from the windowed data.}
}

The different methods require different arguments. These arguments can be specified
through the \code{params} argument:
\describe{
\item{if \code{method = "local"}}{An extra parameter to set the maximum amount of comparisons 
 to check can be specified via extra parameter \code{max_checks}. If not specified this 
 defaults to \code{w}, the size of the window.}
\item{if \code{method = "semi-param"}}{then parameter \code{quantiles} is required to perform 
 the semi-parametric estimation. This fixes the K quantiles the CDFs are compared at, rather 
 than estimating them from the data.}
}
}
\examples{

#simulate data containing a change in distribution
set.seed(42)
data <- c(rnorm(250, 1, 10), rcauchy(1200, 12, 0.1))
plot(data)

#examples with various methods:

#the "global" approach that stores information from the past (default)
system.time(cp <- NUNCoffline(data = data, w = 50, beta = 10))
abline(v = cp$changepoint, col = 2)

# the "local" version that checks for changes inside the sliding window
NUNCoffline(data = data, w = 50, beta = 10, method = "local")

# the approx "local" version (faster for the same detection power)
NUNCoffline(data = data, w = 50, beta = 10, params = list(max_checks = 3), method = "local")

# the "semi parametric" version with pre-specified quantiles (trained on a normal distribution)
NUNCoffline(data, w = 50, beta = 10, params = list(quantiles = quantile(rnorm(1e3))),
            method = "semi-param")
}
