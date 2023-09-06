quantiles <- function(X,k)
{
  # derived from code provided by Hyeyoung Maeng
  k <- k - 2
  n <- length(X)
  Q <- matrix(0, k, n+1)
  sorted.X = sort(X)
  yk = -1 + (2*(1:k)/k-1/k)
  c = -log(2*n-1)
  pk  = (1+exp(c*yk))^-1
  qnt = rep(NA, k)
  for (i in 1:k){
    j  = as.integer((n-1)*pk[i] + 1)
    qnt[i] = sorted.X[j]
  }
  return(c(-Inf,qnt,Inf))
}
