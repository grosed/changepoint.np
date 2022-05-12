
# default : nquantiles = 10
# default : minseglen = 1


pelt <- function(data,beta,nquantiles)
{
     nonparametric.ed.sumstat <- function(data,K=nquantiles)
     			      {
      			      ##USE K points in integral
      			      n <- length(data)
      			      if(K>n) K=n
      			      Q <- matrix(0,K,n+1)
      			      x=sort(data)
      			      yK= -1 + (2*(1:K)/K-1/K)
      			      c=-log(2*n-1)
      			      pK=(1+exp(c*yK))^-1
      			      for (i in 1:K)
			      {
        		        j=as.integer((n-1)*pK[i] + 1)
        			Q[i,-1] <- cumsum(data<x[j])+0.5*cumsum(data==x[j])
      			      }
      			      return(Q)
    			      }
    sumstat <- nonparametric.ed.sumstat(data, K = nquantiles)
    sumstat <- as.vector(sumstat)
    out <- rcpp_pelt(sumstat,beta,nquantiles)
    out[[1]] <- sort(out[[1]][out[[1]] > 0])
    return(pelt.class(data=data,beta=beta,nquantiles=nquantiles,changepoints=out[[1]],out[[2]]))
}






