
nunc <- function(data,beta,nquantiles,wsize)
{
   out <- NUNCoffline(data=data,beta=beta,w=wsize,K=nquantiles,method="local")

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