
pelt.crops <- function(data,nquantiles,beta.min,beta.max,max_iterations)
{
   objective.function <- function(beta)
   {
      out <- changepoint.np.new::pelt(data,beta,nquantiles)
      return(list(cost(out)-beta*length(changepoints(out)),changepoints(out)))
   }

  return(crops(objective.function,beta.min,beta.max,max_iterations))
}