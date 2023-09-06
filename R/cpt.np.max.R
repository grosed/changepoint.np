


cpt.np.max <- function(data,quantiles,penalty)
{
    model <- new(np_max)
    model$setcost(data,quantiles)
    model$setpenalty(penalty)
    results <- create_results(model)
    return(cpt.np.class(data,penalty,results,quantiles,"non parametric cost function (maximum method)"))
}



