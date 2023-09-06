
cpt.normal.mean <- function(data,penalty)
{
    model <- new(normal_mean)
    model$setcost(data)
    model$setpenalty(penalty)
    results <- create_results(model)
    return(cpt.class(data,penalty,results,"normal cost function for change in mean"))
}
