
create_results <- function(model)
{
    changepoints <- model$changepoints()
    res <- c()
    idx <- length(changepoints)
    while(idx - changepoints[idx] > 0)
    {
        res <- c(res,idx - changepoints[idx])
        idx <- idx - changepoints[idx]
    }
    res <- c(0,rev(res),length(changepoints))
    costs <- c()
    for(idx in 1:(length(res)-1))
    {
        costs <- c(costs,model$cost(res[idx]+1,res[idx+1]))
    }
    
    df <- data.frame("start"=res[1:(length(res)-1)]+1,"end"=res[2:length(res)],"cost"=costs)
    return(df)
}
