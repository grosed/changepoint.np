setMethod("plot",signature=list("cpt.class"),function(x)
{
    p <- ggplot(data.frame("x" = 1:length(x@data),"value" = x@data),
                  aes(x=.data$x,y=.data$value))
    p <- p + geom_point(alpha=0.3)
    p <- p + geom_vline(xintercept=head(changepoints(x)$end,-1),
                        linetype="dashed",
                        color = "red")
    return(p)
})
