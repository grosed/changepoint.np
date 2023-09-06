
setMethod("summary",signature=list("cpt.class"),function(object)
{
    cat("changepoint detection using ",object@type,"\n",sep="")
    cat("penalty = ",object@penalty,"\n",sep="")
    cat("changepoint locations are","\n",sep="")
    print(changepoints(object))
})


setMethod("summary",signature=list("cpt.np.class"),function(object)
{
    callNextMethod(object)
    cat("using ",length(object@quantiles)," quantiles located at ","\n",sep="")
    print(object@quantiles)
})
