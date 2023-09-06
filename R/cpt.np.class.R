
.cpt.np.class<-setClass("cpt.np.class",representation(quantiles="numeric"),contains = "cpt.class")

cpt.np.class<-function(data,penalty,results,quantiles,type)
{
    .cpt.np.class(data=data,penalty=penalty,results=results,quantiles=quantiles,type=type)
}
