
.cpt.class<-setClass("cpt.class",representation(data="numeric",penalty="numeric",results="data.frame",type="character"))

cpt.class<-function(data,penalty,results,type)
{
    .cpt.class(data=data,penalty=penalty,results=results,type=type)
}




