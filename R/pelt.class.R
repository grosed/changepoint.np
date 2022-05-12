.pelt.class<-setClass("pelt.class",representation(data="numeric",beta="numeric",nquantiles="numeric",changepoints="numeric",cost="numeric"))

pelt.class<-function(data,beta,nquantiles,changepoints,cost)
{
    .pelt.class(data=data,beta=beta,nquantiles=nquantiles,changepoints=changepoints,cost=cost)
}



setMethod("plot",signature=list("pelt.class"),function(x)
{
  object <- x
  df <- data.frame("x"=1:length(object@data),"y"=object@data)
  cpts<-object@changepoints
  # appease ggplot2
  y <- NULL
  p <- ggplot(data=df, aes(x=x, y=y))
  p <- p + geom_point(alpha=0.3)
  if(length(cpts) > 1)
  {
   for(cpt in cpts[1:(length(cpts)-1)])
   {
     p <- p + geom_vline(xintercept = cpt,color="red")
   }
  }
  p <- p + theme_bw()
  return(p)
})


if(!isGeneric("changepoints")) {setGeneric("changepoints",function(object) {standardGeneric("changepoints")})}
setGeneric("changepoints",function(object) {standardGeneric("changepoints")})
setMethod("changepoints",signature=list("pelt.class"),
          function(object)
          {
	      return(object@changepoints[-length(object@changepoints)])
          })


setGeneric("cost",function(object) {standardGeneric("cost")})
setMethod("cost",signature=list("pelt.class"),
          function(object)
          {
	     return(object@cost)
          })	      



setMethod("summary",signature=list("pelt.class"),function(object)
{
  cat('\n',"nonparametric pelt analysis with n = ",length(object@data)," and penalty (beta)  = ",object@beta,'\n\n',sep="")
  if(length(object@changepoints) == 1)
  {
    cat("No changepoints detected",'\n\n',sep="")
  }
  else
  {
    msg<-paste(length(object@changepoints)-1," ",sep="")
    if(length(object@changepoints) == 2)
     {
       msg<-paste(msg," changepoint",sep="")
     }
     else
     {
       msg<-paste(msg," changepoints",sep="")
     }
     msg<-paste(msg," detected at x = ",'\n',sep="")
     cat(msg)
     msg<-""
     for(cpt in object@changepoints[1:(length(object@changepoints)-1)])
     {
       msg<-paste(msg,cpt,sep=" ")
     }
     msg<-paste(msg,'\n',sep="")
     cat(msg)
  }
  cat("cost = ",cost(object),'\n\n',sep="")
  invisible()
})


setGeneric("show",function(object) {standardGeneric("show")})
setMethod("show",signature=list("pelt.class"),function(object)
{
    summary(object)
    invisible()
})