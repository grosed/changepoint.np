
.nunc.class<-setClass("nunc.class",representation(data="numeric",beta="numeric",nquantiles="numeric",wsize="numeric",detected="numeric",location="numeric"))

nunc.class<-function(data,beta,nquantiles,wsize,detected,location)
{
    .nunc.class(data=data,beta=beta,nquantiles=nquantiles,wsize=wsize,detected=detected,location=location)
}


if(!isGeneric("location")) {setGeneric("location",function(object) {standardGeneric("location")})}
setGeneric("location",function(object) {standardGeneric("location")})
setMethod("location",signature=list("nunc.class"),
          function(object)
          {
	      return(object@location)
          })


if(!isGeneric("detected")) {setGeneric("detected",function(object) {standardGeneric("detected")})}
setGeneric("detected",function(object) {standardGeneric("detected")})
setMethod("detected",signature=list("nunc.class"),
          function(object)
          {
	      return(object@detected)
          })


setMethod("plot",signature=list("nunc.class"),function(x)
{
  object <- x
  df <- data.frame("x"=1:length(object@data),"y"=object@data)
  # appease ggplot2
  y <- NULL
  p <- ggplot(data=df, aes(x=x, y=y))
  p <- p + geom_point(alpha=0.3)
  if(length(location(object)) == 1)
  {
    p <- p + geom_vline(xintercept = object@location,color="red")
  }
  if(length(detected(object)) == 1)
  {
    p <- p + geom_vline(xintercept = object@detected,color="blue")
  }
  p <- p + theme_bw()
  return(p)
})



setMethod("summary",signature=list("nunc.class"),function(object)
{
  cat('\n',"offline nunc analysis with n = ",length(object@data)," data points, penalty (beta)  = ",object@beta,", and window size ",object@wsize,'\n\n',sep="")
  if(length(location(object)) != 1)
  {
    cat("No change detected.",'\n\n',sep="")
  }
  else
  {
     msg <- paste("Change located at ",location(object)," detected by ",detected(object),'\n\n',sep="") 
     cat(msg)
  }
  invisible()
})










