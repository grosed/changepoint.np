
if(!isGeneric("changepoints")) {setGeneric("changepoints",function(object) {standardGeneric("changepoints")})}
setGeneric("changepoints",function(object) {standardGeneric("changepoints")})
setMethod("changepoints",signature=list("cpt.class"),
          function(object)
          {
            return(object@results)
          })	
