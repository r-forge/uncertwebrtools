setClass(
  Class="Variance",
  representation=representation(values="numeric")
  )

setMethod(
  f="initialize",
  signature="Variance",
  definition=function(.Object, val)
  {
    .Object@values<-val
    return(.Object)    
  }
  )

setGeneric(
  name="getVarianceValues",
  def=function(.Object){standardGeneric("getVarianceValues")}
  )

setMethod(
  f="getVarianceValues",
  signature="Variance",
  definition=function(.Object)
  {
    return(.Object@values)    
  }
  )

setGeneric(
  name="getVarianceValueCount",
  def=function(.Object){standardGeneric("getVarianceValueCount")}
  )

setMethod(
  f="getVarianceValueCount",
  signature="Variance",
  definition=function(.Object)
  {
    return(length(.Object@values))
  }
  )

setGeneric(
  name="addVarianceValues<-",
  def=function(.Object, val){standardGeneric("addVarianceValues<-")}
  )

setReplaceMethod(
  f="addVarianceValues",
  signature="Variance",
  definition=function(.Object, val)
  {
    .Object@values<-c(.Object@values, val)
    return(.Object)   
  }
  )