setClass(
  Class="Skewness",
  representation=representation(values="numeric")
  )

setMethod(
  f="initialize",
  signature="Skewness",
  definition=function(.Object, val)
  {
    .Object@values<-val
    return(.Object)    
  }
  )

setGeneric(
  name="getSkewnessValues",
  def=function(.Object){standardGeneric("getSkewnessValues")}
  )

setMethod(
  f="getSkewnessValues",
  signature="Skewness",
  definition=function(.Object)
  {
    return(.Object@values)    
  }
  )

setGeneric(
  name="getSkewnessValueCount",
  def=function(.Object){standardGeneric("getSkewnessValueCount")}
  )

setMethod(
  f="getSkewnessValueCount",
  signature="Skewness",
  definition=function(.Object)
  {
    return(length(.Object@values))
  }
  )

setGeneric(
  name="addSkewnessValues<-",
  def=function(.Object, val){standardGeneric("addSkewnessValues<-")}
  )

setReplaceMethod(
  f="addSkewnessValues",
  signature="Skewness",
  definition=function(.Object, val)
  {
    .Object@values<-c(.Object@values, val)
    return(.Object)   
  }
  )