setClass(
  Class="Correlation",
  representation=representation(values="numeric")
  )

setMethod(
  f="initialize",
  signature="Correlation",
  definition=function(.Object, val)
  {
    .Object@values<-val
    return(.Object)    
  }
  )

setGeneric(
  name="getCorrelationValues",
  def=function(.Object){standardGeneric("getCorrelationValues")}
  )

setMethod(
  f="getCorrelationValues",
  signature="Correlation",
  definition=function(.Object)
  {
    return(.Object@values)    
  }
  )

setGeneric(
  name="getCorrelationValueCount",
  def=function(.Object){standardGeneric("getCorrelationValueCount")}
  )

setMethod(
  f="getCorrelationValueCount",
  signature="Correlation",
  definition=function(.Object)
  {
    return(length(.Object@values))
  }
  )

setGeneric(
  name="addCorrelationValues<-",
  def=function(.Object, val){standardGeneric("addCorrelationValues<-")}
  )

setReplaceMethod(
  f="addCorrelationValues",
  signature="Correlation",
  definition=function(.Object, val)
  {
    .Object@values<-c(.Object@values, val)
    return(.Object)   
  }
  )