setClass(
  Class="Quantile",
  representation=representation(values="numeric", level="numeric")
  )

setMethod(
  f="initialize",
  signature="Quantile",
  definition=function(.Object, val, level)
  {
    .Object@values<-val
    .Object@level<-level
    return(.Object)    
  }
  )

setGeneric(
  name="getQuantileValues",
  def=function(.Object){standardGeneric("getQuantileValues")}
  )

setMethod(
  f="getQuantileValues",
  signature="Quantile",
  definition=function(.Object)
  {
    return(.Object@values)    
  }
  )

setGeneric(
  name="getQuantileLevel",
  def=function(.Object){standardGeneric("getQuantileLevel")}
  )

setMethod(
  f="getQuantileLevel",
  signature="Quantile",
  definition=function(.Object)
  {
    return(.Object@level)    
  }
  )

setGeneric(
  name="getQuantileValueCount",
  def=function(.Object){standardGeneric("getQuantileValueCount")}
  )

setMethod(
  f="getQuantileValueCount",
  signature="Quantile",
  definition=function(.Object)
  {
    return(length(.Object@values))
  }
  )

setGeneric(
  name="addQuantileValues<-",
  def=function(.Object, val){standardGeneric("addQuantileValues<-")}
  )

setReplaceMethod(
  f="addQuantileValues",
  signature="Quantile",
  definition=function(.Object, val)
  {
    .Object@values<-c(.Object@values, val)
    return(.Object)   
  }
  )

setGeneric(
  name="setQuantileLevel<-",
  def=function(.Object, Level){standardGeneric("setQuantileLevel<-")}
  )

setReplaceMethod(
  f="setQuantileLevel",
  signature="Quantile",
  definition=function(.Object, Level)
  {
    .Object@level<-Level
    return(.Object)
    
  }
  )