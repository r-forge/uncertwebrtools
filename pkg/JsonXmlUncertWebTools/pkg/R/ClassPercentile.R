setClass(
  Class="Percentile",
  representation=representation(values="numeric", level="integer")
  )

setMethod(
  f="initialize",
  signature="Percentile",
  definition=function(.Object, val, level)
  {
    .Object@values<-val
    .Object@level<-level
    return(.Object)    
  }
  )

setGeneric(
  name="getPercentileValues",
  def=function(.Object){standardGeneric("getPercentileValues")}
  )

setMethod(
  f="getPercentileValues",
  signature="Percentile",
  definition=function(.Object)
  {
    return(.Object@values)    
  }
  )

setGeneric(
  name="getPercentileLevel",
  def=function(.Object){standardGeneric("getPercentileLevel")}
  )

setMethod(
  f="getPercentileLevel",
  signature="Percentile",
  definition=function(.Object)
  {
    return(.Object@level)    
  }
  )

setGeneric(
  name="getPercentileValueCount",
  def=function(.Object){standardGeneric("getPercentileValueCount")}
  )

setMethod(
  f="getPercentileValueCount",
  signature="Percentile",
  definition=function(.Object)
  {
    return(length(.Object@values))
  }
  )

setGeneric(
  name="addPercentileValues<-",
  def=function(.Object, val){standardGeneric("addPercentileValues<-")}
  )

setReplaceMethod(
  f="addPercentileValues",
  signature="Percentile",
  definition=function(.Object, val)
  {
    .Object@values<-c(.Object@values, val)
    return(.Object)   
  }
  )

setGeneric(
  name="setPercentileLevel<-",
  def=function(.Object, Level){standardGeneric("setPercentileLevel<-")}
  )

setReplaceMethod(
  f="setPercentileLevel",
  signature="Percentile",
  definition=function(.Object, Level)
  {
    .Object@level<-Level
    return(.Object)
    
  }
  )