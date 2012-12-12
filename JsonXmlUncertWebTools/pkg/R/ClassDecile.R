setClass(
  Class="Decile",
  representation=representation(values="numeric", level="integer")
  )

setMethod(
  f="initialize",
  signature="Decile",
  definition=function(.Object, val, level)
  {
    .Object@values<-val
    .Object@level<-level
    return(.Object)    
  }
  )

setGeneric(
  name="getDecileValues",
  def=function(.Object){standardGeneric("getDecileValues")}
  )

setMethod(
  f="getDecileValues",
  signature="Decile",
  definition=function(.Object)
  {
    return(.Object@values)    
  }
  )

setGeneric(
  name="getDecileLevel",
  def=function(.Object){standardGeneric("getDecileLevel")}
  )

setMethod(
  f="getDecileLevel",
  signature="Decile",
  definition=function(.Object)
  {
    return(.Object@level)    
  }
  )

setGeneric(
  name="getDecileValueCount",
  def=function(.Object){standardGeneric("getDecileValueCount")}
  )

setMethod(
  f="getDecileValueCount",
  signature="Decile",
  definition=function(.Object)
  {
    return(length(.Object@values))
  }
  )

setGeneric(
  name="addDecileValues<-",
  def=function(.Object, val){standardGeneric("addDecileValues<-")}
  )

setReplaceMethod(
  f="addDecileValues",
  signature="Decile",
  definition=function(.Object, val)
  {
    .Object@values<-c(.Object@values, val)
    return(.Object)   
  }
  )

setGeneric(
  name="setDecileLevel<-",
  def=function(.Object, Level){standardGeneric("setDecileLevel<-")}
  )

setReplaceMethod(
  f="setDecileLevel",
  signature="Decile",
  definition=function(.Object, Level)
  {
    .Object@level<-Level
    return(.Object)
    
  }
  )