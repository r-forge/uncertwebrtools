setClass(
  Class="Quartile",
  representation=representation(values="numeric", level="numeric")
  )

setMethod(
  f="initialize",
  signature="Quartile",
  definition=function(.Object, val, level)
  {
    .Object@values<-val
    .Object@level<-level
    return(.Object)    
  }
  )

setGeneric(
  name="getQuartileValues",
  def=function(.Object){standardGeneric("getQuartileValues")}
  )

setMethod(
  f="getQuartileValues",
  signature="Quartile",
  definition=function(.Object)
  {
    return(.Object@values)    
  }
  )

setGeneric(
  name="getQuartileLevel",
  def=function(.Object){standardGeneric("getQuartileLevel")}
  )

setMethod(
  f="getQuartileLevel",
  signature="Quartile",
  definition=function(.Object)
  {
    return(.Object@level)    
  }
  )

setGeneric(
  name="getQuartileValueCount",
  def=function(.Object){standardGeneric("getQuartileValueCount")}
  )

setMethod(
  f="getQuartileValueCount",
  signature="Quartile",
  definition=function(.Object)
  {
    return(length(.Object@values))
  }
  )

setGeneric(
  name="addQuartileValues<-",
  def=function(.Object, val){standardGeneric("addQuartileValues<-")}
  )

setReplaceMethod(
  f="addQuartileValues",
  signature="Quartile",
  definition=function(.Object, val)
  {
    .Object@values<-c(.Object@values, val)
    return(.Object)   
  }
  )

setGeneric(
  name="setQuartileLevel<-",
  def=function(.Object, Level){standardGeneric("setQuartileLevel<-")}
  )

setReplaceMethod(
  f="setQuartileLevel",
  signature="Quartile",
  definition=function(.Object, Level)
  {
    .Object@level<-Level
    return(.Object)
           
  }
  )