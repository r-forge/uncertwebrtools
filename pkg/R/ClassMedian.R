setClass(
  Class="Median",
  representation=representation(values="numeric")
  )

setMethod(
  f="initialize",
  signature="Median",
  definition=function(.Object, val)
  {
    .Object@values<-val
    return(.Object)    
  }
  )

setGeneric(
  name="getMedianValues",
  def=function(.Object){standardGeneric("getMedianValues")}
  )

setMethod(
  f="getMedianValues",
  signature="Median",
  definition=function(.Object)
  {
    return(.Object@values)    
  }
  )

setGeneric(
  name="getMedianValueCount",
  def=function(.Object){standardGeneric("getMedianValueCount")}
  )

setMethod(
  f="getMedianValueCount",
  signature="Median",
  definition=function(.Object)
  {
    return(length(.Object@values))
  }
  )

setGeneric(
  name="addMedianValues<-",
  def=function(.Object, val){standardGeneric("addMedianValues<-")}
  )

setReplaceMethod(
  f="addMedianValues",
  signature="Median",
  definition=function(.Object, val)
  {
    .Object@values<-c(.Object@values, val)
    return(.Object)   
  }
  )