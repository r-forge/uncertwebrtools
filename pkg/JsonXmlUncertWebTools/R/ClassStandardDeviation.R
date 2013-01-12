setClass(
  Class="StandardDeviation",
  representation=representation(values="numeric")
  )

setMethod(
  f="initialize",
  signature="StandardDeviation",
  definition=function(.Object, val)
  {
    .Object@values<-val
    return(.Object)    
  }
  )

setGeneric(
  name="getStandardDeviationValues",
  def=function(.Object){standardGeneric("getStandardDeviationValues")}
  )

setMethod(
  f="getStandardDeviationValues",
  signature="StandardDeviation",
  definition=function(.Object)
  {
    return(.Object@values)    
  }
  )

setGeneric(
  name="getStandardDeviationValueCount",
  def=function(.Object){standardGeneric("getStandardDeviationValueCount")}
  )

setMethod(
  f="getStandardDeviationValueCount",
  signature="StandardDeviation",
  definition=function(.Object)
  {
    return(length(.Object@values))
  }
  )

setGeneric(
  name="addStandardDeviationValues<-",
  def=function(.Object, val){standardGeneric("addStandardDeviationValues<-")}
  )

setReplaceMethod(
  f="addStandardDeviationValues",
  signature="StandardDeviation",
  definition=function(.Object, val)
  {
    .Object@values<-c(.Object@values, val)
    return(.Object)   
  }
  )