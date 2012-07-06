setClass(
  Class="Mean",
  representation=representation(values="numeric")
  )

setMethod(
  f="initialize",
  signature="Mean",
  definition=function(.Object, val)
    {
      .Object@values<-val
      return(.Object)    
    }
  )

setGeneric(
  name="getMeanValues",
  def=function(.Object){standardGeneric("getMeanValues")}
  )

setMethod(
  f="getMeanValues",
  signature="Mean",
  definition=function(.Object)
    {
      return(.Object@values)    
    }
  )

setGeneric(
  name="getMeanValueCount",
  def=function(.Object){standardGeneric("getMeanValueCount")}
  )

setMethod(
  f="getMeanValueCount",
  signature="Mean",
  definition=function(.Object)
    {
      return(length(.Object@values))
    }
  )

setGeneric(
  name="addMeanValues<-",
  def=function(.Object, val){standardGeneric("addMeanValues<-")}
  )

setReplaceMethod(
  f="addMeanValues",
  signature="Mean",
  definition=function(.Object, val)
    {
      .Object@values<-c(.Object@values, val)
      return(.Object)   
    }
  )