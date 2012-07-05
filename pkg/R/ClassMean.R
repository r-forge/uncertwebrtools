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
  name="getValues",
  def=function(.Object){standardGeneric("getValues")}
  )

setMethod(
  f="getValues",
  signature="Mean",
  definition=function(.Object)
    {
      return(.Object@values)    
    }
  )

setGeneric(
  name="getCount",
  def=function(.Object){standardGeneric("getCount")}
  )

setMethod(
  f="getCount",
  signature="Mean",
  definition=function(.Object)
    {
      return(length(.Object@values))
    }
  )

setGeneric(
  name="addValues<-"
  def=function(.Object, val){standardGeneric("addValues<-")}
  )

setReplaceMethod(
  f="addValues",
  signature="Mean",
  definition=function(.Object, val)
    {
      .Object@values<-c(values, val)
      return(.Object)   
    }
  )