setClass(
  Class="Kurtosis",
  representation=representation(values="numeric")
  )

setMethod(
  f="initialize",
  signature="Kurtosis",
  definition=function(.Object, val)
  {
    .Object@values<-val
    return(.Object)    
  }
  )

setGeneric(
  name="getKurtosisValues",
  def=function(.Object){standardGeneric("getKurtosisValues")}
  )

setMethod(
  f="getKurtosisValues",
  signature="Kurtosis",
  definition=function(.Object)
  {
    return(.Object@values)    
  }
  )

setGeneric(
  name="getKurtosisValuesCount",
  def=function(.Object){standardGeneric("getKurtosisValuesCount")}
  )

setMethod(
  f="getKurtosisValuesCount",
  signature="Kurtosis",
  definition=function(.Object)
  {
    return(length(.Object@values))
  }
  )

setGeneric(
  name="addKurtosisValues<-",
  def=function(.Object, val){standardGeneric("addKurtosisValues<-")}
  )

setReplaceMethod(
  f="addKurtosisValues",
  signature="Kurtosis",
  definition=function(.Object, val)
  {
    .Object@values<-c(.Object@values, val)
    return(.Object)   
  }
  )