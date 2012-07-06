setClass(
  Class="CoefficientOfVariation",
  representation=representation(values="numeric")
  )

setMethod(
  f="initialize",
  signature="CoefficientOfVariation",
  definition=function(.Object, val)
  {
    .Object@values<-val
    return(.Object)    
  }
  )

setGeneric(
  name="getCoefficientOfVariationValues",
  def=function(.Object){standardGeneric("getCoefficientOfVariationValues")}
  )

setMethod(
  f="getCoefficientOfVariationValues",
  signature="CoefficientOfVariation",
  definition=function(.Object)
  {
    return(.Object@values)    
  }
  )

setGeneric(
  name="getCoefficientOfVariationValueCount",
  def=function(.Object){standardGeneric("getCoefficientOfVariationValueCount")}
  )

setMethod(
  f="getCoefficientOfVariationValueCount",
  signature="CoefficientOfVariation",
  definition=function(.Object)
  {
    return(length(.Object@values))
  }
  )

setGeneric(
  name="addCoefficientOfVariationValues<-",
  def=function(.Object, val){standardGeneric("addCoefficientOfVariationValues<-")}
  )

setReplaceMethod(
  f="addCoefficientOfVariationValues",
  signature="CoefficientOfVariation",
  definition=function(.Object, val)
  {
    .Object@values<-c(.Object@values, val)
    return(.Object)   
  }
  )