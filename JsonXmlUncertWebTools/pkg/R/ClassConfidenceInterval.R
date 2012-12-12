setClass(
  Class="ConfidenceInterval",
  representation=representation(lower="Quantile", upper="Quantile")
  )

setMethod(
  f="initialize",
  signature="ConfidenceInterval",
  definition=function(.Object, upperLevel, upperValues, lowerValues, lowerLevel)
  {
    Upper<-new("Quantile", upperValues, upperLevel)
    Lower<-new("Quantile", lowerValues, lowerLevel)
    .Object@lower<-Lower
    .Object@upper<-Upper
    return(.Object)    
  }
  )

setGeneric(
  name="getConfidenceIntervalLowerLimitAsQuantile",
  def=function(.Object){standardGeneric("getConfidenceIntervalLowerLimitAsQuantile")}
  )

setMethod(
  f="getConfidenceIntervalLowerLimitAsQuantile",
  signature="ConfidenceInterval",
  definition=function(.Object)
  {
    return(.Object@lower)    
  }
  )

setGeneric(
  name="getConfidenceIntervalUpperLimitAsQuantile",
  def=function(.Object){standardGeneric("getConfidenceIntervalUpperLimitAsQuantile")}
  )

setMethod(
  f="getConfidenceIntervalUpperLimitAsQuantile",
  signature="ConfidenceInterval",
  definition=function(.Object)
  {
    return(.Object@upper)    
  }
  )

setGeneric(
  name="getConfidenceIntervalLimitsAsQuantiles",
  def=function(.Object){standardGeneric("getConfidenceIntervalLimitsAsQuantiles")}
  )

setMethod(
  f="getConfidenceIntervalLimitsAsQuantiles",
  signature="ConfidenceInterval",
  definition=function(.Object)
  {
    return(list(lower=.Object@lower,upper=.Object@upper))    
  }
  )

setGeneric(
  name="getConfidenceIntervalLowerLimitAsPrimitives",
  def=function(.Object){standardGeneric("getConfidenceIntervalLowerLimitAsPrimitives")}
  )

setMethod(
  f="getConfidenceIntervalLowerLimitAsPrimitives",
  signature="ConfidenceInterval",
  definition=function(.Object)
  {
    lowerQuantile<-.Object@lower
    listToReturn<-list(level=lowerQuantile@level, values=lowerQuantile@values)
    return(listToReturn)    
  }
  )

setGeneric(
  name="getConfidenceIntervalUpperLimitAsPrimitives",
  def=function(.Object){standardGeneric("getConfidenceIntervalUpperLimitAsPrimitives")}
  )

setMethod(
  f="getConfidenceIntervalUpperLimitAsPrimitives",
  signature="ConfidenceInterval",
  definition=function(.Object)
  {
    upperQuantile<-.Object@upper
    listToReturn<-list(level=upperQuantile@level, values=upperQuantile@values)
    return(listToReturn)
  }
  )

setGeneric(
  name="getConfidenceIntervalLimitsAsPrimitives",
  def=function(.Object){standardGeneric("getConfidenceIntervalLimitsAsPrimitives")}
  )

setMethod(
  f="getConfidenceIntervalLimitsAsPrimitives",
  signature="ConfidenceInterval",
  definition=function(.Object)
  {
    lowerQuantile<-.Object@lower
    l<-list(level=lowerQuantile@level, values=lowerQuantile@values)
    upperQuantile<-.Object@upper
    u<-list(level=upperQuantile@level, values=upperQuantile@values)
    listToReturn<-list(lower=l,upper=u)
    return(listToReturn)
  }
  )

setGeneric(
  name="setConfidenceIntervalLowerLimit<-",
  def=function(.Object, Level, Values){standardGeneric("setConfidenceIntervalLowerLimit<-")}
  )

setReplaceMethod(
  f="setConfidenceIntervalLowerLimit",
  signature="ConfidenceInterval",
  definition=function(.Object, Level, Values)
  {
    limit<-new("Quantile", Values, Level)
    .Object@lower<-limit    
    return(.Object)
  }
  )

setGeneric(
  name="setConfidenceIntervalUpperLimit<-",
  def=function(.Object, Level, Values){standardGeneric("setConfidenceIntervalUpperLimit<-")}
  )

setReplaceMethod(
  f="setConfidenceIntervalUpperLimit",
  signature="ConfidenceInterval",
  definition=function(.Object, Level, Values)
  {
    limit<-new("Quantile", Values, Level)
    .Object@upper<-limit
    return(.Object)
  }
  )