setClass(
  Class="CredibleInterval",
  representation=representation(lower="Quantile", upper="Quantile")
  )

setMethod(
  f="initialize",
  signature="CredibleInterval",
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
  name="getCredibleIntervalLowerLimitAsQuantile",
  def=function(.Object){standardGeneric("getCredibleIntervalLowerLimitAsQuantile")}
  )

setMethod(
  f="getCredibleIntervalLowerLimitAsQuantile",
  signature="CredibleInterval",
  definition=function(.Object)
  {
    return(.Object@lower)    
  }
  )

setGeneric(
  name="getCredibleIntervalUpperLimitAsQuantile",
  def=function(.Object){standardGeneric("getCredibleIntervalUpperLimitAsQuantile")}
  )

setMethod(
  f="getCredibleIntervalUpperLimitAsQuantile",
  signature="CredibleInterval",
  definition=function(.Object)
  {
    return(.Object@upper)    
  }
  )

setGeneric(
  name="getCredibleIntervalLimitsAsQuantiles",
  def=function(.Object){standardGeneric("getCredibleIntervalLimitsAsQuantiles")}
  )

setMethod(
  f="getCredibleIntervalLimitsAsQuantiles",
  signature="CredibleInterval",
  definition=function(.Object)
  {
    return(list(lower=.Object@lower,upper=.Object@upper))    
  }
  )

setGeneric(
  name="getCredibleIntervalLowerLimitAsPrimitives",
  def=function(.Object){standardGeneric("getCredibleIntervalLowerLimitAsPrimitives")}
  )

setMethod(
  f="getCredibleIntervalLowerLimitAsPrimitives",
  signature="CredibleInterval",
  definition=function(.Object)
  {
    lowerQuantile<-.Object@lower
    listToReturn<-list(level=lowerQuantile@level, values=lowerQuantile@values)
    return(listToReturn)    
  }
  )

setGeneric(
  name="getCredibleIntervalUpperLimitAsPrimitives",
  def=function(.Object){standardGeneric("getCredibleIntervalUpperLimitAsPrimitives")}
  )

setMethod(
  f="getCredibleIntervalUpperLimitAsPrimitives",
  signature="CredibleInterval",
  definition=function(.Object)
  {
    upperQuantile<-.Object@upper
    listToReturn<-list(level=upperQuantile@level, values=upperQuantile@values)
    return(listToReturn)
  }
  )

setGeneric(
  name="getCredibleIntervalLimitsAsPrimitives",
  def=function(.Object){standardGeneric("getCredibleIntervalLimitsAsPrimitives")}
  )

setMethod(
  f="getCredibleIntervalLimitsAsPrimitives",
  signature="CredibleInterval",
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
  name="setCredibleIntervalLowerLimit<-",
  def=function(.Object, Level, Values){standardGeneric("setCredibleIntervalLowerLimit<-")}
  )

setReplaceMethod(
  f="setCredibleIntervalLowerLimit",
  signature="CredibleInterval",
  definition=function(.Object, Level, Values)
  {
    limit<-new("Quantile", Values, Level)
    .Object@lower<-limit    
    return(.Object)
  }
  )

setGeneric(
  name="setCredibleIntervalUpperLimit<-",
  def=function(.Object, Level, Values){standardGeneric("setCredibleIntervalUpperLimit<-")}
  )

setReplaceMethod(
  f="setCredibleIntervalUpperLimit",
  signature="CredibleInterval",
  definition=function(.Object, Level, Values)
  {
    limit<-new("Quantile", Values, Level)
    .Object@upper<-limit
    return(.Object)
  }
  )