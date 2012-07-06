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
  name="getConfidenceIntervalLowerLimit",
  def=function(.Object){standardGeneric("getConfidenceIntervalLowerLimit")}
  )

setMethod(
  f="getConfidenceIntervalLowerLimit",
  signature="ConfidenceInterval",
  definition=function(.Object)
  {
    return(.Object@lower)    
  }
  )

setGeneric(
  name="getConfidenceIntervalUpperLimit",
  def=function(.Object){standardGeneric("getConfidenceIntervalUpperLimit")}
  )

setMethod(
  f="getConfidenceIntervalUpperLimit",
  signature="ConfidenceInterval",
  definition=function(.Object)
  {
    return(.Object@upper)    
  }
  )

setGeneric(
  name="getConfidenceIntervalLimits",
  def=function(.Object){standardGeneric("getConfidenceIntervalLimits")}
  )

setMethod(
  f="getConfidenceIntervalLimits",
  signature="ConfidenceInterval",
  definition=function(.Object)
  {
    return(c(.Object@lower,.Object@upper))    
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