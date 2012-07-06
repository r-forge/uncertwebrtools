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
  name="getCredibleIntervalLowerLimit",
  def=function(.Object){standardGeneric("getCredibleIntervalLowerLimit")}
  )

setMethod(
  f="getCredibleIntervalLowerLimit",
  signature="CredibleInterval",
  definition=function(.Object)
  {
    return(.Object@lower)    
  }
  )

setGeneric(
  name="getCredibleIntervalUpperLimit",
  def=function(.Object){standardGeneric("getCredibleIntervalUpperLimit")}
  )

setMethod(
  f="getCredibleIntervalUpperLimit",
  signature="CredibleInterval",
  definition=function(.Object)
  {
    return(.Object@upper)    
  }
  )

setGeneric(
  name="getCredibleIntervalLimits",
  def=function(.Object){standardGeneric("getCredibleIntervalLimits")}
  )

setMethod(
  f="getCredibleIntervalLimits",
  signature="CredibleInterval",
  definition=function(.Object)
  {
    return(c(.Object@lower,.Object@upper))    
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