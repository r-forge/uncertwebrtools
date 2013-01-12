setClass(
  Class="InterquartileRange",
  representation=representation(lower="numeric", upper="numeric")
  )

setMethod(
  f="initialize",
  signature="InterquartileRange",
  definition=function(.Object, upperLimit, lowerLimit)
  {
    .Object@lower<-lowerLimit
    .Object@upper<-upperLimit
    return(.Object)    
  }
  )

setGeneric(
  name="getInterquartileRangeLowerLimit",
  def=function(.Object){standardGeneric("getInterquartileRangeLowerLimit")}
  )

setMethod(
  f="getInterquartileRangeLowerLimit",
  signature="InterquartileRange",
  definition=function(.Object)
  {
    return(.Object@lower)    
  }
  )

setGeneric(
  name="getInterquartileRangeUpperLimit",
  def=function(.Object){standardGeneric("getInterquartileRangeUpperLimit")}
  )

setMethod(
  f="getInterquartileRangeUpperLimit",
  signature="InterquartileRange",
  definition=function(.Object)
  {
    return(.Object@upper)    
  }
  )

setGeneric(
  name="getInterquartileRangeLimits",
  def=function(.Object){standardGeneric("getInterquartileRangeLimits")}
  )

setMethod(
  f="getInterquartileRangeLimits",
  signature="InterquartileRange",
  definition=function(.Object)
  {
    return(c(.Object@lower,.Object@upper))    
  }
  )

setGeneric(
  name="setInterquartileRangeLowerLimit<-",
  def=function(.Object, limit){standardGeneric("setInterquartileRangeLowerLimit<-")}
  )

setReplaceMethod(
  f="setInterquartileRangeLowerLimit",
  signature="InterquartileRange",
  definition=function(.Object, limit)
  {
    .Object@lower<-limit    
    return(.Object)
  }
  )

setGeneric(
  name="setInterquartileRangeUpperLimit<-",
  def=function(.Object, limit){standardGeneric("setInterquartileRangeUpperLimit<-")}
  )

setReplaceMethod(
  f="setInterquartileRangeUpperLimit",
  signature="InterquartileRange",
  definition=function(.Object, limit)
  {
    .Object@upper<-limit
    return(.Object)
  }
  )