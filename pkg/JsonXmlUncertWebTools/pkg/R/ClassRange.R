setClass(
  Class="Range",
  representation=representation(lower="numeric", upper="numeric")
  )

setMethod(
  f="initialize",
  signature="Range",
  definition=function(.Object, upperLimit, lowerLimit)
  {
    .Object@lower<-lowerLimit
    .Object@upper<-upperLimit
    return(.Object)    
  }
  )

setGeneric(
  name="getRangeLowerLimit",
  def=function(.Object){standardGeneric("getRangeLowerLimit")}
  )

setMethod(
  f="getRangeLowerLimit",
  signature="Range",
  definition=function(.Object)
  {
    return(.Object@lower)    
  }
  )

setGeneric(
  name="getRangeUpperLimit",
  def=function(.Object){standardGeneric("getRangeUpperLimit")}
  )

setMethod(
  f="getRangeUpperLimit",
  signature="Range",
  definition=function(.Object)
  {
    return(.Object@upper)    
  }
  )

setGeneric(
  name="getRangeLimits",
  def=function(.Object){standardGeneric("getRangeLimits")}
  )

setMethod(
  f="getRangeLimits",
  signature="Range",
  definition=function(.Object)
  {
    return(c(.Object@lower,.Object@upper))    
  }
  )

setGeneric(
  name="setRangeLowerLimit<-",
  def=function(.Object, limit){standardGeneric("setRangeLowerLimit<-")}
  )

setReplaceMethod(
  f="setRangeLowerLimit",
  signature="Range",
  definition=function(.Object, limit)
  {
    .Object@lower<-limit    
    return(.Object)
  }
  )

setGeneric(
  name="setRangeUpperLimit<-",
  def=function(.Object, limit){standardGeneric("setRangeUpperLimit<-")}
  )

setReplaceMethod(
  f="setRangeUpperLimit",
  signature="Range",
  definition=function(.Object, limit)
  {
    .Object@upper<-limit
    return(.Object)
  }
  )