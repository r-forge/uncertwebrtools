setClass(
  Class="Moment",
  representation=representation(values="numeric", order="integer")
  )

setMethod(
  f="initialize",
  signature="Moment",
  definition=function(.Object, val, Order)
  {
    .Object@values<-val
    .Object@order<-Order
    return(.Object)    
  }
  )

setGeneric(
  name="getMomentValues",
  def=function(.Object){standardGeneric("getMomentValues")}
  )

setMethod(
  f="getMomentValues",
  signature="Moment",
  definition=function(.Object)
  {
    return(.Object@values)    
  }
  )

setGeneric(
  name="getMomentOrder",
  def=function(.Object){standardGeneric("getMomentOrder")}
  )

setMethod(
  f="getMomentOrder",
  signature="Moment",
  definition=function(.Object)
  {
    return(.Object@order)    
  }
  )

setGeneric(
  name="getMomentValueCount",
  def=function(.Object){standardGeneric("getMomentValueCount")}
  )

setMethod(
  f="getMomentValueCount",
  signature="Moment",
  definition=function(.Object)
  {
    return(length(.Object@values))
  }
  )

setGeneric(
  name="addMomentValues<-",
  def=function(.Object, val){standardGeneric("addMomentValues<-")}
  )

setReplaceMethod(
  f="addMomentValues",
  signature="Moment",
  definition=function(.Object, val)
  {
    .Object@values<-c(.Object@values, val)
    return(.Object)   
  }
  )

setGeneric(
  name="setMomentOrder<-",
  def=function(.Object, Order){standardGeneric("setMomentOrder<-")}
  )

setReplaceMethod(
  f="setMomentOrder",
  signature="Moment",
  definition=function(.Object, Order)
  {
    .Object@order<-Order
    return(.Object)
    
  }
  )