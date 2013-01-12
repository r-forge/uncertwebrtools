setClass(
  Class="CenteredMoment",
  representation=representation(values="numeric", order="integer")
  )

setMethod(
  f="initialize",
  signature="CenteredMoment",
  definition=function(.Object, val, Order)
  {
    .Object@values<-val
    .Object@order<-Order
    return(.Object)    
  }
  )

setGeneric(
  name="getCenteredMomentValues",
  def=function(.Object){standardGeneric("getCenteredMomentValues")}
  )

setMethod(
  f="getCenteredMomentValues",
  signature="CenteredMoment",
  definition=function(.Object)
  {
    return(.Object@values)    
  }
  )

setGeneric(
  name="getCenteredMomentOrder",
  def=function(.Object){standardGeneric("getCenteredMomentOrder")}
  )

setMethod(
  f="getCenteredMomentOrder",
  signature="CenteredMoment",
  definition=function(.Object)
  {
    return(.Object@order)    
  }
  )

setGeneric(
  name="getCenteredMomentValueCount",
  def=function(.Object){standardGeneric("getCenteredMomentValueCount")}
  )

setMethod(
  f="getCenteredMomentValueCount",
  signature="CenteredMoment",
  definition=function(.Object)
  {
    return(length(.Object@values))
  }
  )

setGeneric(
  name="addCenteredMomentValues<-",
  def=function(.Object, val){standardGeneric("addCenteredMomentValues<-")}
  )

setReplaceMethod(
  f="addCenteredMomentValues",
  signature="CenteredMoment",
  definition=function(.Object, val)
  {
    .Object@values<-c(.Object@values, val)
    return(.Object)   
  }
  )

setGeneric(
  name="setCenteredMomentOrder<-",
  def=function(.Object, Order){standardGeneric("setCenteredMomentOrder<-")}
  )

setReplaceMethod(
  f="setCenteredMomentOrder",
  signature="CenteredMoment",
  definition=function(.Object, Order)
  {
    .Object@order<-Order
    return(.Object)
    
  }
  )