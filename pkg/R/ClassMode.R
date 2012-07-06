setClass(
  Class="Mode",
  representation=representation(values="numeric", categories="character")
  )

setMethod(
  f="initialize",
  signature="Mode",
  definition=function(.Object, Value, Category)
  {
    .Object@values<-Value
    .Object@categories<-Category
    return(.Object)    
  }
  )

setGeneric(
  name="getModeValues",
  def=function(.Object){standardGeneric("getModeValues")}
  )

setMethod(
  f="getModeValues",
  signature="Mode",
  definition=function(.Object)
  {
    return(.Object@values)    
  }
  )

setGeneric(
  name="getModeCategories",
  def=function(.Object){standardGeneric("getModeCategories")}
  )

setMethod(
  f="getModeCategories",
  signature="Mode",
  definition=function(.Object)
  {
    return(.Object@categories)    
  }
  )


setGeneric(
  name="getModeValueCount",
  def=function(.Object){standardGeneric("getModeValueCount")}
  )

setMethod(
  f="getModeValueCount",
  signature="Mode",
  definition=function(.Object)
  {
    if (length(.Object@categories)==0)
    {
      return(length(.Object@values))
    }
    else
      return(length(.Object@categories))
  }
  )

setGeneric(
  name="addModeValues<-",
  def=function(.Object, val){standardGeneric("addModeValues<-")}
  )

setReplaceMethod(
  f="addModeValues",
  signature="Mode",
  definition=function(.Object, val)
  {
    .Object@values<-c(values, val)
    return(.Object)   
  }
  )

setGeneric(
  name="addModeCategories<-",
  def=function(.Object, Category){standardGeneric("addModeCategories<-")}
  )

setReplaceMethod(
  f="addModeCategories",
  signature="Mode",
  definition=function(.Object, Category)
  {
    .Object@categories<-c(.Object@categories, Category)
    return(.Object)   
  }
  )