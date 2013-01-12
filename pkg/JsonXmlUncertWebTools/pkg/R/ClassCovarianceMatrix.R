setClass(
  Class="CovarianceMatrix",
  representation=representation(values="numeric", dimension="integer")
  )

setMethod(
  f="initialize",
  signature="CovarianceMatrix",
  definition=function(.Object, Value, Dimension)
  {
    .Object@values<-Value
    .Object@dimension<-Dimension
    return(.Object)    
  }
  )

setGeneric(
  name="getCovarianceMatrixValues",
  def=function(.Object){standardGeneric("getCovarianceMatrixValues")}
  )

setMethod(
  f="getCovarianceMatrixValues",
  signature="CovarianceMatrix",
  definition=function(.Object)
  {
    return(.Object@values)    
  }
  )

setGeneric(
  name="getCovarianceMatrixDimension",
  def=function(.Object){standardGeneric("getCovarianceMatrixDimension")}
  )

setMethod(
  f="getCovarianceMatrixDimension",
  signature="CovarianceMatrix",
  definition=function(.Object)
  {
    return(.Object@dimension)    
  }
  )

setGeneric(
  name="getCovarianceMatrixValueCount",
  def=function(.Object){standardGeneric("getCovarianceMatrixValueCount")}
  )

setMethod(
  f="getCovarianceMatrixValueCount",
  signature="CovarianceMatrix",
  definition=function(.Object)
  {
    return(length(.Object@values))
  }
  )

setGeneric(
  name="addCovarianceMatrixValues<-",
  def=function(.Object, val){standardGeneric("addCovarianceMatrixValues<-")}
  )

setReplaceMethod(
  f="addCovarianceMatrixValues",
  signature="CovarianceMatrix",
  definition=function(.Object, val)
  {
    .Object@values<-c(.Object@values, val)
    return(.Object)   
  }
  )

setGeneric(
  name="setCovarianceMatrixDimension<-",
  def=function(.Object, Dimension){standardGeneric("setCovarianceMatrixDimension<-")}
  )

setReplaceMethod(
  f="setCovarianceMatrixDimension",
  signature="CovarianceMatrix",
  definition=function(.Object, Dimension)
  {
    .Object@dimension<-Dimension
    return(.Object)
    
  }
  )