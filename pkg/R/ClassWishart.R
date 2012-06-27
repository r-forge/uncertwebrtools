setClass(
  Class="WishartDistribution",
  representation=representation(degreesOfFreedom="numeric", scaleMatrix="matrix" )
  )

setMethod(
  f="initialize",
  signature="WishartDistribution",
  definition=function(.Object, degrees, matrix){
    .Object@degreesOfFreedom<-degrees
    .Object@scaleMatrix<-matrix
    #validObject(.Object)
    return(.Object)
  }
  )

setGeneric(
  name="getWishartDegreesOfFreedom",
  def=function(.Object) {standardGeneric("getDegreesOfFreedom")}
  )

setMethod(
  f="getWishartDegreesOfFreedom",
  signature="WishartDistribution",
  definition=function(.Object){
    return(.Object@degreesOfFreedom)
  }
  )

setGeneric(
  name="getWishartScaleMatrix",
  def=function(.Object) {standardGeneric("getScaleMatrix")}
  )

setMethod(
  f="getWishartScaleMatrix",
  signature="WishartDistribution",
  definition=function(.Object){
    return(.Object@scaleMatrix)
  }
  )

setGeneric(
  name="setWishartDegreesOfFreedom<-",
  def=function(.Object,Value){standardGeneric("setDegreesOfFreedom<-")}
  )

setReplaceMethod(
  f="setWishartDegreesOfFreedom",
  signature="WishartDistribution",
  definition=function(.Object,Value){
    .Object@degreesOfFreedom<-Value
    return(.Object)
  }
  )

setGeneric(
  name="setWishartScaleMatrix<-",
  def=function(.Object,Value){standardGeneric("setScaleMatrix<-")}
  )

setReplaceMethod(
  f="setWishartScaleMatrix",
  signature="WishartDistribution",
  definition=function(.Object,Value){
    .Object@scaleMatrix<-Value
    return(.Object)
  }
  )

setGeneric(
  name="getWishartSamples",
  def=function(.Object,number) {standardGeneric("getSamples")}
  )

setMethod(
  f="getWishartSamples",
  signature="WishartDistribution",
  definition=function(.Object, number){
    sample<-rWishart(number, .Object@degreesOfFreedom, .Object@scaleMatrix)
    temp<-c()
    for(i in 1:length(sample)){
      temp<-c(temp, new(Class="Realisation", Value=as.double(sample[i]), Id=i, Weight=1/number))
    }
    rsample<-new(Class="RandomSample", temp)
    return(rsample)
  }
  )
