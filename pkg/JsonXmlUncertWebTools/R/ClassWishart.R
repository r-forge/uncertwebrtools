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
    return(.Object)
  }
  )

setGeneric(
  name="getWishartDegreesOfFreedom",
  def=function(.Object) {standardGeneric("getWishartDegreesOfFreedom")}
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
  def=function(.Object) {standardGeneric("getWishartScaleMatrix")}
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
  def=function(.Object,value){standardGeneric("setWishartDegreesOfFreedom<-")}
  )

setReplaceMethod(
  f="setWishartDegreesOfFreedom",
  signature="WishartDistribution",
  definition=function(.Object,value){
    .Object@degreesOfFreedom<-value
    return(.Object)
  }
  )

setGeneric(
  name="setWishartScaleMatrix<-",
  def=function(.Object,value){standardGeneric("setWishartScaleMatrix<-")}
  )

setReplaceMethod(
  f="setWishartScaleMatrix",
  signature="WishartDistribution",
  definition=function(.Object,value){
    .Object@scaleMatrix<-value
    return(.Object)
  }
  )

setGeneric(
  name="getWishartSamples",
  def=function(.Object,number) {standardGeneric("getWishartSamples")}
  )

setMethod(
  f="getWishartSamples",
  signature="WishartDistribution",
  definition=function(.Object, number){
    temp<-rWishart(number, .Object@degreesOfFreedom, .Object@scaleMatrix)
    p<-sqrt(length(.Object@scaleMatrix))
    sample<-list()
    for(k in 1:number){
      t<-list()
      for(i in 1:p)
      {
        r<-list()
        for(j in 1:p)
        {
          r[[j]]<-new(Class="Realisation", Value=as.double(temp[i,j,k]), Id=i, Weight=1/number)
        }
        t[[i]]<-r
      }
      sample[[k]]<-t
    }
    rsample<-new(Class="RandomSample", sample)
    return(rsample)
  }
  )

setGeneric(
  name="getWishartMean",
  def=function(.Object){standardGeneric("getWishartMean")}
  )

setMethod(
  f="getWishartMean",
  signature="WishartDistribution",
  definition=function(.Object)
  {
    return(.Object@scaleMatrix * .Object@degreesOfFreedom)  
  }
  )

setGeneric(
  name="getWishartVariance",
  def=function(.Object){standardGeneric("getWishartVariance")}
  )

setMethod(
  f="getWishartVariance",
  signature="WishartDistribution",
  definition=function(.Object)
  {
    p<-sqrt(length(.Object@scaleMatrix))
    var<-diag(0,nrow=p, ncol=p)
    for(i in 1:p)
    {
      for(j in 1:p)
      {
        var[i,j]<-.Object@degreesOfFreedom *
          ((.Object@scaleMatrix[i,j]^2)+(.Object@scaleMatrix[i,i]*.Object@scaleMatrix[j,j]))
      }
    }
    return(var)
  }
  )

setGeneric(
  name="getWishartStandardDeviation",
  def=function(.Object){standardGeneric("getWishartStandardDeviation")}
  )

setMethod(
  f="getWishartStandardDeviation",
  signature="WishartDistribution",
  definition=function(.Object)
  {
    var<-getWishartVariance(.Object)
    return(sqrt(var))
  }
  )

setGeneric(
  name="getWishartMode",
  def=function(.Object){standardGeneric("getWishartMode")}
  )

setMethod(
  f="getWishartMode",
  signature="WishartDistribution",
  definition=function(.Object)
  {
    if(.Object@degreesOfFreedom>=(length(.Object@scaleMatrix)+1))
    {
      return(.Object@degreesOfFreedom-length(.Object@scaleMatrix)-1)
    }
    else
    {
      return(NULL)
    }
  }
  )