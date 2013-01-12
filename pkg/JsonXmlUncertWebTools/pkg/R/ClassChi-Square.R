setClass(
  Class="ChiSquareDistribution",
  representation=representation(degreesOfFreedom="numeric")
  )

setMethod(
  f="initialize",
  signature="ChiSquareDistribution",
  definition=function(.Object, degrees){
    .Object@degreesOfFreedom<-degrees   
    return(.Object)
  }
  )

setGeneric(
  name="getChiSquareDegreesOfFreedom",
  def=function(.Object) {standardGeneric("getChiSquareDegreesOfFreedom")}
  )

setMethod(
  f="getChiSquareDegreesOfFreedom",
  signature="ChiSquareDistribution",
  definition=function(.Object){
    return(.Object@degreesOfFreedom)
  }
  )

setGeneric(
  name="setChiSquareDegreesOfFreedom<-",
  def=function(.Object,value){standardGeneric("setChiSquareDegreesOfFreedom<-")}
  )

setReplaceMethod(
  f="setChiSquareDegreesOfFreedom",
  signature="ChiSquareDistribution",
  definition=function(.Object,value){
    .Object@degreesOfFreedom<-value
    return(.Object)
  }
  )

setGeneric(
  name="getChiSquareSamples",
  def=function(.Object,number) {standardGeneric("getChiSquareSamples")}
  )

setMethod(
  f="getChiSquareSamples",
  signature="ChiSquareDistribution",
  definition=function(.Object, number){
    sample<-rchisq(number, .Object@degreesOfFreedom)
    temp<-c()
    for(i in 1:length(sample)){
      temp<-c(temp, new(Class="Realisation", Value=as.double(sample[i]), Id=i, Weight=1/number))
    }
    rsample<-new(Class="RandomSample", temp)
    return(rsample)
  }
  )

setGeneric(
  name="getChiSquareMean",
  def=function(.Object){standardGeneric("getChiSquareMean")}
  )

setMethod(
  f="getChiSquareMean",
  signature="ChiSquareDistribution",
  definition=function(.Object)
  {
    return(.Object@degreesOfFreedom)
  }
  )

setGeneric(
  name="getChiSquareVariance",
  def=function(.Object){standardGeneric("getChiSquareVariance")}
  )

setMethod(
  f="getChiSquareVariance",
  signature="ChiSquareDistribution",
  definition=function(.Object)
  {
    return(2*.Object@degreesOfFreedom)
  }
  )
setGeneric(
  name="getChiSquareStandardDeviation",
  def=function(.Object){standardGeneric("getChiSquareStandardDeviation")}
  )

setMethod(
  f="getChiSquareStandardDeviation",
  signature="ChiSquareDistribution",
  definition=function(.Object)
  {
    var<-getChiSquareVariance(.Object)
    return(sqrt(var))
  }
  )

setGeneric(
  name="getChiSquareMode",
  def=function(.Object){standardGeneric("getChiSquareMode")}
  )

setMethod(
  f="getChiSquareMode",
  signature="ChiSquareDistribution",
  definition=function(.Object)
  {
    return(max(0,(.Object@degreesOfFreedom-2)))
  }
  )

setGeneric(
  name="getChiSquareMedian",
  def=function(.Object){standardGeneric("getChiSquareMedian")}
  )

setMethod(
  f="getChiSquareMedian",
  signature="ChiSquareDistribution",
  definition=function(.Object)
  {
    return(.Object@degreesOfFreedom*((1-2/(9*.Object@degreesOfFreedom))^3))
  }
  )

setGeneric(
  name="getChiSquareSkewness",
  def=function(.Object){standardGeneric("getChiSquareSkewness")}
  )

setMethod(
  f="getChiSquareSkewness",
  signature="ChiSquareDistribution",
  definition=function(.Object)
  {
    return(sqrt(8/.Object@degreesOfFreedom))
  }
  )

setGeneric(
  name="getChiSquareKurtosis",
  def=function(.Object){standardGeneric("getChiSquareKurtosis")}
  )

setMethod(
  f="getChiSquareKurtosis",
  signature="ChiSquareDistribution",
  definition=function(.Object)
  {
    return(12/.Object@degreesOfFreedom)
  }
  )