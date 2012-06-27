setClass(
  Class="ChiSquareDistribution",
  representation=representation(degreesOfFreedom="integer")
  )

setMethod(
  f="initialize",
  signature="ChiSquareDistribution",
  definition=function(.Object, degrees){
    .Object@degreesOfFreedom<-degrees   
    #validObject(.Object)
    return(.Object)
  }
  )

setGeneric(
  name="getChiSquareDegreesOfFreedom",
  def=function(.Object) {standardGeneric("getDegreesOfFreedom")}
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
  def=function(.Object,Value){standardGeneric("setDegreesOfFreedom<-")}
  )

setReplaceMethod(
  f="setChiSquareDegreesOfFreedom",
  signature="ChiSquareDistribution",
  definition=function(.Object,Value){
    .Object@degreesOfFreedom<-Value
    return(.Object)
  }
  )

setGeneric(
  name="getChiSquareSamples",
  def=function(.Object,number) {standardGeneric("getSamples")}
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
