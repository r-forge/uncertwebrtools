setClass(
  Class="WeibullDistribution",
  representation=representation(scale="numeric", shape="numeric")
  )

setMethod(
  f="initialize",
  signature="WeibullDistribution",
  definition=function(.Object, scale, shape){
    .Object@scale<-scale
    .Object@shape<-shape
    #validObject(.Object)
    return(.Object)
  }
  )

setGeneric(
  name="getWeibullScale",
  def=function(.Object) {standardGeneric("getScale")}
  )

setMethod(
  f="getWeibullScale",
  signature="WeibullDistribution",
  definition=function(.Object){
    return(.Object@scale)
  }
  )

setGeneric(
  name="getWeibullShape",
  def=function(.Object) {standardGeneric("getShape")}
  )

setMethod(
  f="getWeibullShape",
  signature="WeibullDistribution",
  definition=function(.Object){
    return(.Object@shape)
  }
  )

setGeneric(
  name="setWeibullScale<-",
  def=function(.Object,Value){standardGeneric("setScale<-")}
  )

setReplaceMethod(
  f="setWeibullScale",
  signature="WeibullDistribution",
  definition=function(.Object,Value){
    .Object@scale<-Value
    return(.Object)
  }
  )

setGeneric(
  name="setWeibullShape<-",
  def=function(.Object,Value){standardGeneric("setShape<-")}
  )

setReplaceMethod(
  f="setWeibullShape",
  signature="WeibullDistribution",
  definition=function(.Object,Value){
    .Object@shape<-Value
    return(.Object)
  }
  )

setGeneric(
  name="getWeibullSamples",
  def=function(.Object,number) {standardGeneric("getSamples")}
  )

setMethod(
  f="getWeibullSamples",
  signature="WeibullDistribution",
  definition=function(.Object, number){
    sample<-rweibull(number, .Object@shape, scale=.Object@scale)
    temp<-c()
    for(i in 1:length(sample)){
      temp<-c(temp, new(Class="Realisation", Value=as.double(sample[i]), Id=i, Weight=1/number))
    }
    rsample<-new(Class="RandomSample", temp)
    return(rsample)
  }
  )
