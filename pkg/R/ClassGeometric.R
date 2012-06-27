setClass(
  Class="GeometricDistribution",
  representation=representation(probability="numeric")
  )

setMethod(
  f="initialize",
  signature="GeometricDistribution",
  definition=function(.Object, probability){
    .Object@probability<-probability
    #validObject(.Object)
    return(.Object)
  }
  )

setGeneric(
  name="getGeometricProbability",
  def=function(.Object) {standardGeneric("getProbability")}
  )

setMethod(
  f="getGeometricProbability",
  signature="GeometricDistribution",
  definition=function(.Object){
    return(.Object@probability)
  }
  )

setGeneric(
  name="setGeometricProbability<-",
  def=function(.Object,Value){standardGeneric("setProbability<-")}
  )

setReplaceMethod(
  f="setGeometricProbability",
  signature="GeometricDistribution",
  definition=function(.Object,Value){
    .Object@probability<-Value
    return(.Object)
  }
  )

setGeneric(
  name="getGeometricSamples",
  def=function(.Object,number) {standardGeneric("getSamples")}
  )

setMethod(
  f="getGeometricSamples",
  signature="GeometricDistribution",
  definition=function(.Object, number){
    sample<-rexp(number, .Object@probability)
    temp<-c()
    for(i in 1:length(sample)){
      temp<-c(temp, new(Class="Realisation", Value=as.double(sample[i]), Id=i, Weight=1/number))
    }
    rsample<-new(Class="RandomSample", temp)
    return(rsample)
  }
  )
