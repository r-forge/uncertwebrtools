setClass(
  Class="GeometricDistribution",
  representation=representation(probability="numeric")
  )

setMethod(
  f="initialize",
  signature="GeometricDistribution",
  definition=function(.Object, probability){
    .Object@probability<-probability
    return(.Object)
  }
  )

setGeneric(
  name="getGeometricProbability",
  def=function(.Object) {standardGeneric("getGeometricProbability")}
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
  def=function(.Object,value){standardGeneric("setGeometricProbability<-")}
  )

setReplaceMethod(
  f="setGeometricProbability",
  signature="GeometricDistribution",
  definition=function(.Object,value){
    .Object@probability<-value
    return(.Object)
  }
  )

setGeneric(
  name="getGeometricSamples",
  def=function(.Object,number) {standardGeneric("getGeometricSamples")}
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

setGeneric(
  name="getGeometricMean",
  def=function(.Object){standardGeneric("getGeometricMean")}
  )

setMethod(
  f="getGeometricMean",
  signature="GeometricDistribution",
  definition=function(.Object)
  {
    return(1/.Object@probability)  
  }
  )

setGeneric(
  name="getGeometricVariance",
  def=function(.Object){standardGeneric("getGeometricVariance")}
  )

setMethod(
  f="getGeometricVariance",
  signature="GeometricDistribution",
  definition=function(.Object)
  {
    return((1-.Object@probability)/.Object@probability^2)  
  }
  )

setGeneric(
  name="getGeometricStandardDeviation",
  def=function(.Object){standardGeneric("getGeometricStandardDeviation")}
  )

setMethod(
  f="getGeometricStandardDeviation",
  signature="GeometricDistribution",
  definition=function(.Object)
  {
    var<-getGeometricVariance(.Object)
    return(sqrt(var))  
  }
  )

setGeneric(
  name="getGeometricMode",
  def=function(.Object){standardGeneric("getGeometricMode")}
  )

setMethod(
  f="getGeometricMode",
  signature="GeometricDistribution",
  definition=function(.Object)
  {
    return(1)  
  }
  )

setGeneric(
  name="getGeometricMedian",
  def=function(.Object){standardGeneric("getGeometricMedian")}
  )

setMethod(
  f="getGeometricMedian",
  signature="GeometricDistribution",
  definition=function(.Object)
  {
    t<--1/log((1-.Object@probability),2)
    if(t-floor(t)==0)
    {
      return(c(ceiling(t), ceiling(t)-1))
    }
    else
    {
      return(ceiling(t))
    }
  }
  )

setGeneric(
  name="getGeometricSkewness",
  def=function(.Object){standardGeneric("getGeometricSkewness")}
  )

setMethod(
  f="getGeometricSkewness",
  signature="GeometricDistribution",
  definition=function(.Object)
  {
    return((2-.Object@probability)/sqrt(1-.Object@probability))  
  }
  )

setGeneric(
  name="getGeometricKurtosis",
  def=function(.Object){standardGeneric("getGeometricKurtosis")}
  )

setMethod(
  f="getGeometricKurtosis",
  signature="GeometricDistribution",
  definition=function(.Object)
  {
    return(6+((.Object@probability^2)/(1-.Object@probability)))  
  }
  )