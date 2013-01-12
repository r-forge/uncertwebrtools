setClass(
  Class="LogisticDistribution",
  representation=representation(location="numeric", scale="numeric")
  )

setMethod(
  f="initialize",
  signature="LogisticDistribution",
  definition=function(.Object, location, scale){
    .Object@location<-location
    .Object@scale<-scale
    return(.Object)
  }
  )

setGeneric(
  name="getLogisticLocation",
  def=function(.Object) {standardGeneric("getLogisticLocation")}
  )

setMethod(
  f="getLogisticLocation",
  signature="LogisticDistribution",
  definition=function(.Object){
    return(.Object@location)
  }
  )

setGeneric(
  name="getLogisticScale",
  def=function(.Object) {standardGeneric("getLogisticScale")}
  )

setMethod(
  f="getLogisticScale",
  signature="LogisticDistribution",
  definition=function(.Object){
    return(.Object@scale)
  }
  )

setGeneric(
  name="setLogisticLocation<-",
  def=function(.Object,value){standardGeneric("setLogisticLocation<-")}
  )

setReplaceMethod(
  f="setLogisticLocation",
  signature="LogisticDistribution",
  definition=function(.Object,value){
    .Object@location<-value
    return(.Object)
  }
  )

setGeneric(
  name="setLogisticScale<-",
  def=function(.Object,value){standardGeneric("setLogisticScale<-")}
  )

setReplaceMethod(
  f="setLogisticScale",
  signature="LogisticDistribution",
  definition=function(.Object,value){
    .Object@scale<-value
    return(.Object)
  }
  )

setGeneric(
  name="getLogisticSamples",
  def=function(.Object,number) {standardGeneric("getLogisticSamples")}
  )

setMethod(
  f="getLogisticSamples",
  signature="LogisticDistribution",
  definition=function(.Object, number){
    sample<-rlogis(number, location=.Object@location, scale=.Object@scale)
    temp<-c()
    for(i in 1:length(sample)){
      temp<-c(temp, new(Class="Realisation", Value=as.double(sample[i]), Id=i, Weight=1/number))
    }
    rsample<-new(Class="RandomSample", temp)
    return(rsample)
  }
  )

setGeneric(
  name="getLogisticMean",
  def=function(.Object){standardGeneric("getLogisticMean")}
  )

setMethod(
  f="getLogisticMean",
  signature="LogisticDistribution",
  definition=function(.Object)
  {
    return(.Object@location)  
  }
  )

setGeneric(
  name="getLogisticVariance",
  def=function(.Object){standardGeneric("getLogisticVariance")}
  )

setMethod(
  f="getLogisticVariance",
  signature="LogisticDistribution",
  definition=function(.Object)
  {
    return((.Object@scale^2)*(pi^2)/3)  
  }
  )

setGeneric(
  name="getLogisticStandardDeviation",
  def=function(.Object){standardGeneric("getLogisticStandardDeviation")}
  )

setMethod(
  f="getLogisticStandardDeviation",
  signature="LogisticDistribution",
  definition=function(.Object)
  {
    var<-getLogisticVariance(.Object)
    return(sqrt(var))  
  }
  )

setGeneric(
  name="getLogisticMode",
  def=function(.Object){standardGeneric("getLogisticMode")}
  )

setMethod(
  f="getLogisticMode",
  signature="LogisticDistribution",
  definition=function(.Object)
  {
    return(.Object@location)  
  }
  )

setGeneric(
  name="getLogisticMedian",
  def=function(.Object){standardGeneric("getLogisticMedian")}
  )

setMethod(
  f="getLogisticMedian",
  signature="LogisticDistribution",
  definition=function(.Object)
  {
    return(.Object@location)  
  }
  )

setGeneric(
  name="getLogisticSkewness",
  def=function(.Object){standardGeneric("getLogisticSkewness")}
  )

setMethod(
  f="getLogisticSkewness",
  signature="LogisticDistribution",
  definition=function(.Object)
  {
    return(0)  
  }
  )

setGeneric(
  name="getLogisticKurtosis",
  def=function(.Object){standardGeneric("getLogisticKurtosis")}
  )

setMethod(
  f="getLogisticKurtosis",
  signature="LogisticDistribution",
  definition=function(.Object)
  {
    return(6/5)  
  }
  )