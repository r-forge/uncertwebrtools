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
    #validObject(.Object)
    return(.Object)
  }
  )

setGeneric(
  name="getLogisticLocation",
  def=function(.Object) {standardGeneric("getLocation")}
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
  def=function(.Object) {standardGeneric("getScale")}
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
  def=function(.Object,Value){standardGeneric("setLocation<-")}
  )

setReplaceMethod(
  f="setLogisticLocation",
  signature="LogisticDistribution",
  definition=function(.Object,Value){
    .Object@location<-Value
    return(.Object)
  }
  )

setGeneric(
  name="setLogisticScale<-",
  def=function(.Object,Value){standardGeneric("setScale<-")}
  )

setReplaceMethod(
  f="setLogisticScale",
  signature="LogisticDistribution",
  definition=function(.Object,Value){
    .Object@scale<-Value
    return(.Object)
  }
  )

setGeneric(
  name="getLogisticSamples",
  def=function(.Object,number) {standardGeneric("getSamples")}
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
