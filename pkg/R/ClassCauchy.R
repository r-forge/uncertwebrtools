setClass(
  Class="CauchyDistribution",
  representation=representation(location="numeric", scale="numeric")
  )

setMethod(
  f="initialize",
  signature="CauchyDistribution",
  definition=function(.Object, location, scale){
    .Object@location<-location
    .Object@scale<-scale
    #validObject(.Object)
    return(.Object)
  }
  )

setGeneric(
  name="getCauchyLocation",
  def=function(.Object) {standardGeneric("getLocation")}
  )

setMethod(
  f="getCauchyLocation",
  signature="CauchyDistribution",
  definition=function(.Object){
    return(.Object@location)
  }
  )

setGeneric(
  name="getCauchyScale",
  def=function(.Object) {standardGeneric("getScale")}
  )

setMethod(
  f="getCauchyScale",
  signature="CauchyDistribution",
  definition=function(.Object){
    return(.Object@scale)
  }
  )

setGeneric(
  name="setCauchyLocation<-",
  def=function(.Object,Value){standardGeneric("setLocation<-")}
  )

setReplaceMethod(
  f="setCauchyLocation",
  signature="CauchyDistribution",
  definition=function(.Object,Value){
    .Object@location<-Value
    return(.Object)
  }
  )

setGeneric(
  name="setCauchyScale<-",
  def=function(.Object,Value){standardGeneric("setScale<-")}
  )

setReplaceMethod(
  f="setCauchyScale",
  signature="CauchyDistribution",
  definition=function(.Object,Value){
    .Object@scale<-Value
    return(.Object)
  }
  )

setGeneric(
  name="getCauchySamples",
  def=function(.Object,number) {standardGeneric("getSamples")}
  )

setMethod(
  f="getCauchySamples",
  signature="CauchyDistribution",
  definition=function(.Object, number){
    sample<-rcauchy(number, location=.Object@location, scale=.Object@scale)
    temp<-c()
    for(i in 1:length(sample)){
      temp<-c(temp, new(Class="Realisation", Value=as.double(sample[i]), Id=i, Weight=1/number))
    }
    rsample<-new(Class="RandomSample", temp)
    return(rsample)
  }
  )
