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
    return(.Object)
  }
  )

setGeneric(
  name="getCauchyLocation",
  def=function(.Object) {standardGeneric("getCauchyLocation")}
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
  def=function(.Object) {standardGeneric("getCauchyScale")}
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
  def=function(.Object,value){standardGeneric("setCauchyLocation<-")}
  )

setReplaceMethod(
  f="setCauchyLocation",
  signature="CauchyDistribution",
  definition=function(.Object,value){
    .Object@location<-value
    return(.Object)
  }
  )

setGeneric(
  name="setCauchyScale<-",
  def=function(.Object,value){standardGeneric("setCauchyScale<-")}
  )

setReplaceMethod(
  f="setCauchyScale",
  signature="CauchyDistribution",
  definition=function(.Object,value){
    .Object@scale<-value
    return(.Object)
  }
  )

setGeneric(
  name="getCauchySamples",
  def=function(.Object,number) {standardGeneric("getCauchySamples")}
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

setGeneric(
  name="getCauchyMedian",
  def=function(.Object){standardGeneric("getCauchyMedian")}
  )

setMethod(
  f="getCauchyMedian",
  signature="CauchyDistribution",
  definition=function(.Object)
  {
    return(.Object@location)
  }
  )

setGeneric(
  name="getCauchyMode",
  def=function(.Object){standardGeneric("getCauchyMode")}
  )

setMethod(
  f="getCauchyMode",
  signature="CauchyDistribution",
  definition=function(.Object)
  {
    return(.Object@location)
  }
  )