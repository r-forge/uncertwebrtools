setClass(
  Class="LogNormalDistribution",
  representation=representation(logScale="numeric", shape="numeric")
  )

setMethod(
  f="initialize",
  signature="LogNormalDistribution",
  definition=function(.Object, scale, shape){
    .Object@logScale<-scale
    .Object@shape<-shape
    #validObject(.Object)
    return(.Object)
  }
  )

setGeneric(
  name="getLogNormalLogScale",
  def=function(.Object) {standardGeneric("getLogScale")}
  )

setMethod(
  f="getLogNormalLogScale",
  signature="LogNormalDistribution",
  definition=function(.Object){
    return(.Object@logScale)
  }
  )

setGeneric(
  name="getLogNormalShape",
  def=function(.Object) {standardGeneric("getShape")}
  )

setMethod(
  f="getLogNormalShape",
  signature="LogNormalDistribution",
  definition=function(.Object){
    return(.Object@shape)
  }
  )

setGeneric(
  name="setLogNormalLogScale<-",
  def=function(.Object,Value){standardGeneric("setLogScale<-")}
  )

setReplaceMethod(
  f="setLogNormalLogScale",
  signature="LogNormalDistribution",
  definition=function(.Object,Value){
    .Object@logScale<-Value
    return(.Object)
  }
  )

setGeneric(
  name="setLogNormalShape<-",
  def=function(.Object,Value){standardGeneric("setShape<-")}
  )

setReplaceMethod(
  f="setLogNormalShape",
  signature="LogNormalDistribution",
  definition=function(.Object,Value){
    .Object@shape<-Value
    return(.Object)
  }
  )

setGeneric(
  name="getLogNormalSamples",
  def=function(.Object,number) {standardGeneric("getSamples")}
  )

setMethod(
  f="getLogNormalSamples",
  signature="LogNormalDistribution",
  definition=function(.Object, number){
    sample<-rlnorm(number, meanlog=.Object@logScale, sdlog=sqrt(.Object@shape))
    temp<-c()
    for(i in 1:length(sample)){
      temp<-c(temp, new(Class="Realisation", Value=as.double(sample[i]), Id=i, Weight=1/number))
    }
    rsample<-new(Class="RandomSample", temp)
    return(rsample)
  }
  )
