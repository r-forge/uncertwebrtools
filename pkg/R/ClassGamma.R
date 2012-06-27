setClass(
  Class="GammaDistribution",
  representation=representation(shape="numeric", scale="numeric")
  )

setMethod(
  f="initialize",
  signature="GammaDistribution",
  definition=function(.Object, shape, scale){
    .Object@shape<-shape
    .Object@scale<-scale
    #validObject(.Object)
    return(.Object)
  }
  )

setGeneric(
  name="getGammaShape",
  def=function(.Object) {standardGeneric("getShape")}
  )

setMethod(
  f="getGammaShape",
  signature="GammaDistribution",
  definition=function(.Object){
    return(.Object@shape)
  }
  )

setGeneric(
  name="getGammaScale",
  def=function(.Object) {standardGeneric("getScale")}
  )

setMethod(
  f="getGammaScale",
  signature="GammaDistribution",
  definition=function(.Object){
    return(.Object@scale)
  }
  )

setGeneric(
  name="setGammaShape<-",
  def=function(.Object,Value){standardGeneric("setShape<-")}
  )

setReplaceMethod(
  f="setGammaShape",
  signature="GammaDistribution",
  definition=function(.Object,Value){
    .Object@shape<-Value
    return(.Object)
  }
  )

setGeneric(
  name="setGammaScale<-",
  def=function(.Object,Value){standardGeneric("setScale<-")}
  )

setReplaceMethod(
  f="setGammaScale",
  signature="GammaDistribution",
  definition=function(.Object,Value){
    .Object@scale<-Value
    return(.Object)
  }
  )

setGeneric(
  name="getGammaSamples",
  def=function(.Object,number) {standardGeneric("getSamples")}
  )

setMethod(
  f="getGammaSamples",
  signature="GammaDistribution",
  definition=function(.Object, number){
    sample<-rgamma(number, shape=.Object@shape, scale=.Object@scale)
    temp<-c()
    for(i in 1:length(sample)){
      temp<-c(temp, new(Class="Realisation", Value=as.double(sample[i]), Id=i, Weight=1/number))
    }
    rsample<-new(Class="RandomSample", temp)
    return(rsample)
  }
  )
