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
    return(.Object)
  }
  )

setGeneric(
  name="getGammaShape",
  def=function(.Object) {standardGeneric("getGammaShape")}
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
  def=function(.Object) {standardGeneric("getGammaScale")}
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
  def=function(.Object,value){standardGeneric("setGammaShape<-")}
  )

setReplaceMethod(
  f="setGammaShape",
  signature="GammaDistribution",
  definition=function(.Object,value){
    .Object@shape<-value
    return(.Object)
  }
  )

setGeneric(
  name="setGammaScale<-",
  def=function(.Object,value){standardGeneric("setGammaScale<-")}
  )

setReplaceMethod(
  f="setGammaScale",
  signature="GammaDistribution",
  definition=function(.Object,value){
    .Object@scale<-value
    return(.Object)
  }
  )

setGeneric(
  name="getGammaSamples",
  def=function(.Object,number) {standardGeneric("getGammaSamples")}
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

setGeneric(
  name="getGammaMean",
  def=function(.Object){standardGeneric("getGammaMean")}
  )

setMethod(
  f="getGammaMean",
  signature="GammaDistribution",
  definition=function(.Object)
  {
    return(.Object@shape*.Object@scale)  
  }
  )

setGeneric(
  name="getGammaVariance",
  def=function(.Object){standardGeneric("getGammaVariance")}
  )

setMethod(
  f="getGammaVariance",
  signature="GammaDistribution",
  definition=function(.Object)
  {
    return(.Object@shape*(.Object@scale^2))  
  }
  )

setGeneric(
  name="getGammaStandardDeviation",
  def=function(.Object){standardGeneric("getGammaStandardDeviation")}
  )

setMethod(
  f="getGammaStandardDeviation",
  signature="GammaDistribution",
  definition=function(.Object)
  {
    var<-getGammaVariance(.Object)
    return(sqrt(var))  
  }
  )

setGeneric(
  name="getGammaMode",
  def=function(.Object){standardGeneric("getGammaMode")}
  )

setMethod(
  f="getGammaMode",
  signature="GammaDistribution",
  definition=function(.Object)
  {
    if(.Object@shape>=1)
    {
      return(.Object@scale*(.Object@shape-1))
    }
  }
  )

setGeneric(
  name="getGammaSkewness",
  def=function(.Object){standardGeneric("getGammaSkewness")}
  )

setMethod(
  f="getGammaSkewness",
  signature="GammaDistribution",
  definition=function(.Object)
  {
    return(2/sqrt(.Object@shape))
  }
  )

setGeneric(
  name="getGammaKurtosis",
  def=function(.Object){standardGeneric("getGammaKurtosis")}
  )

setMethod(
  f="getGammaKurtosis",
  signature="GammaDistribution",
  definition=function(.Object)
  {
    return(6/.Object@shape)  
  }
  )