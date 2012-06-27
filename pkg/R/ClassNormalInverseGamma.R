setClass(
  Class="NormalInverseGammaDistribution",
  representation=representation(mean="numeric", varianceScaling="numeric",
                                shape="numeric", scale="numeric")
  )

setMethod(
  f="initialize",
  signature="NormalInverseGammaDistribution",
  definition=function(.Object, mean, variance, shape, scale){
    .Object@mean<-mean
    .Object@varianceScaling<-variance
    .Object@shape<-shape
    .Object@scale<-scale
    #validObject(.Object)
    return(.Object)
  }
  )

setGeneric(
  name="getNormalInverseGammaMean",
  def=function(.Object) {standardGeneric("getMean")}
  )

setMethod(
  f="getNormalInverseGammaMean",
  signature="NormalInverseGammaDistribution",
  definition=function(.Object){
    return(.Object@mean)
  }
  )

setGeneric(
  name="getNormalInverseGammaVarianceScaling",
  def=function(.Object) {standardGeneric("getVarianceScaling")}
  )

setMethod(
  f="getNormalInverseGammaVarianceScaling",
  signature="NormalInverseGammaDistribution",
  definition=function(.Object){
    return(.Object@varianceScaling)
  }
  )

setGeneric(
  name="getNormalInverseGammaShape",
  def=function(.Object) {standardGeneric("getShape")}
  )

setMethod(
  f="getNormalInverseGammaShape",
  signature="NormalInverseGammaDistribution",
  definition=function(.Object){
    return(.Object@shape)
  }
  )

setGeneric(
  name="getNormalInverseGammaScale",
  def=function(.Object) {standardGeneric("getScale")}
  )

setMethod(
  f="getNormalInverseGammaScale",
  signature="NormalInverseGammaDistribution",
  definition=function(.Object){
    return(.Object@scale)
  }
  )

setGeneric(
  name="setNormalInverseGammaMean<-",
  def=function(.Object,Value){standardGeneric("setMean<-")}
  )

setReplaceMethod(
  f="setNormalInverseGammaMean",
  signature="NormalInverseGammaDistribution",
  definition=function(.Object,Value){
    .Object@mean<-Value
    return(.Object)
  }
  )

setGeneric(
  name="setNormalInverseGammaVarianceScaling<-",
  def=function(.Object,Value){standardGeneric("setVarianceScaling<-")}
  )

setReplaceMethod(
  f="setNormalInverseGammaVarianceScaling",
  signature="NormalInverseGammaDistribution",
  definition=function(.Object,Value){
    .Object@varianceScaling<-Value
    return(.Object)
  }
  )

setGeneric(
  name="setNormalInverseGammaShape<-",
  def=function(.Object,Value){standardGeneric("setShape<-")}
  )

setReplaceMethod(
  f="setNormalInverseGammaShape",
  signature="NormalInverseGammaDistribution",
  definition=function(.Object,Value){
    .Object@shape<-Value
    return(.Object)
  }
  )

setGeneric(
  name="setNormalInverseGammaScale<-",
  def=function(.Object,Value){standardGeneric("setScale<-")}
  )

setReplaceMethod(
  f="setNormalInverseGammaScale",
  signature="NormalInverseGammaDistribution",
  definition=function(.Object,Value){
    .Object@scale<-Value
    return(.Object)
  }
  )

# setGeneric(
#   name="getNormalInverseGammaSamples",
#   def=function(.Object,number) {standardGeneric("getSamples")}
#   )
# 
# setMethod(
#   f="getNormalInverseGammaSamples",
#   signature="NormalInverseGammaDistribution",
#   definition=function(.Object, number){
#     sample<-rgamma(number, shape=.Object@shape, scale=.Object@scale)
#     temp<-c()
#     for(i in 1:length(sample)){
#       temp<-c(temp, new(Class="Realisation", Value=as.double(sample[i]), Id=i, Weight=1/number))
#     }
#     rsample<-new(Class="RandomSample", temp)
#     return(rsample)
#   }
#   )
