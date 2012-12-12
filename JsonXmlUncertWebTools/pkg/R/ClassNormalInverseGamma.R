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
    return(.Object)
  }
  )

setGeneric(
  name="getNormInvGammaMean",
  def=function(.Object) {standardGeneric("getNormInvGammaMean")}
  )

setMethod(
  f="getNormInvGammaMean",
  signature="NormalInverseGammaDistribution",
  definition=function(.Object){
    return(.Object@mean)
  }
  )

setGeneric(
  name="getNormInvGammaVarianceScale",
  def=function(.Object) {standardGeneric("getNormInvGammaVarianceScale")}
  )

setMethod(
  f="getNormInvGammaVarianceScale",
  signature="NormalInverseGammaDistribution",
  definition=function(.Object){
    return(.Object@varianceScaling)
  }
  )

setGeneric(
  name="getNormInvGammaShape",
  def=function(.Object) {standardGeneric("getNormInvGammaShape")}
  )

setMethod(
  f="getNormInvGammaShape",
  signature="NormalInverseGammaDistribution",
  definition=function(.Object){
    return(.Object@shape)
  }
  )

setGeneric(
  name="getNormInvGammaScale",
  def=function(.Object) {standardGeneric("getNormInvGammaScale")}
  )

setMethod(
  f="getNormInvGammaScale",
  signature="NormalInverseGammaDistribution",
  definition=function(.Object){
    return(.Object@scale)
  }
  )

setGeneric(
  name="setNormInvGammaMean<-",
  def=function(.Object,value){standardGeneric("setNormInvGammaMean<-")}
  )

setReplaceMethod(
  f="setNormInvGammaMean",
  signature="NormalInverseGammaDistribution",
  definition=function(.Object,value){
    .Object@mean<-value
    return(.Object)
  }
  )

setGeneric(
  name="setNormInvGammaVarianceScale<-",
  def=function(.Object,value){standardGeneric("setNormInvGammaVarianceScale<-")}
  )

setReplaceMethod(
  f="setNormInvGammaVarianceScale",
  signature="NormalInverseGammaDistribution",
  definition=function(.Object,value){
    .Object@varianceScaling<-value
    return(.Object)
  }
  )

setGeneric(
  name="setNormInvGammaShape<-",
  def=function(.Object,value){standardGeneric("setNormInvGammaShape<-")}
  )

setReplaceMethod(
  f="setNormInvGammaShape",
  signature="NormalInverseGammaDistribution",
  definition=function(.Object,value){
    .Object@shape<-value
    return(.Object)
  }
  )

setGeneric(
  name="setNormInvGammaScale<-",
  def=function(.Object,value){standardGeneric("setNormInvGammaScale<-")}
  )

setReplaceMethod(
  f="setNormInvGammaScale",
  signature="NormalInverseGammaDistribution",
  definition=function(.Object,value){
    .Object@scale<-value
    return(.Object)
  }
  )

setGeneric(
  name="getNormInvGammaSamples",
  def=function(.Object,number) {standardGeneric("getNormInvGammaSamples")}
  )

setMethod(
  f="getNormInvGammaSamples",
  signature="NormalInverseGammaDistribution",
  definition=function(.Object, number){
    iScale<-1/.Object@scale
    gSample<-rgamma(number, shape=.Object@shape, scale=iScale)
    sample<-c()
    temp<-c()
    for(i in 1:length(gSample)){
      isample<-1/gSample[i]
      sample[i]<-rnorm(1, mean=.Object@mean, sqrt(isample/.Object@varianceScaling))
      temp<-c(temp, new(Class="Realisation", Value=as.double(sample[i]), Id=i, Weight=1/number))
    }
    rsample<-new(Class="RandomSample", temp)
    return(rsample)
  }
  )

