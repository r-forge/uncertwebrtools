setClass(
  Class="InverseGammaDistribution",
  representation=representation(shape="numeric", scale="numeric")
  )

setMethod(
  f="initialize",
  signature="InverseGammaDistribution",
  definition=function(.Object, shape, scale){
    .Object@shape<-shape
    .Object@scale<-scale
    #validObject(.Object)
    return(.Object)
  }
  )

setGeneric(
  name="getInverseGammaShape",
  def=function(.Object) {standardGeneric("getShape")}
  )

setMethod(
  f="getInverseGammaShape",
  signature="InverseGammaDistribution",
  definition=function(.Object){
    return(.Object@shape)
  }
  )

setGeneric(
  name="getInverseGammaScale",
  def=function(.Object) {standardGeneric("getScale")}
  )

setMethod(
  f="getInverseGammaScale",
  signature="InverseGammaDistribution",
  definition=function(.Object){
    return(.Object@scale)
  }
  )

setGeneric(
  name="setInverseGammaShape<-",
  def=function(.Object,Value){standardGeneric("setShape<-")}
  )

setReplaceMethod(
  f="setInverseGammaShape",
  signature="InverseGammaDistribution",
  definition=function(.Object,Value){
    .Object@shape<-Value
    return(.Object)
  }
  )

setGeneric(
  name="setInverseGammaScale<-",
  def=function(.Object,Value){standardGeneric("setScale<-")}
  )

setReplaceMethod(
  f="setInverseGammaScale",
  signature="InverseGammaDistribution",
  definition=function(.Object,Value){
    .Object@scale<-Value
    return(.Object)
  }
  )

# setGeneric(
#   name="getInverseGammaSamples",
#   def=function(.Object,number) {standardGeneric("getSamples")}
#   )
# 
# setMethod(
#   f="getInverseGammaSamples",
#   signature="InverseGammaDistribution",
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
