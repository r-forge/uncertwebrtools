setClass(
  Class="ParetoDistribution",
  representation=representation(scale="numeric", shape="numeric")
  )

setMethod(
  f="initialize",
  signature="ParetoDistribution",
  definition=function(.Object, scale, shape){
    .Object@scale<-scale
    .Object@shape<-shape
    #validObject(.Object)
    return(.Object)
  }
  )

setGeneric(
  name="getParetoScale",
  def=function(.Object) {standardGeneric("getScale")}
  )

setMethod(
  f="getParetoScale",
  signature="ParetoDistribution",
  definition=function(.Object){
    return(.Object@scale)
  }
  )

setGeneric(
  name="getParetoShape",
  def=function(.Object) {standardGeneric("getShape")}
  )

setMethod(
  f="getParetoShape",
  signature="ParetoDistribution",
  definition=function(.Object){
    return(.Object@shape)
  }
  )

setGeneric(
  name="setParetoScale<-",
  def=function(.Object,Value){standardGeneric("setScale<-")}
  )

setReplaceMethod(
  f="setParetoScale",
  signature="ParetoDistribution",
  definition=function(.Object,Value){
    .Object@scale<-Value
    return(.Object)
  }
  )

setGeneric(
  name="setParetoShape<-",
  def=function(.Object,Value){standardGeneric("setShape<-")}
  )

setReplaceMethod(
  f="setParetoShape",
  signature="ParetoDistribution",
  definition=function(.Object,Value){
    .Object@shape<-Value
    return(.Object)
  }
  )
# 
# setGeneric(
#   name="getParetoSamples",
#   def=function(.Object,number) {standardGeneric("getSamples")}
#   )
# 
# setMethod(
#   f="getParetoSamples",
#   signature="ParetoDistribution",
#   definition=function(.Object, number){
#     sample<-rweibull(number, .Object@shape, scale=.Object@scale)
#     temp<-c()
#     for(i in 1:length(sample)){
#       temp<-c(temp, new(Class="Realisation", Value=as.double(sample[i]), Id=i, Weight=1/number))
#     }
#     rsample<-new(Class="RandomSample", temp)
#     return(rsample)
#   }
#   )
