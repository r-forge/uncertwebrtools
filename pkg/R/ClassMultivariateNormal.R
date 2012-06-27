setClass(
  Class="MultivariateNormalDistribution",
  representation=representation(mean="vector",
                                covarianceMatrix="matrix")
  )

setMethod(
  f="initialize",
  signature="MultivariateNormalDistribution",
  definition=function(.Object, mean, covariance){
    .Object@mean<-mean
    .Object@covarianceMatrix<-covariance
    #validObject(.Object)
    return(.Object)
  }
  )

setGeneric(
  name="getMultivariateNormalMean",
  def=function(.Object) {standardGeneric("getMean")}
  )

setMethod(
  f="getMultivariateNormalMean",
  signature="MultivariateNormalDistribution",
  definition=function(.Object){
    return(.Object@mean)
  }
  )

setGeneric(
  name="getMultivariateNormalCovarianceMatrix",
  def=function(.Object) {standardGeneric("getCovarianceMatrix")}
  )

setMethod(
  f="getMultivariateNormalCovarianceMatrix",
  signature="MultivariateNormalDistribution",
  definition=function(.Object){
    return(.Object@covarianceMatrix)
  }
  )

setGeneric(
  name="setMultivariateNormalMean<-",
  def=function(.Object,Value){standardGeneric("setMean<-")}
  )

setReplaceMethod(
  f="setMultivariateNormalMean",
  signature="MultivariateNormalDistribution",
  definition=function(.Object,Value){
    .Object@mean<-Value
    return(.Object)
  }
  )

setGeneric(
  name="setMultivariateNormalCovarianceMatrix<-",
  def=function(.Object,Value){standardGeneric("setCovarianceMatrix<-")}
  )

setReplaceMethod(
  f="setMultivariateNormalCovarianceMatrix",
  signature="MultivariateNormalDistribution",
  definition=function(.Object,Value){
    .Object@covarianceMatrix<-Value
    return(.Object)
  }
  )
# 
# setGeneric(
#   name="getMultivariateNormalSamples",
#   def=function(.Object,number) {standardGeneric("getSamples")}
#   )
# 
# setMethod(
#   f="getMultivariateNormalSamples",
#   signature="MultivariateNormalDistribution",
#   definition=function(.Object, number){
#     sample<-rweibull(number, .Object@covarianceMatrix, mean=.Object@mean)
#     temp<-c()
#     for(i in 1:length(sample)){
#       temp<-c(temp, new(Class="Realisation", Value=as.double(sample[i]), Id=i, Weight=1/number))
#     }
#     rsample<-new(Class="RandomSample", temp)
#     return(rsample)
#   }
#   )
