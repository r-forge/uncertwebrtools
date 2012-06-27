setClass(
  Class="MultivariateStudentTDistribution",
  representation=representation(mean="vector", 
                                covarianceMatrix="matrix", 
                                degreesOfFreedom="integer")
  )

setMethod(
  f="initialize",
  signature="MultivariateStudentTDistribution",
  definition=function(.Object, mean, covariance, degrees){
    .Object@mean<-mean
    .Object@covarianceMatrix<-covariance
    .Object@degreesOfFreedom<-degrees
    #validObject(.Object)
    return(.Object)
  }
  )

setGeneric(
  name="getMultivariateStudentTMean",
  def=function(.Object) {standardGeneric("getMean")}
  )

setMethod(
  f="getMultivariateStudentTMean",
  signature="MultivariateStudentTDistribution",
  definition=function(.Object){
    return(.Object@mean)
  }
  )

setGeneric(
  name="getMultivariateStudentTCovarianceMatrix",
  def=function(.Object) {standardGeneric("getCovarianceMatrix")}
  )

setMethod(
  f="getMultivariateStudentTCovarianceMatrix",
  signature="MultivariateStudentTDistribution",
  definition=function(.Object){
    return(.Object@covarianceMatrix)
  }
  )

setGeneric(
  name="getMultivariateStudentTDegreesOfFreedom",
  def=function(.Object) {standardGeneric("getDegreesOfFreedom")}
  )

setMethod(
  f="getMultivariateStudentTDegreesOfFreedom",
  signature="MultivariateStudentTDistribution",
  definition=function(.Object){
    return(.Object@degreesOfFreedom)
  }
  )

setGeneric(
  name="setMultivariateStudentTMean<-",
  def=function(.Object,Value){standardGeneric("setMean<-")}
  )

setReplaceMethod(
  f="setMultivariateStudentTMean",
  signature="MultivariateStudentTDistribution",
  definition=function(.Object,Value){
    .Object@mean<-Value
    return(.Object)
  }
  )

setGeneric(
  name="setMultivariateStudentTCovarianceMatrix<-",
  def=function(.Object,Value){standardGeneric("setCovarianceMatrix<-")}
  )

setReplaceMethod(
  f="setMultivariateStudentTCovarianceMatrix",
  signature="MultivariateStudentTDistribution",
  definition=function(.Object,Value){
    .Object@covarianceMatrix<-Value
    return(.Object)
  }
  )

setGeneric(
  name="setMultivariateStudentTDegreesOfFreedom<-",
  def=function(.Object,Value){standardGeneric("setDegreesOfFreedom<-")}
  )

setReplaceMethod(
  f="setMultivariateStudentTDegreesOfFreedom",
  signature="MultivariateStudentTDistribution",
  definition=function(.Object,Value){
    .Object@degreesOfFreedom<-Value
    return(.Object)
  }
  )
# 
# setGeneric(
#   name="getMultivariateStudentTSamples",
#   def=function(.Object,number) {standardGeneric("getSamples")}
#   )
# 
# setMethod(
#   f="getMultivariateStudentTSamples",
#   signature="MultivariateStudentTDistribution",
#   definition=function(.Object, number){
#     sample<-rt(number, .Object@degreesOfFreedom)
#     temp<-c()
#     for(i in 1:length(sample)){
#       temp<-c(temp, new(Class="Realisation", Value=as.double(sample[i]), Id=i, Weight=1/number))
#     }
#     rsample<-new(Class="RandomSample", temp)
#     return(rsample)
#   }
#   )
