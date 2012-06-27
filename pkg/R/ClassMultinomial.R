setClass(
  Class="MultinomialDistribution",
  representation=representation(numberOfTrials="integer",
                                probabilityOfSuccess="vector")
  )

setMethod(
  f="initialize",
  signature="MultinomialDistribution",
  definition=function(.Object, numberOfTrials, probabilityOfSuccess){
    .Object@numberOfTrials<-numberOfTrials
    .Object@probabilityOfSuccess<-probabilityOfSuccess
    #validObject(.Object)
    return(.Object)
  }
  )

setGeneric(
  name="getMultinomialNumberOfTrials",
  def=function(.Object) {standardGeneric("getNumberOfTrials")}
  )

setMethod(
  f="getMultinomialNumberOfTrials",
  signature="MultinomialDistribution",
  definition=function(.Object){
    return(.Object@numberOfTrials)
  }
  )

setGeneric(
  name="getMultinomialProbabilityOfSuccess",
  def=function(.Object) {standardGeneric("getProbabilityOfSuccess")}
  )

setMethod(
  f="getMultinomialProbabilityOfSuccess",
  signature="MultinomialDistribution",
  definition=function(.Object){
    return(.Object@probabilityOfSuccess)
  }
  )

setGeneric(
  name="setMultinomialNumberOfTrials<-",
  def=function(.Object,Value){standardGeneric("setNumberOfTrials<-")}
  )

setReplaceMethod(
  f="setMultinomialNumberOfTrials",
  signature="MultinomialDistribution",
  definition=function(.Object,Value){
    .Object@numberOfTrials<-Value
    return(.Object)
  }
  )

setGeneric(
  name="setMultinomialProbabilityOfSuccess<-",
  def=function(.Object,Value){standardGeneric("setProbabilityOfSuccess<-")}
  )

setReplaceMethod(
  f="setMultinomialProbabilityOfSuccess",
  signature="MultinomialDistribution",
  definition=function(.Object,Value){
    .Object@probabilityOfSuccess<-Value
    return(.Object)
  }
  )
# 
# setGeneric(
#   name="getMultinomialSamples",
#   def=function(.Object,number) {standardGeneric("getSamples")}
#   )
# 
# setMethod(
#   f="getMultinomialSamples",
#   signature="MultinomialDistribution",
#   definition=function(.Object, number){
#     sample<-rweibull(number, .Object@probabilityOfSuccess, numberOfTrials=.Object@numberOfTrials)
#     temp<-c()
#     for(i in 1:length(sample)){
#       temp<-c(temp, new(Class="Realisation", Value=as.double(sample[i]), Id=i, Weight=1/number))
#     }
#     rsample<-new(Class="RandomSample", temp)
#     return(rsample)
#   }
#   )
