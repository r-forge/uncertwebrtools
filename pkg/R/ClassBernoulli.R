setClass(
  Class="BernoulliDistribution",
  representation=representation(probabilities="numeric")
  )

setMethod(
  f="initialize",
  signature="BernoulliDistribution",
  definition=function(.Object, probability){
    .Object@probabilities<-probability 
    #validObject(.Object)
    return(.Object)
  }
  )

setGeneric(
  name="getBernoulliProbabilities",
  def=function(.Object) {standardGeneric("getProbabilities")}
  )

setMethod(
  f="getBernoulliProbabilities",
  signature="BernoulliDistribution",
  definition=function(.Object){
    return(.Object@probabilities)
  }
  )

setGeneric(
  name="setBernoulliProbabilities<-",
  def=function(.Object,Value){standardGeneric("setProbabilities<-")}
  )

setReplaceMethod(
  f="setBernoulliProbabilities",
  signature="BernoulliDistribution",
  definition=function(.Object,Value){
    .Object@probabilities<-Value
    return(.Object)
  }
  )

# setGeneric(
#   name="getBernoulliSamples",
#   def=function(.Object,number) {standardGeneric("getSamples")}
#   )
# 
# setMethod(
#   f="getBernoulliSamples",
#   signature="BernoulliDistribution",
#   definition=function(.Object, number){
#     sample<-rchisq(number, .Object@probabilities)
#     temp<-c()
#     for(i in 1:length(sample)){
#       temp<-c(temp, new(Class="Realisation", Value=as.double(sample[i]), Id=i, Weight=1/number))
#     }
#     rsample<-new(Class="RandomSample", temp)
#     return(rsample)
#   }
#   )
