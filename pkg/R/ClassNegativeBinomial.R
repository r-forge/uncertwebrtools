setClass(
  Class="NegativeBinomialDistribution",
  representation=representation(numberOfFailures="integer", probability="numeric")
  )

setMethod(
  f="initialize",
  signature="NegativeBinomialDistribution",
  definition=function(.Object, failures, probability){
    .Object@numberOfFailures<-failures
    .Object@probability<-probability
    #validObject(.Object)
    return(.Object)
  }
  )

setGeneric(
  name="getNegativeBinomialNumberOfFailures",
  def=function(.Object) {standardGeneric("getNumberOfFailures")}
  )

setMethod(
  f="getNegativeBinomialNumberOfFailures",
  signature="NegativeBinomialDistribution",
  definition=function(.Object){
    return(.Object@numberOfFailures)
  }
  )

setGeneric(
  name="getNegativeBinomialProbability",
  def=function(.Object) {standardGeneric("getProbability")}
  )

setMethod(
  f="getNegativeBinomialProbability",
  signature="NegativeBinomialDistribution",
  definition=function(.Object){
    return(.Object@probability)
  }
  )

setGeneric(
  name="setNegativeBinomialNumberOfFailures<-",
  def=function(.Object,Value){standardGeneric("setNumberOfFailures<-")}
  )

setReplaceMethod(
  f="setNegativeBinomialNumberOfFailures",
  signature="NegativeBinomialDistribution",
  definition=function(.Object,Value){
    .Object@numberOfFailures<-Value
    return(.Object)
  }
  )

setGeneric(
  name="setNegativeBinomialProbability<-",
  def=function(.Object,Value){standardGeneric("setProbability<-")}
  )

setReplaceMethod(
  f="setNegativeBinomialProbability",
  signature="NegativeBinomialDistribution",
  definition=function(.Object,Value){
    .Object@probability<-Value
    return(.Object)
  }
  )

setGeneric(
  name="getNegativeBinomialSamples",
  def=function(.Object,number) {standardGeneric("getSamples")}
  )

setMethod(
  f="getNegativeBinomialSamples",
  signature="NegativeBinomialDistribution",
  definition=function(.Object, number){
    sample<-rnbinom(number, .Object@numberOfFailures, .Object@probabilityOfSuccess)
    temp<-c()
    for(i in 1:length(sample)){
      temp<-c(temp, new(Class="Realisation", Value=as.double(sample[i]), Id=i, Weight=1/number))
    }
    rsample<-new(Class="RandomSample", temp)
    return(rsample)
  }
  )
