setClass(
  Class="BinomialDistribution",
  representation=representation(numberOfTrials="integer", probabilityOfSuccess="numeric")
  )

setMethod(
  f="initialize",
  signature="BinomialDistribution",
  definition=function(.Object, trials, probability){
    .Object@numberOfTrials<-trials
    .Object@probabilityOfSuccess<-probability
    #validObject(.Object)
    return(.Object)
  }
  )

setGeneric(
  name="getBinomialNumberOfTrials",
  def=function(.Object) {standardGeneric("getNumberOfTrials")}
  )

setMethod(
  f="getBinomialNumberOfTrials",
  signature="BinomialDistribution",
  definition=function(.Object){
    return(.Object@numberOfTrials)
  }
  )

setGeneric(
  name="getBinomialProbabilityOfSuccess",
  def=function(.Object) {standardGeneric("getProbabilityOfSuccess")}
  )

setMethod(
  f="getBinomialProbabilityOfSuccess",
  signature="BinomialDistribution",
  definition=function(.Object){
    return(.Object@probabilityOfSuccess)
  }
  )

setGeneric(
  name="setBinomialNumberOfTrials<-",
  def=function(.Object,Value){standardGeneric("setNumberOfTrials<-")}
  )

setReplaceMethod(
  f="setBinomialNumberOfTrials",
  signature="BinomialDistribution",
  definition=function(.Object,Value){
    .Object@numberOfTrials<-Value
    return(.Object)
  }
  )

setGeneric(
  name="setBinomialProbabilityOfSuccess<-",
  def=function(.Object,Value){standardGeneric("setProbabilityOfSuccess<-")}
  )

setReplaceMethod(
  f="setBinomialProbabilityOfSuccess",
  signature="BinomialDistribution",
  definition=function(.Object,Value){
    .Object@probabilityOfSuccess<-Value
    return(.Object)
  }
  )

setGeneric(
  name="getBinomialSamples",
  def=function(.Object,number) {standardGeneric("getSamples")}
  )

setMethod(
  f="getBinomialSamples",
  signature="BinomialDistribution",
  definition=function(.Object, number){
    sample<-rbinom(number, .Object@numberOfTrials, .Object@probabilityOfSuccess)
    temp<-c()
    for(i in 1:length(sample)){
      temp<-c(temp, new(Class="Realisation", Value=as.double(sample[i]), Id=i, Weight=1/number))
    }
    rsample<-new(Class="RandomSample", temp)
    return(rsample)
  }
  )
