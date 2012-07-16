setClass(
  Class="BinomialDistribution",
  representation=representation(numberOfTrials="numeric", probabilityOfSuccess="numeric")
  )

setMethod(
  f="initialize",
  signature="BinomialDistribution",
  definition=function(.Object, trials, probability){
    .Object@numberOfTrials<-trials
    .Object@probabilityOfSuccess<-probability
    return(.Object)
  }
  )

setGeneric(
  name="getBinomialNumberOfTrials",
  def=function(.Object) {standardGeneric("getBinomialNumberOfTrials")}
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
  def=function(.Object) {standardGeneric("getBinomialProbabilityOfSuccess")}
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
  def=function(.Object,value){standardGeneric("setBinomialNumberOfTrials<-")}
  )

setReplaceMethod(
  f="setBinomialNumberOfTrials",
  signature="BinomialDistribution",
  definition=function(.Object,value){
    .Object@numberOfTrials<-value
    return(.Object)
  }
  )

setGeneric(
  name="setBinomialSuccessProbability<-",
  def=function(.Object,value){standardGeneric("setBinomialSuccessProbability<-")}
  )

setReplaceMethod(
  f="setBinomialSuccessProbability",
  signature="BinomialDistribution",
  definition=function(.Object,value){
    .Object@probabilityOfSuccess<-value
    return(.Object)
  }
  )

setGeneric(
  name="getBinomialSamples",
  def=function(.Object,number) {standardGeneric("getBinomialSamples")}
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

setGeneric(
  name="getBinomialMean",
  def=function(.Object){standardGeneric("getBinomialMean")}
  )

setMethod(
  f="getBinomialMean",
  signature="BinomialDistribution",
  definition=function(.Object)
  {
    return(.Object@numberOfTrials*.Object@probabilityOfSuccess)  
  }
  )

setGeneric(
  name="getBinomialVariance",
  def=function(.Object){standardGeneric("getBinomialVariance")}
  )

setMethod(
  f="getBinomialVariance",
  signature="BinomialDistribution",
  definition=function(.Object)
  {
    return(.Object@numberOfTrials*.Object@probabilityOfSuccess*(1-.Object@probabilityOfSuccess))  
  }
  )

setGeneric(
  name="getBinomialStandardDeviation",
  def=function(.Object){standardGeneric("getBinomialStandardDeviation")}
  )

setMethod(
  f="getBinomialStandardDeviation",
  signature="BinomialDistribution",
  definition=function(.Object)
  {
    var<-getBinomialVariance(.Object)
    return(sqrt(var))
  }
  )

setGeneric(
  name="getBinomialMode",
  def=function(.Object){standardGeneric("getBinomialMode")}
  )

setMethod(
  f="getBinomialMode",
  signature="BinomialDistribution",
  definition=function(.Object)
  {
    t<-(.Object@numberOfTrials+1)*.Object@probabilityOfSuccess
    if(t-floor(t)==0 & t>0 & t<=.Object@numberOfTrials)
    {
      return(c(t, t-1))
    }
    else if(.Object@probabilityOfSuccess==1)
    {
      return(.Object@numberOfTrials)
    }
    else
    {
      return(floor(t))
    }
  }
  )

setGeneric(
  name="getBinomialSkewness",
  def=function(.Object){standardGeneric("getBinomialSkewness")}
  )

setMethod(
  f="getBinomialSkewness",
  signature="BinomialDistribution",
  definition=function(.Object)
  {
    d<-sqrt(.Object@numberOfTrials*.Object@probabilityOfSuccess*(1-.Object@probabilityOfSuccess))  
    return((1-2*.Object@probabilityOfSuccess)/d)
  }
  )

setGeneric(
  name="getBinomialKurtosis",
  def=function(.Object){standardGeneric("getBinomialKurtosis")}
  )

setMethod(
  f="getBinomialKurtosis",
  signature="BinomialDistribution",
  definition=function(.Object)
  {
    d<-.Object@probabilityOfSuccess*(1-.Object@probabilityOfSuccess)  
    return((1-6*d)/(d*.Object@numberOfTrials))
  }
  )
