setClass(
  Class="MultinomialDistribution",
  representation=representation(numberOfTrials="numeric",
                                probabilityOfSuccess="vector")
  )

setMethod(
  f="initialize",
  signature="MultinomialDistribution",
  definition=function(.Object, numberOfTrials, probabilityOfSuccess){
    .Object@numberOfTrials<-numberOfTrials
    .Object@probabilityOfSuccess<-probabilityOfSuccess
    return(.Object)
  }
  )

setGeneric(
  name="getMultinomNumberOfTrials",
  def=function(.Object) {standardGeneric("getMultinomNumberOfTrials")}
  )

setMethod(
  f="getMultinomNumberOfTrials",
  signature="MultinomialDistribution",
  definition=function(.Object){
    return(.Object@numberOfTrials)
  }
  )

setGeneric(
  name="getMultinomSuccessProbability",
  def=function(.Object) {standardGeneric("getMultinomSuccessProbability")}
  )

setMethod(
  f="getMultinomSuccessProbability",
  signature="MultinomialDistribution",
  definition=function(.Object){
    return(.Object@probabilityOfSuccess)
  }
  )

setGeneric(
  name="setMultinomNumberOfTrials<-",
  def=function(.Object,value){standardGeneric("setMultinomNumberOfTrials<-")}
  )

setReplaceMethod(
  f="setMultinomNumberOfTrials",
  signature="MultinomialDistribution",
  definition=function(.Object,value){
    .Object@numberOfTrials<-value
    return(.Object)
  }
  )

setGeneric(
  name="setMultinomSuccessProbability<-",
  def=function(.Object,value){standardGeneric("setMultinomSuccessProbability<-")}
  )

setReplaceMethod(
  f="setMultinomSuccessProbability",
  signature="MultinomialDistribution",
  definition=function(.Object,value){
    .Object@probabilityOfSuccess<-value
    return(.Object)
  }
  )

setGeneric(
  name="getMultinomSamples",
  def=function(.Object,number) {standardGeneric("getMultinomSamples")}
  )

setMethod(
  f="getMultinomSamples",
  signature="MultinomialDistribution",
  definition=function(.Object, number){
    sample<-rmultinom(number, .Object@numberOfTrials*length(.Object@probabilityOfSuccess),
                      .Object@probabilityOfSuccess)
    temp<-list()
    for(i in 1:number)
    {
      a<-c()
      for(j in 1:length(.Object@probabilityOfSuccess))
      {
          a<-c(a,new(Class="Realisation", Value=as.double(sample[j,i]),
                         Id=i, Weight=1/number))
      }
      temp[[i]]<-a
    }
    rsample<-new(Class="RandomSample", temp)
    return(rsample)
  }
  )

setGeneric(
  name="getMultinomMean",
  def=function(.Object){standardGeneric("getMultinomMean")}
  )

setMethod(
  f="getMultinomMean",
  signature="MultinomialDistribution",
  definition=function(.Object)
  {
    mean<-c()
    for(i in 1:length(.Object@probabilityOfSuccess))
    {
      mean[[i]]<-.Object@numberOfTrials*.Object@probabilityOfSuccess[i]
    }
    return(mean)  
  }
  )

setGeneric(
  name="getMultinomVariance",
  def=function(.Object){standardGeneric("getMultinomVariance")}
  )

setMethod(
  f="getMultinomVariance",
  signature="MultinomialDistribution",
  definition=function(.Object)
  {
    var<-c()
    for(i in 1:length(.Object@probabilityOfSuccess))
    {
      p<-.Object@probabilityOfSuccess[[i]]
      var[[i]]<-.Object@numberOfTrials*p*(1-p)
    }
    return(var)
  }
  )

setGeneric(
  name="getMultinomStandardDeviation",
  def=function(.Object){standardGeneric("getMultinomStandardDeviation")}
  )

setMethod(
  f="getMultinomStandardDeviation",
  signature="MultinomialDistribution",
  definition=function(.Object)
  {
    var<-getMultinomialVariance(.Object)
    sd<-c()
    for(i in 1:length(var))
    {
      sd[[i]]<-sqrt(var[[i]])
    }
    return(sd)  
  }
  )