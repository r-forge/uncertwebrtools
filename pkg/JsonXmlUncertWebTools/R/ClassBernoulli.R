setClass(
  Class="BernoulliDistribution",
  representation=representation(probabilities="numeric")
  )

setMethod(
  f="initialize",
  signature="BernoulliDistribution",
  definition=function(.Object, probability){
    .Object@probabilities<-probability 
    return(.Object)
  }
  )

setGeneric(
  name="getBernoulliProbabilities",
  def=function(.Object) {standardGeneric("getBernoulliProbabilities")}
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
  def=function(.Object,value){standardGeneric("setBernoulliProbabilities<-")}
  )

setReplaceMethod(
  f="setBernoulliProbabilities",
  signature="BernoulliDistribution",
  definition=function(.Object,value){
    .Object@probabilities<-value
    return(.Object)
  }
  )

setGeneric(
  name="getBernoulliSamples",
  def=function(.Object,number) {standardGeneric("getBernoulliSamples")}
  )

setMethod(
  f="getBernoulliSamples",
  signature="BernoulliDistribution",
  definition=function(.Object, number){
    sample<-c()
    for(i in 1:number){
      p<-runif(1,0,1)
      if(p<=.Object@probabilities)
      {
        s<-1
      }
      else
      {
        s<-0
      }
      sample<-c(sample, new(Class="Realisation", Value=as.double(s), Id=i, Weight=1/number))
    }
    rsample<-new(Class="RandomSample", sample)
    return(rsample)
  }
  )


setGeneric(
  name="getBernoulliMean",
  def=function(.Object){standardGeneric("getBernoulliMean")}
  )

setMethod(
  f="getBernoulliMean",
  signature="BernoulliDistribution",
  definition=function(.Object)
  {
    return(.Object@probabilities)  
  }
  )

setGeneric(
  name="getBernoulliVariance",
  def=function(.Object){standardGeneric("getBernoulliVariance")}
  )

setMethod(
  f="getBernoulliVariance",
  signature="BernoulliDistribution",
  definition=function(.Object)
  {
    p<-.Object@probabilities
    q<-1-p
    return(p*q)
  }
  )

setGeneric(
  name="getBernoulliStandardDeviation",
  def=function(.Object){standardGeneric("getBernoulliStandardDeviation")}
  )

setMethod(
  f="getBernoulliStandardDeviation",
  signature="BernoulliDistribution",
  definition=function(.Object)
  {
    var<-getBernoulliVariance(.Object)
    return(sqrt(var))  
  }
  )

setGeneric(
  name="getBernoulliMode",
  def=function(.Object){standardGeneric("getBernoulliMode")}
  )

setMethod(
  f="getBernoulliMode",
  signature="BernoulliDistribution",
  definition=function(.Object)
  {
    p<-.Object@probabilities
    q<-1-p
    if(q>p)
    {
      return(0)
    }
    else if(q==p)
    {
      return(list(0,1))
    }
    else
    {
      return(1)
    }
  }
  )

setGeneric(
  name="getBernoulliMedian",
  def=function(.Object){standardGeneric("getBernoulliMedian")}
  )

setMethod(
  f="getBernoulliMedian",
  signature="BernoulliDistribution",
  definition=function(.Object)
  {
    p<-.Object@probabilities
    q<-1-p
    if(q>p)
    {
      return(0)
    }
    else if(q==p)
    {
      return(0.5)
    }
    else
    {
      return(1)
    }
  }
  )

setGeneric(
  name="getBernoulliSkewness",
  def=function(.Object){standardGeneric("getBernoulliSkewness")}
  )

setMethod(
  f="getBernoulliSkewness",
  signature="BernoulliDistribution",
  definition=function(.Object)
  {
    p<-.Object@probabilities
    q<-1-p
    return((q-p)/sqrt(p*q))  
  }
  )

setGeneric(
  name="getBernoulliKurtosis",
  def=function(.Object){standardGeneric("getBernoulliKurtosis")}
  )

setMethod(
  f="getBernoulliKurtosis",
  signature="BernoulliDistribution",
  definition=function(.Object)
  {
    p<-.Object@probabilities
    q<-1-p
    return((1-6*p*q)/(p*q))  
  }
  )
