setClass(
  Class="NegativeBinomialDistribution",
  representation=representation(numberOfFailures="numeric", probability="numeric")
  )

setMethod(
  f="initialize",
  signature="NegativeBinomialDistribution",
  definition=function(.Object, failures, probability){
    .Object@numberOfFailures<-failures
    .Object@probability<-probability
    return(.Object)
  }
  )

setGeneric(
  name="getNegBinomNumberOfFailures",
  def=function(.Object) {standardGeneric("getNegBinomNumberOfFailures")}
  )

setMethod(
  f="getNegBinomNumberOfFailures",
  signature="NegativeBinomialDistribution",
  definition=function(.Object){
    return(.Object@numberOfFailures)
  }
  )

setGeneric(
  name="getNegBinomProbability",
  def=function(.Object) {standardGeneric("getNegBinomProbability")}
  )

setMethod(
  f="getNegBinomProbability",
  signature="NegativeBinomialDistribution",
  definition=function(.Object){
    return(.Object@probability)
  }
  )

setGeneric(
  name="setNegBinomNumberOfFailures<-",
  def=function(.Object,value){standardGeneric("setNegBinomNumberOfFailures<-")}
  )

setReplaceMethod(
  f="setNegBinomNumberOfFailures",
  signature="NegativeBinomialDistribution",
  definition=function(.Object,value){
    .Object@numberOfFailures<-value
    return(.Object)
  }
  )

setGeneric(
  name="setNegBinomProbability<-",
  def=function(.Object,value){standardGeneric("setNegBinomProbability<-")}
  )

setReplaceMethod(
  f="setNegBinomProbability",
  signature="NegativeBinomialDistribution",
  definition=function(.Object,value){
    .Object@probability<-value
    return(.Object)
  }
  )

setGeneric(
  name="getNegBinomSamples",
  def=function(.Object,number) {standardGeneric("getNegBinomSamples")}
  )

setMethod(
  f="getNegBinomSamples",
  signature="NegativeBinomialDistribution",
  definition=function(.Object, number){
    sample<-rnbinom(number, .Object@numberOfFailures, .Object@probability)
    temp<-c()
    for(i in 1:length(sample)){
      temp<-c(temp, new(Class="Realisation", Value=as.double(sample[i]), Id=i, Weight=1/number))
    }
    rsample<-new(Class="RandomSample", temp)
    return(rsample)
  }
  )

setGeneric(
  name="getNegBinomMean",
  def=function(.Object){standardGeneric("getNegBinomMean")}
  )

setMethod(
  f="getNegBinomMean",
  signature="NegativeBinomialDistribution",
  definition=function(.Object)
  {
    r<-.Object@numberOfFailures
    p<-.Object@probability
    return(p*r/(1-p))  
  }
  )

setGeneric(
  name="getNegBinomVariance",
  def=function(.Object){standardGeneric("getNegBinomVariance")}
  )

setMethod(
  f="getNegBinomVariance",
  signature="NegativeBinomialDistribution",
  definition=function(.Object)
  {
    r<-.Object@numberOfFailures
    p<-.Object@probability
    return(p*r/((1-p)^2))  
  }
  )

setGeneric(
  name="getNegBinomStandardDeviation",
  def=function(.Object){standardGeneric("getNegBinomStandardDeviation")}
  )

setMethod(
  f="getNegBinomStandardDeviation",
  signature="NegativeBinomialDistribution",
  definition=function(.Object)
  {
    var<-getNegativeBinomialVariance(.Object)
    return(sqrt(var))  
  }
  )

setGeneric(
  name="getNegBinomMode",
  def=function(.Object){standardGeneric("getNegBinomMode")}
  )

setMethod(
  f="getNegBinomMode",
  signature="NegativeBinomialDistribution",
  definition=function(.Object)
  {
    r<-.Object@numberOfFailures
    p<-.Object@probability
    if(r>1)
    {
      return(floor((p*(r-1))/(1-p)))
    }
    else
    {
      return(0)  
    }
  }
  )

setGeneric(
  name="getNegBinomSkewness",
  def=function(.Object){standardGeneric("getNegBinomSkewness")}
  )

setMethod(
  f="getNegBinomSkewness",
  signature="NegativeBinomialDistribution",
  definition=function(.Object)
  {
    r<-.Object@numberOfFailures
    p<-.Object@probability
    return((p+1)/sqrt(p*r))  
  }
  )

setGeneric(
  name="getNegBinomKurtosis",
  def=function(.Object){standardGeneric("getNegBinomKurtosis")}
  )

setMethod(
  f="getNegBinomKurtosis",
  signature="NegativeBinomialDistribution",
  definition=function(.Object)
  {
    r<-.Object@numberOfFailures
    p<-.Object@probability
    return((6/r)+(((1-p)^2)/(p*r)))  
  }
  )