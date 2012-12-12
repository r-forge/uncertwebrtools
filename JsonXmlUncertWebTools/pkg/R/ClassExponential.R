setClass(
  Class="ExponentialDistribution",
  representation=representation(rate="numeric")
  )

setMethod(
  f="initialize",
  signature="ExponentialDistribution",
  definition=function(.Object, rate){
    .Object@rate<-rate
    return(.Object)
  }
  )

setGeneric(
  name="getExponentialRate",
  def=function(.Object) {standardGeneric("getExponentialRate")}
  )

setMethod(
  f="getExponentialRate",
  signature="ExponentialDistribution",
  definition=function(.Object){
    return(.Object@rate)
  }
  )

setGeneric(
  name="setExponentialRate<-",
  def=function(.Object,value){standardGeneric("setExponentialRate<-")}
  )

setReplaceMethod(
  f="setExponentialRate",
  signature="ExponentialDistribution",
  definition=function(.Object,value){
    .Object@rate<-value
    return(.Object)
  }
  )

setGeneric(
  name="getExponentialSamples",
  def=function(.Object,number) {standardGeneric("getExponentialSamples")}
  )

setMethod(
  f="getExponentialSamples",
  signature="ExponentialDistribution",
  definition=function(.Object, number){
    sample<-rexp(number, rate=.Object@rate)
    temp<-c()
    for(i in 1:length(sample)){
      temp<-c(temp, new(Class="Realisation", Value=as.double(sample[i]), Id=i, Weight=1/number))
    }
    rsample<-new(Class="RandomSample", temp)
    return(rsample)
  }
  )

setGeneric(
  name="getExponentialMean",
  def=function(.Object){standardGeneric("getExponentialMean")}
  )

setMethod(
  f="getExponentialMean",
  signature="ExponentialDistribution",
  definition=function(.Object)
  {
    return(1/.Object@rate)  
  }
  )

setGeneric(
  name="getExponentialVariance",
  def=function(.Object){standardGeneric("getExponentialVariance")}
  )

setMethod(
  f="getExponentialVariance",
  signature="ExponentialDistribution",
  definition=function(.Object)
  {
    return((1/.Object@rate)^2)  
  }
  )

setGeneric(
    name="getExponentialStandardDeviation",
    def=function(.Object){standardGeneric("getExponentialStandardDeviation")}
    )

setMethod(
  f="getExponentialStandardDeviation",
  signature="ExponentialDistribution",
  definition=function(.Object)
  {
    var<-getExponentialVariance(.Object)
    return(sqrt(var))  
  }
  )

setGeneric(
    name="getExponentialMode",
    def=function(.Object){standardGeneric("getExponentialMode")}
    )

setMethod(
  f="getExponentialMode",
  signature="ExponentialDistribution",
  definition=function(.Object)
  {
    return(0)  
  }
  )

setGeneric(
    name="getExponentialMedian",
    def=function(.Object){standardGeneric("getExponentialMedian")}
    )

setMethod(
  f="getExponentialMedian",
  signature="ExponentialDistribution",
  definition=function(.Object)
  {
    return(log(2)/.Object@rate)  
  }
  )

setGeneric(
    name="getExponentialSkewness",
    def=function(.Object){standardGeneric("getExponentialSkewness")}
    )

setMethod(
  f="getExponentialSkewness",
  signature="ExponentialDistribution",
  definition=function(.Object)
  {
    return(2)  
  }
  )

setGeneric(
  name="getExponentialKurtosis",
  def=function(.Object){standardGeneric("getExponentialKurtosis")}
  )

setMethod(
  f="getExponentialKurtosis",
  signature="ExponentialDistribution",
  definition=function(.Object)
  {
    return(6)  
  }
  )