setClass(
  Class="ExponentialDistribution",
  representation=representation(rate="numeric")
  )

setMethod(
  f="initialize",
  signature="ExponentialDistribution",
  definition=function(.Object, rate){
    .Object@rate<-rate
    #validObject(.Object)
    return(.Object)
  }
  )

setGeneric(
  name="getExponentialRate",
  def=function(.Object) {standardGeneric("getRate")}
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
  def=function(.Object,Value){standardGeneric("setRate<-")}
  )

setReplaceMethod(
  f="setExponentialRate",
  signature="ExponentialDistribution",
  definition=function(.Object,Value){
    .Object@rate<-Value
    return(.Object)
  }
  )

setGeneric(
  name="getExponentialSamples",
  def=function(.Object,number) {standardGeneric("getSamples")}
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
