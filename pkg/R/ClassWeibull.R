setClass(
  Class="WeibullDistribution",
  representation=representation(scale="numeric", shape="numeric")
  )

setMethod(
  f="initialize",
  signature="WeibullDistribution",
  definition=function(.Object, scale, shape){
    .Object@scale<-scale
    .Object@shape<-shape
    return(.Object)
  }
  )

setGeneric(
  name="getWeibullScale",
  def=function(.Object) {standardGeneric("getWeibullScale")}
  )

setMethod(
  f="getWeibullScale",
  signature="WeibullDistribution",
  definition=function(.Object){
    return(.Object@scale)
  }
  )

setGeneric(
  name="getWeibullShape",
  def=function(.Object) {standardGeneric("getWeibullShape")}
  )

setMethod(
  f="getWeibullShape",
  signature="WeibullDistribution",
  definition=function(.Object){
    return(.Object@shape)
  }
  )

setGeneric(
  name="setWeibullScale<-",
  def=function(.Object,value){standardGeneric("setWeibullScale<-")}
  )

setReplaceMethod(
  f="setWeibullScale",
  signature="WeibullDistribution",
  definition=function(.Object,value){
    .Object@scale<-value
    return(.Object)
  }
  )

setGeneric(
  name="setWeibullShape<-",
  def=function(.Object,value){standardGeneric("setWeibullShape<-")}
  )

setReplaceMethod(
  f="setWeibullShape",
  signature="WeibullDistribution",
  definition=function(.Object,value){
    .Object@shape<-value
    return(.Object)
  }
  )

setGeneric(
  name="getWeibullSamples",
  def=function(.Object,number) {standardGeneric("getWeibullSamples")}
  )

setMethod(
  f="getWeibullSamples",
  signature="WeibullDistribution",
  definition=function(.Object, number){
    sample<-rweibull(number, .Object@shape, scale=.Object@scale)
    temp<-c()
    for(i in 1:length(sample)){
      temp<-c(temp, new(Class="Realisation", Value=as.double(sample[i]), Id=i, Weight=1/number))
    }
    rsample<-new(Class="RandomSample", temp)
    return(rsample)
  }
  )

setGeneric(
  name="getWeibullMean",
  def=function(.Object){standardGeneric("getWeibullMean")}
  )

setMethod(
  f="getWeibullMean",
  signature="WeibullDistribution",
  definition=function(.Object)
  {
    return(.Object@scale*gamma(1+(1/.Object@shape)))  
  }
  )

setGeneric(
  name="getWeibullVariance",
  def=function(.Object){standardGeneric("getWeibullVariance")}
  )

setMethod(
  f="getWeibullVariance",
  signature="WeibullDistribution",
  definition=function(.Object)
  {
    m<-getWeibullMean(.Object)
    return(((.Object@scale^2)*gamma(1+(2/.Object@shape)))-(m^2))
  }
  )

setGeneric(
  name="getWeibullStandardDeviation",
  def=function(.Object){standardGeneric("getWeibullStandardDeviation")}
  )

setMethod(
  f="getWeibullStandardDeviation",
  signature="WeibullDistribution",
  definition=function(.Object)
  {
    var<-getWeibullVariance(.Object)
    return(sqrt(var))  
  }
  )

setGeneric(
  name="getWeibullMode",
  def=function(.Object){standardGeneric("getWeibullMode")}
  )

setMethod(
  f="getWeibullMode",
  signature="WeibullDistribution",
  definition=function(.Object)
  {
    if(.Object@shape>1)
    {
      return(.Object@scale*(((.Object@shape-1)/.Object@shape)^(1/.Object@shape)))
    }
    else if(.Object@shape==1)
    {
      return(0)
    }
    else
    {
      return(NULL)
    }
  }
  )

setGeneric(
  name="getWeibullMedian",
  def=function(.Object){standardGeneric("getWeibullMedian")}
  )

setMethod(
  f="getWeibullMedian",
  signature="WeibullDistribution",
  definition=function(.Object)
  {
    return(.Object@scale*(log(2)^(1/.Object@shape)))  
  }
  )

setGeneric(
  name="getWeibullSkewness",
  def=function(.Object){standardGeneric("getWeibullSkewness")}
  )

setMethod(
  f="getWeibullSkewness",
  signature="WeibullDistribution",
  definition=function(.Object)
  {
    m<-getWeibullMean(.Object)
    s<-getWeibullStandardDeviation(.Object)
    return(((gamma(1+(3/.Object@shape))*(.Object@scale^3))-(3*m*(s^2))-(m^3))/(s^3))  
  }
  )

setGeneric(
  name="getWeibullKurtosis",
  def=function(.Object){standardGeneric("getWeibullKurtosis")}
  )

setMethod(
  f="getWeibullKurtosis",
  signature="WeibullDistribution",
  definition=function(.Object)
  {
    m<-getWeibullMean(.Object)
    s<-getWeibullStandardDeviation(.Object)
    w<-getWeibullSkewness(.Object)
    return((((gamma(1+(4/.Object@shape))*(.Object@scale^4))-(4*w*m*(s^3))-(6*(m^2)*(s^2))-(m^4))/(s^4))-3)
  }
  )