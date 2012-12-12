setClass(
  Class="FDistribution",
  representation=representation(denominator="numeric", numerator="numeric")
  )

setMethod(
  f="initialize",
  signature="FDistribution",
  definition=function(.Object, denominator, numerator){
    .Object@denominator<-denominator
    .Object@numerator<-numerator
    return(.Object)
  }
  )

setGeneric(
  name="getFDenominator",
  def=function(.Object) {standardGeneric("getFDenominator")}
  )

setMethod(
  f="getFDenominator",
  signature="FDistribution",
  definition=function(.Object){
    return(.Object@denominator)
  }
  )

setGeneric(
  name="getFNumerator",
  def=function(.Object) {standardGeneric("getFNumerator")}
  )

setMethod(
  f="getFNumerator",
  signature="FDistribution",
  definition=function(.Object){
    return(.Object@numerator)
  }
  )

setGeneric(
  name="setFDenominator<-",
  def=function(.Object,value){standardGeneric("setFDenominator<-")}
  )

setReplaceMethod(
  f="setFDenominator",
  signature="FDistribution",
  definition=function(.Object,value){
    .Object@denominator<-value
    return(.Object)
  }
  )

setGeneric(
  name="setFNumerator<-",
  def=function(.Object,value){standardGeneric("setFNumerator<-")}
  )

setReplaceMethod(
  f="setFNumerator",
  signature="FDistribution",
  definition=function(.Object,value){
    .Object@numerator<-value
    return(.Object)
  }
  )

setGeneric(
  name="getFSamples",
  def=function(.Object,number) {standardGeneric("getFSamples")}
  )

setMethod(
  f="getFSamples",
  signature="FDistribution",
  definition=function(.Object, number){
    sample<-rf(number, .Object@denominator, .Object@numerator)
    temp<-c()
    for(i in 1:length(sample)){
      temp<-c(temp, new(Class="Realisation", Value=as.double(sample[i]), Id=i, Weight=1/number))
    }
    rsample<-new(Class="RandomSample", temp)
    return(rsample)
  }
  )

setGeneric(
  name="getFMean",
  def=function(.Object){standardGeneric("getFMean")}
  )

setMethod(
  f="getFMean",
  signature="FDistribution",
  definition=function(.Object)
  {
    if(.Object@denominator>2)
    {
      return(.Object@denominator/(.Object@denominator-2))
    }
    else
    {
      return(NULL)
    }
  }
  )

setGeneric(
  name="getFVariance",
  def=function(.Object){standardGeneric("getFVariance")}
  )

setMethod(
  f="getFVariance",
  signature="FDistribution",
  definition=function(.Object)
  {
    d<-.Object@denominator
    n<-.Object@numerator
    if(d>4)
    {
      return((2*(d^2)*(n+d-2))/(n*((d-2)^2)*(d-4)))
    }
    else
    {
      return(NULL)
    }
  }
  )

setGeneric(
  name="getFStandardDeviation",
  def=function(.Object){standardGeneric("getFStandardDeviation")}
  )

setMethod(
  f="getFStandardDeviation",
  signature="FDistribution",
  definition=function(.Object)
  {
    var<-getFVariance(.Object)
    return(sqrt(var))  
  }
  )

setGeneric(
  name="getFMode",
  def=function(.Object){standardGeneric("getFMode")}
  )

setMethod(
  f="getFMode",
  signature="FDistribution",
  definition=function(.Object)
  {
    d<-.Object@denominator
    n<-.Object@numerator
    if(n>2)
    {
      return(((n-2)*d)/((d+2)*n))
    }
    else
    {
      return(NULL)
    }
  }
  )

setGeneric(
  name="getFSkewness",
  def=function(.Object){standardGeneric("getFSkewness")}
  )

setMethod(
  f="getFSkewness",
  signature="FDistribution",
  definition=function(.Object)
  {
    d<-.Object@denominator
    n<-.Object@numerator
    if(d>6)
    {
      return((sqrt(8*(d-4))*(2*n+d-2))/((d-6)*sqrt((n+d-2)*n)))  
    }
    else
    {
      return(NULL)
    }
  }
  )

setGeneric(
  name="getFKurtosis",
  def=function(.Object){standardGeneric("getFKurtosis")}
  )

setMethod(
  f="getFKurtosis",
  signature="FDistribution",
  definition=function(.Object)
  {
    d<-.Object@denominator
    n<-.Object@numerator
    if(d>8)
    {
      return((12*(((n+d-2)*(5*d-22)*n)+((d-4)*((d-2)^2))))/(n*(d-6)*(d-8)*(n+d-2)))  
    }
    else
    {
      return(NULL)
    }
  }
  )