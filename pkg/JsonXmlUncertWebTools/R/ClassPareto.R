setClass(
  Class="ParetoDistribution",
  representation=representation(scale="numeric", shape="numeric")
  )

setMethod(
  f="initialize",
  signature="ParetoDistribution",
  definition=function(.Object, scale, shape){
    .Object@scale<-scale
    .Object@shape<-shape
    return(.Object)
  }
  )

setGeneric(
  name="getParetoScale",
  def=function(.Object) {standardGeneric("getParetoScale")}
  )

setMethod(
  f="getParetoScale",
  signature="ParetoDistribution",
  definition=function(.Object){
    return(.Object@scale)
  }
  )

setGeneric(
  name="getParetoShape",
  def=function(.Object) {standardGeneric("getParetoShape")}
  )

setMethod(
  f="getParetoShape",
  signature="ParetoDistribution",
  definition=function(.Object){
    return(.Object@shape)
  }
  )

setGeneric(
  name="setParetoScale<-",
  def=function(.Object,value){standardGeneric("setParetoScale<-")}
  )

setReplaceMethod(
  f="setParetoScale",
  signature="ParetoDistribution",
  definition=function(.Object,value){
    .Object@scale<-value
    return(.Object)
  }
  )

setGeneric(
  name="setParetoShape<-",
  def=function(.Object,value){standardGeneric("setParetoShape<-")}
  )

setReplaceMethod(
  f="setParetoShape",
  signature="ParetoDistribution",
  definition=function(.Object,value){
    .Object@shape<-value
    return(.Object)
  }
  )

setGeneric(
  name="getParetoSamples",
  def=function(.Object,number) {standardGeneric("getParetoSamples")}
  )

setMethod(
  f="getParetoSamples",
  signature="ParetoDistribution",
  definition=function(.Object, number){
    temp<-runif(number, 0, 1)
    sample<-c()
    for(i in 1:number){
      s<-.Object@scale/(temp[i]^(1/.Object@shape))
      sample<-c(sample, new(Class="Realisation", Value=as.double(s), Id=i, Weight=1/number))
    }
    rsample<-new(Class="RandomSample", sample)
    return(rsample)
  }
  )

setGeneric(
  name="getParetoMean",
  def=function(.Object){standardGeneric("getParetoMean")}
  )

setMethod(
  f="getParetoMean",
  signature="ParetoDistribution",
  definition=function(.Object)
  {
    s<-.Object@scale
    t<-.Object@shape
    if(t>1)
    {
      return(t*s/(t-1))
    }
    else
    {
      return(NULL)
    }
  }
  )

setGeneric(
  name="getParetoVariance",
  def=function(.Object){standardGeneric("getParetoVariance")}
  )

setMethod(
  f="getParetoVariance",
  signature="ParetoDistribution",
  definition=function(.Object)
  {
    s<-.Object@scale
    t<-.Object@shape
    if(t>2)
    {
      return(((s^2)*t)/(((t-1)^2)*(t-2)))
    }
    else
    {
      return(NULL)
    }
  }
  )

setGeneric(
  name="getParetoStandardDeviation",
  def=function(.Object){standardGeneric("getParetoStandardDeviation")}
  )

setMethod(
  f="getParetoStandardDeviation",
  signature="ParetoDistribution",
  definition=function(.Object)
  {
    var<-getParetoVariance(.Object)
    if(!is.null(var))
    {
      return(sqrt(var))  
    }
    else
    {
      return(NULL)
    }
  }
  )

setGeneric(
  name="getParetoMode",
  def=function(.Object){standardGeneric("getParetoMode")}
  )

setMethod(
  f="getParetoMode",
  signature="ParetoDistribution",
  definition=function(.Object)
  {
    return(.Object@scale)  
  }
  )

setGeneric(
  name="getParetoMedian",
  def=function(.Object){standardGeneric("getParetoMedian")}
  )

setMethod(
  f="getParetoMedian",
  signature="ParetoDistribution",
  definition=function(.Object)
  {
    s<-.Object@scale
    t<-.Object@shape
    return(s*(2^(1/t)))  
  }
  )

setGeneric(
  name="getParetoSkewness",
  def=function(.Object){standardGeneric("getParetoSkewness")}
  )

setMethod(
  f="getParetoSkewness",
  signature="ParetoDistribution",
  definition=function(.Object)
  {
    t<-.Object@shape
    if(t>3)
    {
      return(2*(t+1)*sqrt((t-2)/t)/(t-3))
    }
    else
    {
      return(NULL)
    }
  }
  )

setGeneric(
  name="getParetoKurtosis",
  def=function(.Object){standardGeneric("getParetoKurtosis")}
  )

setMethod(
  f="getParetoKurtosis",
  signature="ParetoDistribution",
  definition=function(.Object)
  {
    t<-.Object@shape
    if(t>4)
    {
      return((6*(t^3+t^2-6*t-2))/(t*(t-3)*(t-4)))
    }
    else
    {
      return(NULL)
    }
  }
  )