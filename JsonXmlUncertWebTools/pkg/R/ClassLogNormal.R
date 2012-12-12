setClass(
  Class="LogNormalDistribution",
  representation=representation(logScale="numeric", shape="numeric")
  )

setMethod(
  f="initialize",
  signature="LogNormalDistribution",
  definition=function(.Object, scale, shape){
    .Object@logScale<-scale
    .Object@shape<-shape
    return(.Object)
  }
  )

setGeneric(
  name="getLogNormalLogScale",
  def=function(.Object) {standardGeneric("getLogNormalLogScale")}
  )

setMethod(
  f="getLogNormalLogScale",
  signature="LogNormalDistribution",
  definition=function(.Object){
    return(.Object@logScale)
  }
  )

setGeneric(
  name="getLogNormalShape",
  def=function(.Object) {standardGeneric("getLogNormalShape")}
  )

setMethod(
  f="getLogNormalShape",
  signature="LogNormalDistribution",
  definition=function(.Object){
    return(.Object@shape)
  }
  )

setGeneric(
  name="setLogNormalLogScale<-",
  def=function(.Object,value){standardGeneric("setLogNormalLogScale<-")}
  )

setReplaceMethod(
  f="setLogNormalLogScale",
  signature="LogNormalDistribution",
  definition=function(.Object,value){
    .Object@logScale<-value
    return(.Object)
  }
  )

setGeneric(
  name="setLogNormalShape<-",
  def=function(.Object,value){standardGeneric("setLogNormalShape<-")}
  )

setReplaceMethod(
  f="setLogNormalShape",
  signature="LogNormalDistribution",
  definition=function(.Object,value){
    .Object@shape<-value
    return(.Object)
  }
  )

setGeneric(
  name="getLogNormalSamples",
  def=function(.Object,number) {standardGeneric("getLogNormalSamples")}
  )

setMethod(
  f="getLogNormalSamples",
  signature="LogNormalDistribution",
  definition=function(.Object, number){
    sample<-rlnorm(number, meanlog=.Object@logScale, sdlog=sqrt(.Object@shape))
    temp<-c()
    for(i in 1:length(sample)){
      temp<-c(temp, new(Class="Realisation", Value=as.double(sample[i]), Id=i, Weight=1/number))
    }
    rsample<-new(Class="RandomSample", temp)
    return(rsample)
  }
  )

setGeneric(
  name="getLogNormalMean",
  def=function(.Object){standardGeneric("getLogNormalMean")}
  )

setMethod(
  f="getLogNormalMean",
  signature="LogNormalDistribution",
  definition=function(.Object)
  {
    return(exp(.Object@logScale+(.Object@shape/2)))  
  }
  )

setGeneric(
  name="getLogNormalVariance",
  def=function(.Object){standardGeneric("getLogNormalVariance")}
  )

setMethod(
  f="getLogNormalVariance",
  signature="LogNormalDistribution",
  definition=function(.Object)
  {
    s<-.Object@shape
    l<-.Object@logScale
    return((exp(s)-1)*exp(2*l+s))  
  }
  )

setGeneric(
  name="getLogNormalStandardDeviation",
  def=function(.Object){standardGeneric("getLogNormalStandardDeviation")}
  )

setMethod(
  f="getLogNormalStandardDeviation",
  signature="LogNormalDistribution",
  definition=function(.Object)
  {
    var<-getLogNormalVariance(.Object)
    return(sqrt(var))  
  }
  )

setGeneric(
  name="getLogNormalMode",
  def=function(.Object){standardGeneric("getLogNormalMode")}
  )

setMethod(
  f="getLogNormalMode",
  signature="LogNormalDistribution",
  definition=function(.Object)
  {
    return(exp(.Object@logScale-.Object@shape))  
  }
  )

setGeneric(
  name="getLogNormalMedian",
  def=function(.Object){standardGeneric("getLogNormalMedian")}
  )

setMethod(
  f="getLogNormalMedian",
  signature="LogNormalDistribution",
  definition=function(.Object)
  {
    return(exp(.Object@logScale))  
  }
  )

setGeneric(
  name="getLogNormalSkewness",
  def=function(.Object){standardGeneric("getLogNormalSkewness")}
  )

setMethod(
  f="getLogNormalSkewness",
  signature="LogNormalDistribution",
  definition=function(.Object)
  {
    s<-.Object@shape
    return((exp(s)+2)*sqrt(exp(s)-1))  
  }
  )

setGeneric(
  name="getLogNormalKurtosis",
  def=function(.Object){standardGeneric("getLogNormalKurtosis")}
  )

setMethod(
  f="getLogNormalKurtosis",
  signature="LogNormalDistribution",
  definition=function(.Object)
  {
    s<-.Object@shape
    return(exp(4*s)+(2*exp(3*s))+(3*exp(2*s))-6)  
  }
  )