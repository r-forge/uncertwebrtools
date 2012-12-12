setClass(
  Class="UniformDistribution",
  representation=representation(minimum="numeric", maximum="numeric")
  )

setMethod(
  f="initialize",
  signature="UniformDistribution",
  definition=function(.Object, min, max){
    .Object@minimum<-min
    .Object@maximum<-max
    return(.Object)
  }
  )

setGeneric(
  name="getUniformMinimum",
  def=function(.Object) {standardGeneric("getUniformMinimum")}
  )

setMethod(
  f="getUniformMinimum",
  signature="UniformDistribution",
  definition=function(.Object){
    return(.Object@minimum)
  }
  )

setGeneric(
  name="getUniformMaximum",
  def=function(.Object) {standardGeneric("getUniformMaximum")}
  )

setMethod(
  f="getUniformMaximum",
  signature="UniformDistribution",
  definition=function(.Object){
    return(.Object@maximum)
  }
  )

setGeneric(
  name="setUniformMinimum<-",
  def=function(.Object,value){standardGeneric("setUniformMinimum<-")}
  )

setReplaceMethod(
  f="setUniformMinimum",
  signature="UniformDistribution",
  definition=function(.Object,value){
    .Object@minimum<-value
    return(.Object)
  }
  )

setGeneric(
  name="setUniformMaximum<-",
  def=function(.Object,value){standardGeneric("setUniformMaximum<-")}
  )

setReplaceMethod(
  f="setUniformMaximum",
  signature="UniformDistribution",
  definition=function(.Object,value){
    .Object@maximum<-value
    return(.Object)
  }
  )

setGeneric(
  name="getUniformSamples",
  def=function(.Object,number) {standardGeneric("getUniformSamples")}
  )

setMethod(
  f="getUniformSamples",
  signature="UniformDistribution",
  definition=function(.Object, number){
    sample<-runif(number, min=.Object@minimum, max=.Object@maximum)
    temp<-c()
    for(i in 1:length(sample)){
      temp<-c(temp, new(Class="Realisation", Value=as.double(sample[i]), Id=i, Weight=1/number))
    }
    rsample<-new(Class="RandomSample", temp)
    return(rsample)
  }
  )

setGeneric(
  name="getUniformMean",
  def=function(.Object){standardGeneric("getUniformMean")}
  )

setMethod(
  f="getUniformMean",
  signature="UniformDistribution",
  definition=function(.Object)
  {
    return((.Object@maximum+.Object@minimum)/2)  
  }
  )

setGeneric(
  name="getUniformVariance",
  def=function(.Object){standardGeneric("getUniformVariance")}
  )

setMethod(
  f="getUniformVariance",
  signature="UniformDistribution",
  definition=function(.Object)
  {
    return(((.Object@maximum-.Object@minimum)^2)/12)  
  }
  )

setGeneric(
  name="getUniformStandardDeviation",
  def=function(.Object){standardGeneric("getUniformStandardDeviation")}
  )

setMethod(
  f="getUniformStandardDeviation",
  signature="UniformDistribution",
  definition=function(.Object)
  {
    var<-getUniformVariance(.Object)
    return(sqrt(var))  
  }
  )

setGeneric(
  name="getUniformMode",
  def=function(.Object){standardGeneric("getUniformMode")}
  )

setMethod(
  f="getUniformMode",
  signature="UniformDistribution",
  definition=function(.Object)
  {
    return(runif(1,.Object@minimum, .Object@maximum))  
  }
  )

setGeneric(
  name="getUniformMedian",
  def=function(.Object){standardGeneric("getUniformMedian")}
  )

setMethod(
  f="getUniformMedian",
  signature="UniformDistribution",
  definition=function(.Object)
  {
    return((.Object@maximum+.Object@minimum)/2)  
  }
  )

setGeneric(
  name="getUniformSkewness",
  def=function(.Object){standardGeneric("getUniformSkewness")}
  )

setMethod(
  f="getUniformSkewness",
  signature="UniformDistribution",
  definition=function(.Object)
  {
    return(0)  
  }
  )

setGeneric(
  name="getUniformKurtosis",
  def=function(.Object){standardGeneric("getUniformKurtosis")}
  )

setMethod(
  f="getUniformKurtosis",
  signature="UniformDistribution",
  definition=function(.Object)
  {
    return(-6/5)  
  }
  )