setClass(
  Class="NormalDistribution",
  representation=representation(mean="numeric", variance="numeric")
  )

setMethod(
  f="initialize",
  signature="NormalDistribution",
  definition=function(.Object, mean, variance){
    .Object@mean<-mean
    .Object@variance<-variance
    validObject(.Object)
    return(.Object)
  }
  )

setGeneric(
  name="getNormalMean",
  def=function(.Object) {standardGeneric("getNormalMean")}
  )

setMethod(
  f="getNormalMean",
  signature="NormalDistribution",
  definition=function(.Object){
    return(.Object@mean)
  }
  )

setGeneric(
  name="getNormalVariance",
  def=function(.Object) {standardGeneric("getNormalVariance")}
  )

setMethod(
  f="getNormalVariance",
  signature="NormalDistribution",
  definition=function(.Object){
    return(.Object@variance)
  }
  )

setGeneric(
  name="setNormalMean<-",
  def=function(.Object,value){standardGeneric("setNormalMean<-")}
  )

setReplaceMethod(
  f="setNormalMean",
  signature="NormalDistribution",
  definition=function(.Object,value){
    .Object@mean<-value
    return(.Object)
  }
  )

setGeneric(
  name="setNormalVariance<-",
  def=function(.Object,value){standardGeneric("setNormalVariance<-")}
  )

setReplaceMethod(
  f="setNormalVariance",
  signature="NormalDistribution",
  definition=function(.Object,value){
    .Object@variance<-value
    return(.Object)
  }
  )

setGeneric(
  name="getNormalSamples",
  def=function(.Object,number) {standardGeneric("getNormalSamples")}
  )

setMethod(
  f="getNormalSamples",
  signature="NormalDistribution",
  definition=function(.Object, number){
    sample<-rnorm(number, .Object@mean, sqrt(.Object@variance))
    temp<-c()
    for(i in 1:length(sample)){
      temp<-c(temp, new(Class="Realisation", Value=as.double(sample[i]), Id=i, Weight=1/number))
    }
    rsample<-new(Class="RandomSample", temp)
    return(rsample)
  }
  )

setGeneric(
  name="writeToXML",
  def=function(.Object, file){standardGeneric("writeToXML")}
  )

setMethod(
  f="writeToXML",
  signature="NormalDistribution",
  definition=function(.Object, file){
    library("XML")
    data<-xmlNode("un:NormalDistribution", 
                  attrs=c(xmlns="http://www.uncertml.org/2.0"), 
                  xmlNode("un:mean", .Object@mean), 
                  xmlNode("un:variance", .Object@variance))
    write(toString(data), file)    
    detach("package:XML")
  }
  )

setGeneric(
  name="getNormalStandardDeviation",
  def=function(.Object){standardGeneric("getNormalStandardDeviation")}
  )

setMethod(
  f="getNormalStandardDeviation",
  signature="NormalDistribution",
  definition=function(.Object)
  {
    var<-getNormalVariance(.Object)
    return(sqrt(var))  
  }
  )

setGeneric(
  name="getNormalMode",
  def=function(.Object){standardGeneric("getNormalMode")}
  )

setMethod(
  f="getNormalMode",
  signature="NormalDistribution",
  definition=function(.Object)
  {
    return(.Object@mean)  
  }
  )

setGeneric(
  name="getNormalMedian",
  def=function(.Object){standardGeneric("getNormalMedian")}
  )

setMethod(
  f="getNormalMedian",
  signature="NormalDistribution",
  definition=function(.Object)
  {
    return(.Object@mean)  
  }
  )

setGeneric(
  name="getNormalSkewness",
  def=function(.Object){standardGeneric("getNormalSkewness")}
  )

setMethod(
  f="getNormalSkewness",
  signature="NormalDistribution",
  definition=function(.Object)
  {
    return(0)  
  }
  )

setGeneric(
  name="getNormalKurtosis",
  def=function(.Object){standardGeneric("getNormalKurtosis")}
  )

setMethod(
  f="getNormalKurtosis",
  signature="NormalDistribution",
  definition=function(.Object)
  {
    return(0)  
  }
  )