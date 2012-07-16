setClass(
  Class="StudentTDistribution",
  representation=representation(location="numeric", 
                                scale="numeric", 
                                degreesOfFreedom="integer")
  )

setMethod(
  f="initialize",
  signature="StudentTDistribution",
  definition=function(.Object, location, scale, degrees){
    .Object@location<-location
    .Object@scale<-scale
    .Object@degreesOfFreedom<-degrees
    return(.Object)
  }
  )

setGeneric(
  name="getStudentTLocation",
  def=function(.Object) {standardGeneric("getStudentTLocation")}
  )

setMethod(
  f="getStudentTLocation",
  signature="StudentTDistribution",
  definition=function(.Object){
    return(.Object@location)
  }
  )

setGeneric(
  name="getStudentTScale",
  def=function(.Object) {standardGeneric("getStudentTScale")}
  )

setMethod(
  f="getStudentTScale",
  signature="StudentTDistribution",
  definition=function(.Object){
    return(.Object@scale)
  }
  )

setGeneric(
  name="getStudentTDegreesOfFreedom",
  def=function(.Object) {standardGeneric("getStudentTDegreesOfFreedom")}
  )

setMethod(
  f="getStudentTDegreesOfFreedom",
  signature="StudentTDistribution",
  definition=function(.Object){
    return(.Object@degreesOfFreedom)
  }
  )

setGeneric(
  name="setStudentTLocation<-",
  def=function(.Object,value){standardGeneric("setStudentTLocation<-")}
  )

setReplaceMethod(
  f="setStudentTLocation",
  signature="StudentTDistribution",
  definition=function(.Object,value){
    .Object@location<-value
    return(.Object)
  }
  )

setGeneric(
  name="setStudentTScale<-",
  def=function(.Object,value){standardGeneric("setStudentTScale<-")}
  )

setReplaceMethod(
  f="setStudentTScale",
  signature="StudentTDistribution",
  definition=function(.Object,value){
    .Object@scale<-value
    return(.Object)
  }
  )

setGeneric(
  name="setStudentTDegreesOfFreedom<-",
  def=function(.Object,value){standardGeneric("setStudentTDegreesOfFreedom<-")}
  )

setReplaceMethod(
  f="setStudentTDegreesOfFreedom",
  signature="StudentTDistribution",
  definition=function(.Object,value){
    .Object@degreesOfFreedom<-value
    return(.Object)
  }
  )

setGeneric(
  name="getStudentTSamples",
  def=function(.Object,number) {standardGeneric("getStudentTSamples")}
  )

setMethod(
  f="getStudentTSamples",
  signature="StudentTDistribution",
  definition=function(.Object, number){
    sample<-rt(number, .Object@degreesOfFreedom)
    temp<-c()
    for(i in 1:length(sample)){
      temp<-c(temp, new(Class="Realisation", Value=as.double(sample[i]), Id=i, Weight=1/number))
    }
    rsample<-new(Class="RandomSample", temp)
    return(rsample)
  }
  )

setGeneric(
  name="getStudentTMean",
  def=function(.Object){standardGeneric("getStudentTMean")}
  )

setMethod(
  f="getStudentTMean",
  signature="StudentTDistribution",
  definition=function(.Object)
  {
    return(1/.Object@rate)  
  }
  )

setGeneric(
  name="getStudentTVariance",
  def=function(.Object){standardGeneric("getStudentTVariance")}
  )

setMethod(
  f="getStudentTVariance",
  signature="StudentTDistribution",
  definition=function(.Object)
  {
    return((1/.Object@rate)^2)  
  }
  )

setGeneric(
  name="getStudentTStandardDeviation",
  def=function(.Object){standardGeneric("getStudentTStandardDeviation")}
  )

setMethod(
  f="getStudentTStandardDeviation",
  signature="StudentTDistribution",
  definition=function(.Object)
  {
    var<-getStudentTVariance(.Object)
    return(sqrt(var))  
  }
  )

setGeneric(
  name="getStudentTMode",
  def=function(.Object){standardGeneric("getStudentTMode")}
  )

setMethod(
  f="getStudentTMode",
  signature="StudentTDistribution",
  definition=function(.Object)
  {
    return(0)  
  }
  )

setGeneric(
  name="getStudentTMedian",
  def=function(.Object){standardGeneric("getStudentTMedian")}
  )

setMethod(
  f="getStudentTMedian",
  signature="StudentTDistribution",
  definition=function(.Object)
  {
    return(log(2)/.Object@rate)  
  }
  )

setGeneric(
  name="getStudentTSkewness",
  def=function(.Object){standardGeneric("getStudentTSkewness")}
  )

setMethod(
  f="getStudentTSkewness",
  signature="StudentTDistribution",
  definition=function(.Object)
  {
    return(2)  
  }
  )

setGeneric(
  name="getStudentTKurtosis",
  def=function(.Object){standardGeneric("getStudentTKurtosis")}
  )

setMethod(
  f="getStudentTKurtosis",
  signature="StudentTDistribution",
  definition=function(.Object)
  {
    return(6)  
  }
  )