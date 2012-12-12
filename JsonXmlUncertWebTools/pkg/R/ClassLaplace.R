setClass(
  Class="LaplaceDistribution",
  representation=representation(location="numeric", scale="numeric")
  )

setMethod(
  f="initialize",
  signature="LaplaceDistribution",
  definition=function(.Object, location, scale){
    .Object@location<-location
    .Object@scale<-scale
    return(.Object)
  }
  )

setGeneric(
  name="getLaplaceLocation",
  def=function(.Object) {standardGeneric("getLaplaceLocation")}
  )

setMethod(
  f="getLaplaceLocation",
  signature="LaplaceDistribution",
  definition=function(.Object){
    return(.Object@location)
  }
  )

setGeneric(
  name="getLaplaceScale",
  def=function(.Object) {standardGeneric("getLaplaceScale")}
  )

setMethod(
  f="getLaplaceScale",
  signature="LaplaceDistribution",
  definition=function(.Object){
    return(.Object@scale)
  }
  )

setGeneric(
  name="setLaplaceLocation<-",
  def=function(.Object,value){standardGeneric("setLaplaceLocation<-")}
  )

setReplaceMethod(
  f="setLaplaceLocation",
  signature="LaplaceDistribution",
  definition=function(.Object,value){
    .Object@location<-value
    return(.Object)
  }
  )

setGeneric(
  name="setLaplaceScale<-",
  def=function(.Object,value){standardGeneric("setLaplaceScale<-")}
  )

setReplaceMethod(
  f="setLaplaceScale",
  signature="LaplaceDistribution",
  definition=function(.Object,value){
    .Object@scale<-value
    return(.Object)
  }
  )

setGeneric(
  name="getLaplaceSamples",
  def=function(.Object,number) {standardGeneric("getLaplaceSamples")}
  )

setMethod(
  f="getLaplaceSamples",
  signature="LaplaceDistribution",
  definition=function(.Object, number){
    sample<-runif(number, -1/2, 1/2)
    temp<-c()
    for(i in 1:length(sample)){
      s<-.Object@location-(.Object@scale*sign(sample[i])*log(1-2*abs(sample[i])))
      temp<-c(temp, new(Class="Realisation", Value=as.double(s), Id=i, Weight=1/number))
    }
    rsample<-new(Class="RandomSample", temp)
    return(rsample)
  }
  )

setGeneric(
  name="getLaplaceMean",
  def=function(.Object){standardGeneric("getLaplaceMean")}
  )

setMethod(
  f="getLaplaceMean",
  signature="LaplaceDistribution",
  definition=function(.Object)
  {
    return(.Object@location)  
  }
  )

setGeneric(
  name="getLaplaceVariance",
  def=function(.Object){standardGeneric("getLaplaceVariance")}
  )

setMethod(
  f="getLaplaceVariance",
  signature="LaplaceDistribution",
  definition=function(.Object)
  {
    return(2*(.Object@scale^2))  
  }
  )

setGeneric(
  name="getLaplaceStandardDeviation",
  def=function(.Object){standardGeneric("getLaplaceStandardDeviation")}
  )

setMethod(
  f="getLaplaceStandardDeviation",
  signature="LaplaceDistribution",
  definition=function(.Object)
  {
    var<-getLaplaceVariance(.Object)
    return(sqrt(var))  
  }
  )

setGeneric(
  name="getLaplaceMode",
  def=function(.Object){standardGeneric("getLaplaceMode")}
  )

setMethod(
  f="getLaplaceMode",
  signature="LaplaceDistribution",
  definition=function(.Object)
  {
    return(.Object@location)  
  }
  )

setGeneric(
  name="getLaplaceMedian",
  def=function(.Object){standardGeneric("getLaplaceMedian")}
  )

setMethod(
  f="getLaplaceMedian",
  signature="LaplaceDistribution",
  definition=function(.Object)
  {
    return(.Object@location)  
  }
  )

setGeneric(
  name="getLaplaceSkewness",
  def=function(.Object){standardGeneric("getLaplaceSkewness")}
  )

setMethod(
  f="getLaplaceSkewness",
  signature="LaplaceDistribution",
  definition=function(.Object)
  {
    return(0)  
  }
  )

setGeneric(
  name="getLaplaceKurtosis",
  def=function(.Object){standardGeneric("getLaplaceKurtosis")}
  )

setMethod(
  f="getLaplaceKurtosis",
  signature="LaplaceDistribution",
  definition=function(.Object)
  {
    return(3)  
  }
  )