setClass(
  Class="PoissonDistribution",
  representation=representation(rate="numeric")
  )

setMethod(
  f="initialize",
  signature="PoissonDistribution",
  definition=function(.Object, rate){
    .Object@rate<-rate
    return(.Object)
  }
  )

setGeneric(
  name="getPoissonRate",
  def=function(.Object) {standardGeneric("getPoissonRate")}
  )

setMethod(
  f="getPoissonRate",
  signature="PoissonDistribution",
  definition=function(.Object){
    return(.Object@rate)
  }
  )

setGeneric(
  name="setPoissonRate<-",
  def=function(.Object,value){standardGeneric("setPoissonRate<-")}
  )

setReplaceMethod(
  f="setPoissonRate",
  signature="PoissonDistribution",
  definition=function(.Object,value){
    .Object@rate<-value
    return(.Object)
  }
  )

setGeneric(
  name="getPoissonSamples",
  def=function(.Object,number) {standardGeneric("getPoissonSamples")}
  )

setMethod(
  f="getPoissonSamples",
  signature="PoissonDistribution",
  definition=function(.Object, number){
    sample<-rpois(number, .Object@rate)
    temp<-c()
    for(i in 1:length(sample)){
      temp<-c(temp, new(Class="Realisation", Value=as.double(sample[i]), Id=i, Weight=1/number))
    }
    rsample<-new(Class="RandomSample", temp)
    return(rsample)
  }
  )

setGeneric(
  name="getPoissonMean",
  def=function(.Object){standardGeneric("getPoissonMean")}
  )

setMethod(
  f="getPoissonMean",
  signature="PoissonDistribution",
  definition=function(.Object)
  {
    return(.Object@rate)  
  }
  )

setGeneric(
  name="getPoissonVariance",
  def=function(.Object){standardGeneric("getPoissonVariance")}
  )

setMethod(
  f="getPoissonVariance",
  signature="PoissonDistribution",
  definition=function(.Object)
  {
    return(.Object@rate)  
  }
  )

setGeneric(
  name="getPoissonStandardDeviation",
  def=function(.Object){standardGeneric("getPoissonStandardDeviation")}
  )

setMethod(
  f="getPoissonStandardDeviation",
  signature="PoissonDistribution",
  definition=function(.Object)
  {
    var<-getPoissonVariance(.Object)
    return(sqrt(var))  
  }
  )

setGeneric(
  name="getPoissonMode",
  def=function(.Object){standardGeneric("getPoissonMode")}
  )

setMethod(
  f="getPoissonMode",
  signature="PoissonDistribution",
  definition=function(.Object)
  {
    return(list(floor(.Object@rate),ceiling(.Object@rate)-1))  
  }
  )

setGeneric(
  name="getPoissonSkewness",
  def=function(.Object){standardGeneric("getPoissonSkewness")}
  )

setMethod(
  f="getPoissonSkewness",
  signature="PoissonDistribution",
  definition=function(.Object)
  {
    return(1/sqrt(.Object@rate))  
  }
  )

setGeneric(
  name="getPoissonKurtosis",
  def=function(.Object){standardGeneric("getPoissonKurtosis")}
  )

setMethod(
  f="getPoissonKurtosis",
  signature="PoissonDistribution",
  definition=function(.Object)
  {
    return(1/.Object@rate)  
  }
  )